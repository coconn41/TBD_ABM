testing_data = data.frame(LS = c("Adult","Adult","Nymph","Nymph"),
                          Variant = c("Ha","V1","Ha","V1"),
                          count = c(3452,987,691,786))
L_prob = testing_data[3,3]/(testing_data[3,3]+testing_data[4,3])
N_prob = testing_data[1,3]/(testing_data[1,3]+testing_data[2,3])
# Using a simple probability to give probability of comparative attachment
# paste0("Nymphs have a ",round(testing_data[1,3]/(testing_data[1,3]+testing_data[2,3]),3),
#        " probability of feeding on mice and a ",
#        1-round(testing_data[1,3]/(testing_data[1,3]+testing_data[2,3]),3),
#        " probability of feeding on deer")
# paste0("Larvae have a ",round(testing_data[3,3]/(testing_data[3,3]+testing_data[4,3]),3),
#        " probability of feeding on mice and a ",
#        1-round(testing_data[3,3]/(testing_data[3,3]+testing_data[4,3]),3),
#        " probability of feeding on deer")

attach_ticks = function(tick_agents,deer_paths,mouse_agents,other_agents,
                        LA_probability,NA_probability,AA_probability){
  T_matches1 <- tick_agents %>%
    mutate(deer_links = ifelse(paste0(row,",",
                                      col,",",
                                      network_ID) %in% paste0(deer_paths$row,",",
                                                              deer_paths$col,",",
                                                              deer_paths$network_ID)==T,
                               match(paste0(.$row,",",
                                            .$col,",",
                                            .$network_ID),
                                     deer_paths$locs,0),0)) %>%
    filter(deer_links > 0) %>%
    mutate(deer_links = map(paste0(.$row,",",
                                   .$col,",",
                                   .$network_ID),function(element) {
                                     mvar2 = deer_paths$Agent_ID[deer_paths$locs %in% element]
                                   })) %>%
    mutate(num_pot_deer_links = lengths(deer_links))
  for(i in 1:nrow(T_matches1)){
    if(T_matches1[i,]$Lifestage=="Larvae"){p = LA_probability}
    if(T_matches1[i,]$Lifestage=="Nymph"){p = NA_probability}
    if(T_matches1[i,]$Lifestage=="Adult"){p = AA_probability}
    x = rbinom(n = T_matches1[i,]$num_pot_deer_links,size=1,p)
    if(sum(x)==0){T_matches1[i,]$deer_links=0}
    if(sum(x)!=0){T_matches1[i,]$deer_links=sample(x=subset(unlist(T_matches1[i,]$deer_links)*x,
                    (unlist(T_matches1[i,]$deer_links)*x)>0),
           size = 1)}
  }
  T_matches1$deer_links = unlist(T_matches1$deer_links)
  T_matches2 = tick_agents %>%
    mutate(deer_links = ifelse(paste0(row,",",
                                      col,",",
                                      network_ID) %in% paste0(deer_paths$row,",",
                                                              deer_paths$col,",",
                                                              deer_paths$network_ID)==T,
                               match(paste0(.$row,",",
                                            .$col,",",
                                            .$network_ID),
                                     deer_paths$locs,0),0)) %>%
    filter(deer_links == 0) %>%
    bind_rows(.,T_matches1) 
  
  T_matches3 = T_matches2 %>%
    mutate(mouse_links = match(paste0(.$row,",",
                                    .$col,",",
                                    .$network_ID),
                             paste0(mouse_agents$row,",",
                                    mouse_agents$col,",",
                                    mouse_agents$network_ID),0)) %>%
    mutate(mouse_links = map(paste0(.$row,",",
                                    .$col,",",
                                    .$network_ID),function(element) {
                                      mvar2 = mouse_agents$Agent_ID[mouse_agents$locs %in% element]
                                    })) %>%
    mutate(num_pot_mouse_links = lengths(.$mouse_links)) %>%
    filter(num_pot_mouse_links > 0)
  
  for(i in 1:nrow(T_matches3)){
    if(T_matches3[i,]$Lifestage=="Larvae"){p = LA_probability}
    if(T_matches3[i,]$Lifestage=="Nymph"){p = NA_probability}
    if(T_matches3[i,]$Lifestage=="Adult"){p = AA_probability}
    x = rbinom(n = T_matches3[i,]$num_pot_mouse_links,size=1,p)
    if(sum(x)==0){T_matches3[i,]$mouse_links=0}
    if(sum(x)!=0){T_matches3[i,]$mouse_links = sample(x=subset(unlist(T_matches3[i,]$mouse_links)*x,
                    (unlist(T_matches3[i,]$mouse_links)*x)>0),
           size = 1)}}
  T_matches3$mouse_links = unlist(T_matches3$mouse_links)
  T_matches4 = T_matches2 %>%
    mutate(mouse_links = match(paste0(.$row,",",
                                      .$col,",",
                                      .$network_ID),
                               paste0(mouse_agents$row,",",
                                      mouse_agents$col,",",
                                      mouse_agents$network_ID),0)) %>%
    mutate(mouse_links = map(paste0(.$row,",",
                                    .$col,",",
                                    .$network_ID),function(element) {
                                      mvar2 = mouse_agents$Agent_ID[mouse_agents$locs %in% element]
                                    })) %>%
    mutate(num_pot_mouse_links = lengths(.$mouse_links)) %>%
    filter(num_pot_mouse_links == 0) %>%
    mutate(mouse_links = 0) %>%
    bind_rows(.,T_matches3)
  tick_agents <<- T_matches4 %>%
    mutate(selection = ifelse(deer_links>0&mouse_links>0&Lifestage=="Nymph",
                          rbinom(n=1,size = 1,prob = N_prob),
                          ifelse(deer_links>0&mouse_links>0&Lifestage=="Larvae",
                                 rbinom(n=1,size=1,prob = L_prob),
                                 ifelse(deer_links>0&mouse_links&Lifestage=="Adult",
                                        1,-1))),
           links = ifelse(selection==1,mouse_links,
                          ifelse(selection==0,deer_links,0))) %>%
    dplyr::select(-c(num_pot_deer_links,num_pot_mouse_links,deer_links,mouse_links,selection)) 
}
# start_time = Sys.time()
# attach_ticks(tick_agents,deer_paths,mouse_agents,LA_probability,NA_probability,AA_probability)
# end_time = Sys.time()
# end_time-start_time