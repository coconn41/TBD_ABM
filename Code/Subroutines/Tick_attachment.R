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
  
  # This includes only ticks that are not linked to a host
  # and also have not already succeeded in a bloodmeal
  T_matches0 <- tick_agents %>% 
    filter(links==0&dropped==0)
  
  # This includes only ticks that are not linked to a host
  # and also have not already succeeded in a bloodmeal and
  # checks to see if deer have crossed their path
  T_matches1 <- T_matches0 %>%
    mutate(deer_links = ifelse(paste0(row,",",
                                      col,",",
                                      network_ID) %in% paste0(deer_paths$row,",",
                                                              deer_paths$col,",",
                                                              deer_paths$network_ID)==T,
                               match(paste0(.$row,",",
                                            .$col,",",
                                            .$network_ID),
                                     deer_paths$locs,0),0))
  
  # This includes only ticks that are not linked to a host
  # and also have not already succeeded in a bloodmeal and
  # also includes only newly attached ticks, it then assigns them
  # the agent ID of the deer that picked them up
  T_matches2 <- T_matches1 %>%
    filter(deer_links > 0) %>%
    mutate(deer_links = deer_paths$Agent_ID[.$deer_links])#%>% # Below is miscoded
    # mutate(deer_links = ifelse(Lifestage == "Larvae",ifelse(rbinom(n=1,
    #                                                                size=1,
    #                                                                prob=LA_probability)==1,
    #                                                         deer_links,0),
    #                            ifelse(Lifestage == "Nymph",ifelse(rbinom(n=1,
    #                                                                      size=1,
    #                                                                      prob=NA_probability)==1,
    #                                                               deer_links,0),
    #                                   ifelse(Lifestage == "Adult",ifelse(rbinom(n=1,
    #                                                                             size=1,
    #                                                                             prob=AA_probability)==1,
    #                                                                      deer_links,0),0)))) 


  # This includes only ticks that are not linked to a host
  # and also have not already succeeded in a bloodmeal and
  # includes ticks that were not newly picked up by deer. It
  # then recombines them with the ones that were newly picked up
  T_matches3 = T_matches1 %>%
    filter(deer_links == 0) %>%
    bind_rows(.,T_matches2) 
  
  
  T_matches4 = T_matches3 %>%
    mutate(mouse_links = match(paste0(.$row,",",
                                    .$col,",",
                                    .$network_ID),
                             paste0(mouse_agents$row,",",
                                    mouse_agents$col,",",
                                    mouse_agents$network_ID),0)) 
  
  T_matches5 = T_matches4 %>%
    filter(mouse_links > 0) %>%
    mutate(mouse_links = mouse_agents$Agent_ID[.$mouse_links])#%>% # Below is miscoded
    # mutate(mouse_links = ifelse(Lifestage == "Larvae",ifelse(rbinom(n=1,
    #                                                                size=1,
    #                                                                prob=LA_probability)==1,
    #                                                         mouse_links,0),
    #                            ifelse(Lifestage == "Nymph",ifelse(rbinom(n=1,
    #                                                                      size=1,
    #                                                                      prob=NA_probability)==1,
    #                                                               mouse_links,0),
    #                                   ifelse(Lifestage == "Adult",ifelse(rbinom(n=1,
    #                                                                             size=1,
    #                                                                             prob=AA_probability)==1,
    #                                                                      mouse_links,0),0)))) 
  T_matches6 = T_matches4 %>%
    filter(mouse_links == 0) %>%
    bind_rows(.,T_matches5)
  
  tick_agents <<- T_matches6 %>%
    mutate(selection = ifelse(deer_links>0&mouse_links>0&Lifestage=="Nymph",
                          rbinom(n=1,size = 1,prob = N_prob),
                          ifelse(deer_links>0&mouse_links>0&Lifestage=="Larvae",
                                 rbinom(n=1,size=1,prob = L_prob),
                                 ifelse(deer_links>0&mouse_links>0&Lifestage=="Adult",
                                        0,-1))),
           linked_type = ifelse(selection==1,"Mouse",
                                ifelse(selection==0,"Deer","N")),
           links = ifelse(selection==1,mouse_links,
                          ifelse(selection==0,deer_links,0))) %>%
    dplyr::select(-c(deer_links,mouse_links,selection)) %>%
    rbind(.,tick_agents %>% filter(links>0|dropped>0))  # This combines back with already linked ticks

}