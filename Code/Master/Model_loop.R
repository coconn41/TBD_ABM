if(net_select!="all"){
deer_agents = deer_agents %>% 
  filter(network_ID == net_select)
mouse_agents = mouse_agents %>%
  filter(network_ID == net_select)
tick_agents = tick_agents %>%
  filter(network_ID == net_select)
nymph_agents = nymph_agents %>%
  filter(network_ID == net_select)
jump_probability_df = jump_probability_df %>%
  filter(network_ID == net_select)
aspatial_network = aspatial_network %>%
  filter(network_ID == net_select)
network1 = network1 %>%
  filter(network_ID == net_select)
reduced_patches = reduced_patches %>%
  filter(network_ID == net_select)
spat_network = spat_network %>%
  filter(network_ID == net_select)}


#pb = txtProgressBar(min = 1, max = go_timesteps, initial = 1) 
# start_time = Sys.time()
for(i in 1:go_timesteps){
  # Update environment
  
  update_enviro(i,daylight)
  
  # Add nymphs in during summer of Year 1
  if(year==1&day==171){
    tick_agents = rbind(tick_agents,nymph_agents)
    remove(nymph_agents)
  }
  
  # Move mice
  if(daytime=="day"){mouse_agents <- mouse_agents %>%
    mutate(movement = case_when(row==gridrows & !col%in%c(1,gridcols) ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
                                row==1 & !col%in%c(1,gridcols) ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
                                col==gridcols & !row%in%c(1,gridrows) ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
                                col==1 & !row%in%c(1,gridrows) ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
                                row==gridrows & col==gridcols ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
                                row==gridrows & col==1 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
                                row==1 & col==gridcols ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
                                row==1 & col==1 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
                                TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
           # movement = case_when(possibilities==2 ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
           #                      possibilities==3 ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
           #                      possibilities==4 ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
           #                      possibilities==5 ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
           #                      possibilities==6 ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
           #                      possibilities==7 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
           #                      possibilities==8 ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
           #                      possibilities==9 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
           #                      TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
           row = case_when(movement==1 ~ row-1,
                           movement==2 ~ row-1,
                           movement==3 ~ row-1,
                           movement==7 ~ row+1,
                           movement==8 ~ row+1,
                           movement==9 ~ row+1,
                           TRUE ~ row),
           col = case_when(movement==1 ~ col-1,
                           movement==3 ~ col+1,
                           movement==4 ~ col-1,
                           movement==6 ~ col+1,
                           movement==7 ~ col-1,
                           movement==9 ~ col+1,
                           TRUE ~ col),
           locs = paste0(row,",",col,",",network_ID)) %>%
    mutate(prob = runif(n = nrow(.),min = 0,max=1)) %>%
    arrange(desc(prob))}
  
  # Move others
  #other_movement(other_agents,daytime)
  
  # Move deer
  if(daytime=="day"){deer_agents <- deer_agents %>%
    mutate(possibility_row_min = row-100,
           possibility_row_max = row+100,
           possibility_col_min = col-100,
           possiblitiy_col_max = col+100) %>%
    group_by(Agent_ID) %>%
    mutate(new_row = sample(possibility_row_min:possibility_row_max,size = 1),
           new_col = sample(possibility_col_min:possiblitiy_col_max,size = 1),
           new_patch = layer) %>%
    mutate(jump_patch = ifelse(new_row<=0|new_col<=0,1,0)) %>%
    mutate(new_patch = ifelse(jump_patch == 1,
                              sample(subset(jump_probability_df,
                                            origin_ID==layer&
                                              network_ID==network_ID)$destination_ID,
                                     size = 1,
                                     prob = subset(jump_probability_df,
                                                   origin_ID==layer,
                                                   network_ID==network_ID)$probability),
                              layer)) %>%
    mutate(new_row = ifelse(new_patch!=layer,round(runif(n=1,
                                                         min = 0,
                                                         max = subset(reduced_patches %>% st_drop_geometry(),
                                                                      layer==layer)$gridrows)),
                            new_row),
           new_col = ifelse(new_patch!=layer,round(runif(n=1,
                                                         min = 0,
                                                         max = subset(reduced_patches %>% st_drop_geometry(),
                                                                      layer==layer)$gridcols)),
                            new_col)) %>%
    mutate(old_row = row,
           old_col = col,
           row = new_row,
           col = new_col,
           layer = new_patch,
           locs = paste0(row,",",col,",",network_ID),
           jump_patch = 0) }
if(season!="winter"){  
  # Create deer paths
  if(daytime=="day"){deer_paths <- deer_agents %>%
    filter(jump_patch==0) %>%
    select(Agent_ID,network_ID,layer,old_row,old_col,new_row,new_col) %>%
    rowwise() %>%
    mutate(cells = list(bresenham_line(old_col, old_row, new_col, new_row))) %>%
    ungroup() %>%
    unnest(cells) %>%
    mutate(row = cells[,1],
           col = cells[,2]) %>%
    select(Agent_ID,network_ID,layer,row,col) %>%
    distinct() %>%
    group_by(network_ID,layer,row,col) %>%
    mutate(locs = paste0(row,",",
                         col,",",
                         layer,",",
                         network_ID)) %>%
    ungroup() %>%
    group_by(Agent_ID) %>%
    mutate(prob = runif(min=0,max=1,n=1)) %>%
    arrange(prob)}
  
  # Attach ticks
  if(daytime=="day"){T_matches1 <- tick_agents %>% 
    filter(links==0&dropped==0)
  
  # This includes only ticks that are not linked to a host
  # and also have not already succeeded in a bloodmeal and
  # checks to see if deer have crossed their path
  
  T_matches2 <- T_matches1 %>%
    mutate(deer_links = match(paste0(.$row,",",
                                     .$col,",",
                                     .$layer,",",
                                     .$network_ID),
                              deer_paths$locs,NA))
  
  # This includes only ticks that are not linked to a host
  # and also have not already succeeded in a bloodmeal and
  # also includes only newly attached ticks, it then assigns them
  # the agent ID of the deer that picked them up  
  T_matches3 <- T_matches2 %>%
    mutate(deer_links = deer_paths[deer_links,]$Agent_ID)
  
  
  T_matches4 = T_matches3 %>%
    mutate(mouse_links = match(paste0(.$row,",",
                                      .$col,",",
                                      .$layer,",",
                                      .$network_ID),
                               paste0(mouse_agents$row,",",
                                      mouse_agents$col,",",
                                      mouse_agents$layer,",",
                                      mouse_agents$network_ID),NA)) 
  
  T_matches5 = T_matches4 %>%
    mutate(mouse_links = mouse_agents[.$mouse_links,]$Agent_ID)#%>% # Below is miscoded
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
  
  tick_agents <- T_matches5 %>%
    mutate(selection = ifelse(deer_links>0&mouse_links>0&Lifestage=="Nymph",
                              rbinom(n=1,size = 1,prob = N_prob),
                              ifelse(deer_links>0&mouse_links>0&Lifestage=="Larvae",
                                     rbinom(n=1,size=1,prob = L_prob),
                                     ifelse(deer_links>0&mouse_links>0&Lifestage=="Adult",
                                            0,-1))),
           linked_type = ifelse(selection==1,"Mouse",
                                ifelse(selection==0,"Deer","N")),
           links = ifelse(selection==1,mouse_links,
                          ifelse(selection==0,deer_links,NA))) %>%
    dplyr::select(-c(deer_links,mouse_links,selection)) %>%
    rbind(.,tick_agents %>% filter(links>0|dropped>0))  # This combines back with already linked ticks
  
  non_links = tick_agents %>% 
    filter(is.na(links)==F)
  
  deer_agents <- deer_agents %>%
    mutate(tick_links = non_links[match(Agent_ID,non_links$links),]$Agent_ID) %>%
    mutate(tick_links = ifelse(is.na(tick_links)==T,0,tick_links))
  
  mouse_agents <- mouse_agents %>%
    mutate(tick_links = non_links[match(Agent_ID,non_links$mouse_links),]$Agent_ID) %>%
    mutate(tick_links = ifelse(is.na(tick_links)==T,0,tick_links))
  
  tick_agents <- tick_agents %>%
    mutate(links = ifelse(is.na(links)==T,0,links))}
  }
  #####
  # Transfer pathogens
  #####
  tick_agents <- tick_agents %>%
    mutate(transfer_type = case_when(links>0 & 
                                       attempted_pathogen_transfer == 0 &
                                       linked_type == "Deer" &
                                       Infection_status == "v1" &
                                       deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "t2dv1",
                                     links>0 & 
                                       attempted_pathogen_transfer == 0 &
                                       linked_type == "Deer" &
                                       Infection_status == "None" &
                                       deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==1 ~ "d2tv1",
                                     links>0 & 
                                       attempted_pathogen_transfer == 0 &
                                       linked_type == "Deer" &
                                       Infection_status == "None" &
                                       deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "None",
                                     links>0 & 
                                       attempted_pathogen_transfer == 0 &
                                       linked_type == "Mouse" &
                                       Infection_status == "ha" &
                                       deer_agents[match(links,deer_agents$Agent_ID),]$Ha_infected==0 ~ "t2mha",
                                     links>0 & 
                                       attempted_pathogen_transfer == 0 &
                                       linked_type == "Mouse" &
                                       Infection_status == "None" &
                                       deer_agents[match(links,deer_agents$Agent_ID),]$Ha_infected==1 ~ "m2tha",
                                     TRUE ~ "None")) %>%
    # mutate(transfer_type = case_when(links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Deer" &
    #                                    Infection_status == "ha" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$Ha_infected==0 ~ "t2dha",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Deer" &
    #                                    Infection_status == "v1" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "t2dv1",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Deer" &
    #                                    Infection_status == "None" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$Ha_infected==1 ~ "d2tha",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Deer" &
    #                                    Infection_status == "None" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==1 ~ "d2tv1",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Deer" &
    #                                    Infection_status == "None" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "None",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Mouse" &
    #                                    Infection_status == "ha" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$Ha_infected==0 ~ "t2mha",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Mouse" &
    #                                    Infection_status == "v1" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "t2mv1",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Mouse" &
    #                                    Infection_status == "None" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$Ha_infected==1 ~ "m2tha",
    #                                  links>0 & 
    #                                    attempted_pathogen_transfer == 0 &
    #                                    linked_type == "Mouse" &
    #                                    Infection_status == "None" &
    #                                    deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==1 ~ "m2tv1",
    #                                  TRUE ~ "None")) %>%
    # mutate(Infection_status = case_when(transfer_type == "d2tha" ~ transfer_outcomes_ha[rbinom(n = 1, size = 1, prob = deer_infect_tick_ha)+1],
    #                                     transfer_type == "d2tv1" ~ transfer_outcomes_v1[rbinom(n = 1, size = 1, prob = deer_infect_tick_v1)+1],
    #                                     transfer_type == "m2tha" ~ transfer_outcomes_ha[rbinom(n = 1, size = 1, prob = mouse_infect_tick_ha)+1],
    #                                     transfer_type == "m2tv1" ~ transfer_outcomes_v1[rbinom(n = 1, size = 1, prob = mouse_infect_tick_v1)+1],
    #                                     transfer_type == "None" ~ "None",
    #                                     TRUE ~ "None"))
    mutate(Infection_status = case_when(transfer_type == "d2tv1" ~ transfer_outcomes_v1[rbinom(n = 1, size = 1, prob = deer_infect_tick_v1)+1],
                                        transfer_type == "m2tha" ~ transfer_outcomes_ha[rbinom(n = 1, size = 1, prob = mouse_infect_tick_ha)+1],
                                        transfer_type == "None" ~ Infection_status,
                                        TRUE ~ "None"))
  
  dmatches1 <- deer_agents %>%
    filter(tick_links == 0 | V1_infected == 1)
  
  deer_agents <- deer_agents %>%
    filter(tick_links>1 & V1_infected == 0) %>%
    mutate(#Ha_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2dha",
      #                             rbinom(n = 1, size = 1, prob = tick_infect_deer_ha),
      #                             0),
      V1_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2dv1",
                           rbinom(n = 1, size = 1, prob = tick_infect_deer_v1),
                           0)) %>%
    bind_rows(.,dmatches1)
  
  m_matches1 <- mouse_agents %>%
    filter(tick_links==0 | Ha_infected == 1)
  
  mouse_agents <- mouse_agents %>%
    filter(tick_links>1 & Ha_infected == 0) %>%
    mutate(Ha_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2mha",
                                rbinom(n = 1, size = 1, prob = tick_infect_mouse_ha),
                                0),
           # V1_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2mv1",
           #                      rbinom(n = 1, size = 1, prob = tick_infect_deer_v1),0) 
    ) %>%
    bind_rows(.,m_matches1)
  
  tick_agents <- tick_agents %>%
    mutate(transfer_type = "None",
           attempted_pathogen_transfer = ifelse(links>0 & Infection_status != "None",1,0))
#####  
  # Groom ticks
  if(daytime=="day"){tick_agents <- tick_agents %>%
    mutate(die = ifelse(time_on_host > 0 & linked_type == "Deer",
                        rbinom(n=1,size=1,prob = deer_GR),
                        ifelse(time_on_host > 0 & linked_type == "Mouse",
                               rbinom(n=1,size=1,prob = mouse_GR),0))) %>% # deer_GR mouse_GR
    mutate(die = ifelse(die==1,ifelse(rbinom(n=1,size=1,prob = Groom_survival)==1,
                                      0,1),0)) # Groom survival function
  
  groom_list <- subset(tick_agents,tick_agents$die==1)$Agent_ID
  
  deer_agents <- deer_agents %>% 
    mutate(tick_links = ifelse(tick_links %in% groom_list,0,tick_links))
  
  mouse_agents <- mouse_agents %>%
    mutate(tick_links = ifelse(tick_links %in% groom_list,0,tick_links))
  
  tick_agents <- tick_agents %>%
    filter(die==0)}
  
  # Mate ticks
  if(daytime=="day"){mating_fn(tick_agents = tick_agents)}
  
  # Tick timer
  tick_timer(tick_agents = tick_agents)
  
  # Tick drop off
  tick_drop_fn(tick_agents = tick_agents)
  
  # Lay eggs
  lay_eggs(tick_agents = tick_agents)
  
  # Tick molting
  tick_molting(tick_agents = tick_agents)
  
  # Tick death
  tick_death(tick_agents = tick_agents)
  
  # Host timer
  host_timer_fn(deer_agents = deer_agents,
                mouse_agents = mouse_agents)
  
  # "Kill" hosts
  kill_hosts_fn(deer_agents = deer_agents,
                mouse_agents = mouse_agents)
  
  
  # Compile results
  track_data(i = i,
             tick_agents = tick_agents,
             deer_agents = deer_agents,
             mouse_agents = mouse_agents)
  
  if(i%%100==0){print(paste0("timestep ", i, ",day ",day," of year ", year))}
  #setTxtProgressBar(pb,i)
}
# end_time = Sys.time()
# end_time-start_time
#9.043 minutes to do network 3
  
  # Update tick processes (Lay eggs, molt, die)
  
  # Track population data
  
  

  
  
  