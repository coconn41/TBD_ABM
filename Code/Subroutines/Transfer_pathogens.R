transfer_pathogens = function(tick_agents, deer_agents, mouse_agents){
  transfer_outcomes_ha = c("None","ha")
  transfer_outcomes_v1 = c("None","v1")
  tick_agents <<- tick_agents %>%
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
                                        transfer_type == "None" ~ "None",
                                        TRUE ~ "None"))
  
  dmatches1 <- deer_agents %>%
    filter(tick_links == 0)
  
  deer_agents <<- deer_agents %>%
    filter(tick_links>1) %>%
     mutate(#Ha_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2dha",
    #                             rbinom(n = 1, size = 1, prob = tick_infect_deer_ha),
    #                             0),
           V1_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2dv1",
                                rbinom(n = 1, size = 1, prob = tick_infect_deer_v1),
                                0)) %>%
    bind_rows(.,dmatches1)

  m_matches1 <- mouse_agents %>%
    filter(tick_links==0)
  
  mouse_agents <<- mouse_agents %>%
    filter(tick_links>1) %>%
    mutate(Ha_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2mha",
                                rbinom(n = 1, size = 1, prob = tick_infect_mouse_ha),
                                0),
           # V1_infected = ifelse(tick_agents[which(tick_agents$Agent_ID==tick_links),]$transfer_type == "t2mv1",
           #                      rbinom(n = 1, size = 1, prob = tick_infect_deer_v1),0) 
 ) %>%
    bind_rows(.,m_matches1)
  
  tick_agents <<- tick_agents %>%
    mutate(transfer_type = "None",
           attempted_pathogen_transfer = ifelse(links>0 & Infection_status != "None",1,0))
}
