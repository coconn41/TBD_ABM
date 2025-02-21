transfer_outcomes_ha = c("None","ha")
transfer_outcomes_v1 = c("None","v1")
transfer_pathogens = function(tick_agents, deer_agents, mouse_agents){
    tick_agents <- tick_agents %>%
      mutate(transfer_type = case_when(links>0 & 
                                         time_on_host >=24 &
                                         ((sex == "female" & Lifestage == "Adult") |
                                            (Lifestage == "Larvae") |
                                            (Lifestage == "Nymph")) &
                                         attempted_pathogen_transfer == 0 &
                                         linked_type == "Deer" &
                                         Infection_status == "v1" &
                                         deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "t2dv1",
                                       links>0 & 
                                         ((sex == "female" & Lifestage == "Adult") |
                                            (Lifestage == "Larvae") |
                                            (Lifestage == "Nymph")) &
                                         attempted_pathogen_transfer == 0 &
                                         linked_type == "Deer" &
                                         Infection_status == "None" &
                                         deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==1 ~ "d2tv1",
                                       # links>0 & 
                                       #   time_on_host >=24 &
                                       #   sex == "female" &
                                       #   Lifestage != "Eggs" &
                                       #   attempted_pathogen_transfer == 0 &
                                       #   linked_type == "Deer" &
                                       #   Infection_status == "None" &
                                       #   deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "None",
                                       links>0 & 
                                         time_on_host >=24 &
                                         ((sex == "female" & Lifestage == "Adult") |
                                            (Lifestage == "Larvae") |
                                            (Lifestage == "Nymph")) &
                                         attempted_pathogen_transfer == 0 &
                                         linked_type == "Mouse" &
                                         Infection_status == "ha" &
                                         mouse_agents[match(links,mouse_agents$Agent_ID),]$Ha_infected==0 ~ "t2mha",
                                       links>0 & 
                                         ((sex == "female" & Lifestage == "Adult") |
                                            (Lifestage == "Larvae") |
                                            (Lifestage == "Nymph")) &
                                         attempted_pathogen_transfer == 0 &
                                         linked_type == "Mouse" &
                                         Infection_status == "None" &
                                         mouse_agents[match(links,mouse_agents$Agent_ID),]$Ha_infected==1 ~ "m2tha",
                                       TRUE ~ "None")) %>%
      mutate(Infection_status = case_when(transfer_type == "d2tv1" & Lifestage != "Larvae" ~ transfer_outcomes_v1[rbinom(n = n(), size = 1, prob = deer_infect_tick_v1)+1],
                                          transfer_type == "m2tha" & Lifestage != "Larvae" ~ transfer_outcomes_ha[rbinom(n = n(), size = 1, prob = mouse_infect_tick_ha)+1],
                                          transfer_type == "d2tv1" & Lifestage == "Larvae" ~ "v1",
                                          transfer_type == "m2tha" & Lifestage == "Larvae" ~ "ha",
                                          transfer_type == "None" ~ Infection_status,
                                          TRUE ~ Infection_status))
    
    d_matches1 <- deer_agents %>%
      filter(map_lgl(tick_links, ~ identical(.,0)) | V1_infected == 1)
    
    t2dv1_ids <- tick_agents$Agent_ID[tick_agents$transfer_type == "t2dv1"]
    
    deer_agents <- deer_agents %>%
      filter(map_lgl(tick_links, ~ !identical(.,0)) & V1_infected == 0)
    
    deer_agents$tick_links <- map(deer_agents$tick_links, ~ .x[!is.na(.x)])
    deer_agents$transfer_count <- vapply(deer_agents$tick_links, function(x) sum(x %in% t2dv1_ids), integer(1))
    new_infections <- rbinom(n = nrow(deer_agents), size = deer_agents$transfer_count, prob = tick_infect_deer_v1) > 0
    deer_agents$V1_infected <- ifelse(deer_agents$V1_infected == 0 & new_infections, 1, deer_agents$V1_infected)
    deer_agents = subset(deer_agents, select = -c(transfer_count))
    deer_agents <- deer_agents %>%
      rbind(.,d_matches1)
    
    m_matches1 <- mouse_agents %>%
      filter(map_lgl(tick_links, ~ identical(.,0)) | Ha_infected == 1)
    
    t2mha_ids <- tick_agents$Agent_ID[tick_agents$transfer_type == "t2mha"]
    
    mouse_agents <- mouse_agents %>%
      filter(map_lgl(tick_links, ~ !identical(.,0)) & Ha_infected == 0)
    
    mouse_agents$tick_links <- map(mouse_agents$tick_links, ~ .x[!is.na(.x)])
    mouse_agents$transfer_count <- vapply(mouse_agents$tick_links, function(x) sum(x %in% t2mha_ids), integer(1))
    new_infections <- rbinom(n = nrow(mouse_agents), size = mouse_agents$transfer_count, prob = tick_infect_mouse_ha) > 0
    mouse_agents$Ha_infected <- ifelse(mouse_agents$Ha_infected == 0 & new_infections, 1, mouse_agents$Ha_infected)
    mouse_agents = subset(mouse_agents, select = -c(transfer_count))
    mouse_agents <- mouse_agents %>%
      rbind(.,m_matches1)
    
    tick_agents <- tick_agents %>%
      mutate(attempted_pathogen_transfer = ifelse(links>0 & 
                                                    Infection_status != "None" &
                                                    transfer_type != "None" & 
                                                    attempted_pathogen_transfer == 0,1,attempted_pathogen_transfer))
}
