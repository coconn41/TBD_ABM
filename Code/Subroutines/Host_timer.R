host_timer_fn = function(deer_agents, mouse_agents){
  mouse_agents <<- mouse_agents %>%
    mutate(Age = Age+1,
           Ha_infection_timer = case_when(Ha_infected == 1 & Ha_infection_timer==0 ~ 1,
                                          Ha_infected == 1 & Ha_infection_timer>0 ~ Ha_infection_timer + 1,
                                          TRUE ~ 0))
           # Ha_infection_timer = ifelse(Ha_infected==1 & Ha_infection_timer==0,1,
           #                             ifelse(Ha_infected==1 & Ha_infection_timer>0,
           #                                    Ha_infection_timer + 1,0)),
           # V1_infection_timer = ifelse(V1_infected==1 & V1_infection_timer==0,1,
           #                             ifelse(V1_infected==1 & V1_infection_timer>0,
           #                                    V1_infection_timer+1,0)),
           # groom_timer = case_when(groom_timer > 1 ~ 0,
           #                         is.na(tick_links) == F ~ groom_timer + mouse_GR,
           #                         is.na(tick_links) == T ~ 0,
           #                         TRUE ~ groom_timer))
  deer_agents <<- deer_agents %>%
    mutate(Age = Age+1,
           # Ha_infection_timer = ifelse(Ha_infected==1 & Ha_infection_timer==0,1,
           #                             ifelse(Ha_infected==1 & Ha_infection_timer>0,
           #                                    Ha_infection_timer + 1,0)),
           V1_infection_timer = case_when(V1_infected==1 & V1_infection_timer==0 ~ 1,
                                          V1_infected==1 & V1_infection_timer>0 ~ V1_infection_timer+1,
                                          TRUE ~ 0))
           # V1_infection_timer = ifelse(V1_infected==1 & V1_infection_timer==0,1,
           #                             ifelse(V1_infected==1 & V1_infection_timer>0,
           #                                    V1_infection_timer+1,0)),
           # groom_timer = case_when(groom_timer > 1 ~ 0,
           #                         is.na(tick_links) == F ~ groom_timer + deer_GR,
           #                         is.na(tick_links) == T ~ 0,
           #                         TRUE ~ groom_timer))
 }
 