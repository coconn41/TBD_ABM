host_timer_fn = function(deer_agents, mouse_agents){
  mouse_agents <<- mouse_agents %>%
    mutate(Age = Age+1,
           Ha_infection_timer = ifelse(Ha_infected==1 & Ha_infection_timer==0,1,
                                       ifelse(Ha_infected==1 & Ha_infection_timer>0,
                                              Ha_infection_timer + 1,0)),
           V1_infection_timer = ifelse(V1_infected==1 & V1_infection_timer==0,1,
                                       ifelse(V1_infected==1 & V1_infection_timer>0,
                                              V1_infection_timer+1,0)))
  deer_agents <<- deer_agents %>%
    mutate(Age = Age+1,
           Ha_infection_timer = ifelse(Ha_infected==1 & Ha_infection_timer==0,1,
                                       ifelse(Ha_infected==1 & Ha_infection_timer>0,
                                              Ha_infection_timer + 1,0)),
           V1_infection_timer = ifelse(V1_infected==1 & V1_infection_timer==0,1,
                                       ifelse(V1_infected==1 & V1_infection_timer>0,
                                              V1_infection_timer+1,0)))
}
