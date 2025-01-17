kill_hosts_fn = function(deer_agents,mouse_agents){
  deer_agents <<- deer_agents %>%
    mutate(Kill = ) %>% # Make a kill function based on exponential distribution?
    mutate(Age = ifelse(Kill==1,0,Age),
           Ha_infected = ifelse(Kill==1,0,Ha_infected),
           V1_infected = ifelse(Kill==1,0,V1_infected),
           Kill = 0)
  
  mouse_agents <<- mouse_agents %>%
    mutate(Kill = ) %>% # Make a kill function based on exponential distribution?
    mutate(Age = ifelse(Kill==1,0,Age),
           Ha_infected = ifelse(Kill==1,0,Ha_infected),
           V1_infected = ifelse(Kill==1,0,V1_infected),
           Kill = 0)
}

