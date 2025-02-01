kill_hosts_fn = function(deer_agents,mouse_agents){
  deer_agents <<- deer_agents %>%
    mutate(Kill = case_when(Age==(11*24*365) ~ 1,
                            TRUE ~ as.numeric(rbinom(n = n(),
                                          size = 1,
                                          prob = 1/(11*24*365))))) %>% # Maximum lifespan equal 11 years
    mutate(Age = ifelse(Kill==1,0,Age),
           Ha_infected = ifelse(Kill==1,0,Ha_infected),
           V1_infected = ifelse(Kill==1,0,V1_infected),
           Kill = 0)
  
  mouse_agents <<- mouse_agents %>%
    mutate(Kill = case_when(Age==(2*24*365)~1,
                            TRUE ~ as.numeric(rbinom(n = n(),
                                   size=1,
                                   prob = 1/(2*24*365))))) %>% # Maximum lifespan equal 2 years
    mutate(Age = ifelse(Kill==1,0,Age),
           Ha_infected = ifelse(Kill==1,0,Ha_infected),
           V1_infected = ifelse(Kill==1,0,V1_infected),
           Kill = 0)
}

