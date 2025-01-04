groom_fn = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(die = ifelse(time_on_host > 0 & linked_type == "Deer",
                        rbinom(n=1,size=1,prob = deer_GR),
                        ifelse(time_on_host > 0 & linked_type == "Mouse",
                               rbinom(n=1,size=1,prob = mouse_GR),0))) %>% # deer_GR mouse_GR
    mutate(die = ifelse(die==1,ifelse(rbinom(n=1,size=1,prob = Groom_survival)==1,
                                      0,1),0)) %>% # Groom survival function
    filter(die==0)
}
