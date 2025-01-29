groom_fn = function(tick_agents,deer_agents){
  tick_agents <- tick_agents %>%
    mutate(die = ifelse(time_on_host > 0 & linked_type == "Deer",
                        rbinom(n=1,size=1,prob = deer_GR),
                        ifelse(time_on_host > 0 & linked_type == "Mouse",
                               rbinom(n=1,size=1,prob = mouse_GR),0))) %>% # deer_GR mouse_GR
    mutate(die = ifelse(die==1,ifelse(rbinom(n=1,size=1,prob = Groom_survival)==1,
                                      0,1),0)) # Groom survival function
  
  groom_list <<- subset(tick_agents,tick_agents$die==1)$Agent_ID
  
  deer_agents <<- deer_agents %>% 
    mutate(tick_links = ifelse(tick_links %in% groom_list,0,tick_links))
  
  mouse_agents <<- mouse_agents %>%
    mutate(tick_links = ifelse(tick_links %in% groom_list,0,tick_links))
  
  tick_agents <<- tick_agents %>%
    filter(die==0)
}
