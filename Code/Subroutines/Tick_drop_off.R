tick_drop_fn = function(tick_agents){
  L_ticks = tick_agents %>%
    filter(Lifestage == "Larvae",
           fed == 1,
           dropped == 0) %>%
    mutate(dropped = sum(rbinom(n = num_ticks, size = 1, prob = 1/(12-time_since_fed))),
           num_ticks = num_ticks - dropped) 
  
  L_ticks2 = L_ticks %>%
    uncount(dropped) %>%
    mutate(dropped = 1,
           Agent_ID = (max(tick_agents$Agent_ID)+1):((max(tick_agents$Agent_ID))+nrow(.)),
           num_ticks = 1)
  
  L_ticks = L_ticks %>%
    mutate(dropped = 0)
  tick_agents <<- tick_agents %>%
    mutate(dropped = ifelse(fed == 1 & Lifestage == "Adult",1,
                            ifelse(fed == 1 & Lifestage == "Nymph",1,fed))) %>%
    filter(!c(Agent_ID %in% L_ticks)) %>%
    bind_rows(.,L_ticks) %>%
    bind_rows(.,L_ticks2) %>%
    mutate(row = ifelse(links>0&dropped==1&linked_type == "Deer",
                        deer_agents[which(deer_agents$Agent_ID==links),]$row,
                        ifelse(links>0&dropped==1&linked_type == "Mouse",
                               mouse_agents[which(mouse_agents$Agent_ID==links),]$row,row)),
           col = ifelse(links>0&dropped==1&linked_type == "Deer",
                        deer_agents[which(deer_agents$Agent_ID==links),]$col,
                        ifelse(links>0&dropped==1&linked_type == "Mouse",
                               mouse_agents[which(mouse_agents$Agent_ID==links),]$col,col)),
           layer = ifelse(links>0&dropped==1&linked_type == "Deer",
                          deer_agents[which(deer_agents$Agent_ID==links),]$layer,
                          ifelse(links>0&dropped==1&linked_type == "Mouse",
                                 mouse_agents[which(mouse_agents$Agent_ID==links),]$layer,layer))) %>%
    mutate(links = ifelse(links>0&dropped==1,0,links))
}


