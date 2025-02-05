tick_drop_fn = function(tick_agents){
  L_ticks = tick_agents %>%
    filter(Lifestage == "Larvae",
           fed == 1,
           dropped == 0) %>%
    mutate(dropped = sum(rbinom(n = n(), size = num_ticks, prob = 1/(12-time_since_fed))),
           num_ticks = num_ticks - dropped) 
donext = TRUE
if(nrow(L_ticks)!=0 & sum(L_ticks$dropped)>0){ L_ticks2 = L_ticks %>%
    uncount(dropped) %>%
    mutate(dropped = -1,
           Agent_ID = (max(tick_agents$Agent_ID)+1):((max(tick_agents$Agent_ID))+nrow(.)),
           num_ticks = 1)
  
  L_ticks = L_ticks %>%
    mutate(dropped = 0)
  tick_agents <- tick_agents %>%
    mutate(dropped = ifelse(fed == 1 & Lifestage == "Adult",-1,
                            ifelse(fed == 1 & Lifestage == "Nymph",-1,dropped))) %>%
    filter(!c(Agent_ID %in% L_ticks$Agent_ID)) %>%
    bind_rows(.,L_ticks) %>%
    bind_rows(.,L_ticks2)
  
  dropped_IDs = tick_agents %>% filter(dropped==-1)
  dropped_IDs = dropped_IDs$Agent_ID
  
deer_agents <<- deer_agents %>%
  mutate(tick_links = ifelse(tick_links%in%dropped_IDs,0,tick_links))
mouse_agents <<- mouse_agents %>%
  mutate(tick_links = ifelse(tick_links%in%dropped_IDs,0,tick_links))
  
tick_agents <<- tick_agents %>%
    mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
    mutate(row = ifelse(links>0&dropped==1&linked_type == "Deer",
                        deer_agents[match(.$links,deer_agents$Agent_ID),]$row,
                        #deer_agents[which(deer_agents$Agent_ID==links),]$row,
                        ifelse(links>0&dropped==1&linked_type == "Mouse",
                               mouse_agents[match(.$links,mouse_agents$Agent_ID),]$row,row)),
           #mouse_agents[which(mouse_agents$Agent_ID==links),]$row,row)),
           col = ifelse(links>0&dropped==1&linked_type == "Deer",
                        deer_agents[match(.$links,deer_agents$Agent_ID),]$col,
                        #  deer_agents[which(deer_agents$Agent_ID==links),]$col,
                        ifelse(links>0&dropped==1&linked_type == "Mouse",
                               mouse_agents[match(.$links,mouse_agents$Agent_ID),]$col,col)),
           #mouse_agents[which(mouse_agents$Agent_ID==links),]$col,col)),
           layer = ifelse(links>0&dropped==1&linked_type == "Deer",
                          deer_agents[match(.$links,deer_agents$Agent_ID),]$layer,
                          #deer_agents[which(deer_agents$Agent_ID==links),]$layer,
                          ifelse(links>0&dropped==1&linked_type == "Mouse",
                                 mouse_agents[match(.$links,mouse_agents$Agent_ID),]$layer,layer))) %>%
    mutate(links = ifelse(links>0&dropped==1,0,links))
donext = FALSE
}
if(donext==TRUE){
if(nrow(L_ticks)==0 | sum(L_ticks$dropped)==0){tick_agents <- tick_agents %>%
    mutate(dropped = ifelse(fed == 1 & Lifestage == "Adult",-1,
                            ifelse(fed == 1 & Lifestage == "Nymph",-1,dropped))) %>%
    filter(!c(Agent_ID %in% L_ticks$Agent_ID)) %>%
    bind_rows(.,L_ticks)

dropped_IDs = tick_agents %>% filter(dropped==-1)
dropped_IDs = dropped_IDs$Agent_ID

deer_agents$tick_links = map(deer_agents$tick_links, ~ .x[!(.x %in% dropped_IDs)])
mouse_agents$tick_links = map(mouse_agents$tick_links, ~ .x[!(.x %in% dropped_IDs)])

# deer_agents <<- deer_agents %>%
#   mutate(tick_links = ifelse(tick_links%in%dropped_IDs,0,tick_links))
# mouse_agents <<- mouse_agents %>%
#   mutate(tick_links = ifelse(tick_links%in%dropped_IDs,0,tick_links))

tick_agents <- tick_agents %>%
  mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
    mutate(row = ifelse(links>0&dropped==1&linked_type == "Deer",
                        deer_agents[match(.$links,deer_agents$Agent_ID),]$row,
                        #deer_agents[which(deer_agents$Agent_ID==links),]$row,
                        ifelse(links>0&dropped==1&linked_type == "Mouse",
                               mouse_agents[match(.$links,mouse_agents$Agent_ID),]$row,row)),
                               #mouse_agents[which(mouse_agents$Agent_ID==links),]$row,row)),
           col = ifelse(links>0&dropped==1&linked_type == "Deer",
                        deer_agents[match(.$links,deer_agents$Agent_ID),]$col,
                      #  deer_agents[which(deer_agents$Agent_ID==links),]$col,
                        ifelse(links>0&dropped==1&linked_type == "Mouse",
                               mouse_agents[match(.$links,mouse_agents$Agent_ID),]$col,col)),
                               #mouse_agents[which(mouse_agents$Agent_ID==links),]$col,col)),
           layer = ifelse(links>0&dropped==1&linked_type == "Deer",
                          deer_agents[match(.$links,deer_agents$Agent_ID),]$layer,
                          #deer_agents[which(deer_agents$Agent_ID==links),]$layer,
                          ifelse(links>0&dropped==1&linked_type == "Mouse",
                                 mouse_agents[match(.$links,mouse_agents$Agent_ID),]$layer,layer))) %>%
                                # mouse_agents[which(mouse_agents$Agent_ID==links),]$layer,layer))) #%>%
    mutate(links = ifelse(links>0&dropped==1,0,links))}}
return(tick_agents)
return(deer_agents)
return(mouse_agents)
}


