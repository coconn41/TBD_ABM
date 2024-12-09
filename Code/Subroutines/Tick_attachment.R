attach_ticks = function(tick_agents,deer_agents,mouse_agents,other_agents){
    tick_agents <<- tick_agents %>%
      mutate(links = ifelse((locs %in% deer_agents$locs)==T,
                            match(locs,deer_agents$locs,0),0))
}

# add mouse and duplicate attachment sampling function