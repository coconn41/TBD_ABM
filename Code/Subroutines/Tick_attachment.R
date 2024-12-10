attach_ticks = function(tick_agents,deer_paths,mouse_agents,other_agents){
    tick_agents <<- tick_agents %>%
      mutate(links = ifelse(paste0(row,",",
                                   col,",",
                                   network_ID) %in% paste0(deer_paths$row,",",
                                              deer_paths$col,",",
                                              deer_paths$network_ID)==T,
                            match(paste0(.$row,",",
                                         .$col,",",
                                         .$network_ID),
                                  paste0(deer_paths$row,",",
                                         deer_paths$col,",",
                                         deer_paths$network_ID),0),0)) %>%
      mutate(links = map_int(links, ~ ifelse(. > 0, deer_paths$Agent_ID[.], 0)))
}

# add mouse and duplicate attachment sampling function