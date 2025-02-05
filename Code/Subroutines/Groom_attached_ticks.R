groom_fn = function(tick_agents,deer_agents){
  
  groomed_ticks_deer <- c()
  
  deer_agents <<- deer_agents %>%
    mutate(tick_links = map2(tick_links, groom_timer, ~ {
      if (.y >= 1) {  
        idx <- sample(length(.x), 1)  # Choose a random index
        groomed_ticks_deer <<- c(groomed_ticks_deer, .x[idx])  # Store removed element
        result <- .x[-idx]  # Remove the element
        if (length(result) == 0) NA else result  # Replace empty list with NA
      } else {
        .x  
      }
    }))
    
  groomed_ticks_mice <- c() 
  
  mouse_agents <<- mouse_agents %>%
    mutate(tick_links = map2(tick_links, groom_timer, ~ {
      if (.y >= 1) {  
        idx <- sample(length(.x), 1)  # Choose a random index
        groomed_ticks_mice <<- c(groomed_ticks_mice, .x[idx])  # Store removed element
        result <- .x[-idx]  # Remove the element
        if (length(result) == 0) NA else result  # Replace empty list with NA
      } else {
        .x  
      }
    }))
  
  tick_agents <- tick_agents %>%
    filter(!c(Agent_ID%in%groomed_ticks_deer),
           !c(Agent_ID%in%groomed_ticks_mice))
}
