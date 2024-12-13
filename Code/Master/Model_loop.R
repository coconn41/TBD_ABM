for(i in 1:go_timesteps){
  # Update environment
  update_enviro(i)
  
  # Move mice
  mouse_movement(mouse_agents,daytime)
  
  # Move others
  other_movement(other_agents,daytime)
  
  # Move deer
  deer_movement(deer_agents = deer_agents,
                daytime = daytime,
                network = aspatial_network)
  
  # Create deer paths
  create_deer_paths(deer_agents = deer_agents)
  
  # Attach ticks
  attach_ticks(tick_agents = tick_agents,
               deer_paths = deer_paths,
               mouse_agents = mouse_agents,
               LA_probability = LA_probability,
               NA_probability = NA_probability,
               AA_probability = AA_probability)
  
  # Update tick timer
  
  # Mate ticks
  
  # Drop off ticks
  
  # Update tick processes (Lay eggs, molt, die)
  
  # Track population data
  
  setTxtProgressBar(pb,i)
}
  
  
  