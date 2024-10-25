for(i in 1:go_timesteps){
  # Update environment
  update_enviro(i)
  
  # Move mice
  mouse_movement(mouse_agents,daytime)
  
  # Move others
  other_movement(other_agents,daytime)
  
  # Move deer
  
  # Attach ticks
  
  # Update tick timer
  
  # Mate ticks
  
  # Drop off ticks
  
  # Update tick processes (Lay eggs, molt, die)
  
  # Track population data
  
  setTxtProgressBar(pb,i)
}
  
  
  