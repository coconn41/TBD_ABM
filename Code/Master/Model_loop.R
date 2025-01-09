if(net_select!="all"){
deer_agents = deer_agents %>% 
  filter(network_ID == net_select)
mouse_agents = mouse_agents %>%
  filter(network_ID == net_select)
tick_agents = tick_agents %>%
  filter(network_ID == net_select)
jump_probability_df = jump_probability_df %>%
  filter(network_ID == net_select)
aspatial_network = aspatial_network %>%
  filter(network_ID == net_select)
network1 = network1 %>%
  filter(network_ID == net_select)
reduced_patches = reduced_patches %>%
  filter(network_ID == net_select)
spat_network = spat_network %>%
  filter(network_ID == net_select)}

start_time = Sys.time()
for(i in 1:48){#go_timesteps
  # Update environment
  update_enviro(i,daylight)
  
  # Move mice
  if(daytime=="day"){mouse_movement(mouse_agents,daytime)}
  
  # Move others
  #other_movement(other_agents,daytime)
  
  # Move deer
  if(daytime=="day"){deer_movement(deer_agents = deer_agents,
                daytime = daytime,
                network = aspatial_network)}
if(season!="winter"){  
  # Create deer paths
  if(daytime=="day"){create_deer_paths(deer_agents = deer_agents)}
  
  # Attach ticks
  if(daytime=="day"){attach_ticks(tick_agents = tick_agents,
               deer_paths = deer_paths,
               mouse_agents = mouse_agents,
               LA_probability = LA_probability,
               NA_probability = NA_probability,
               AA_probability = AA_probability)}
  
  # Transfer pathogens
  transfer_pathogens(tick_agents = tick_agents,
                     deer_agents = deer_agents,
                     mouse_agents = mouse_agents)
  
  # Groom ticks
  if(daytime=="day"){groom_fn(tick_agents = tick_agents)}
  
  # Mate ticks
  if(daytime=="day"){mating_fn(tick_agents = tick_agents)}
  
  # Tick timer
  tick_timer(tick_agents = tick_agents)
  
  # Tick drop off
  tick_drop_fn(tick_agents = tick_agents)
  
  # Tick death
  tick_death(tick_agents = tick_agents)
  }
}
end_time = Sys.time()
end_time-start_time
#9.043 minutes to do network 3
  
  # Update tick processes (Lay eggs, molt, die)
  
  # Track population data
  
  setTxtProgressBar(pb,i)
}
  
  
  