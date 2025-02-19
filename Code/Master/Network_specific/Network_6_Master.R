# If on running via a HPCC slurm script, set wd
setwd("/user/collinoc/Cluster_TBD_ABM/")

# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Set number of cores:
cores = 8
if(detectCores()>10){computer = "Cluster"
large_cores = 24}
if(detectCores()==10){computer = "Personal"
large_cores = 8}

#####
# Calculate data or load data:
#####
calculate_data = FALSE
if(calculate_data==TRUE){source(paste0(getwd(),'/Code/Model_set_up/Calc_mod_setup.R'))}

#####
# Load data:
#####
# Agents and patches:
source(paste0(getwd(),'/Code/Model_set_up/Load_model_environment.R'))
# Sunlight times (for daily activity):
source(paste0(getwd(),'/Code/Model_set_up/Sunlight_times.R'))

#####
# Load parameter values
#####
source(paste0(getwd(),"/Code/Parameters/Parameter_script.R"))

#####
# Load model subroutines:
#####
source(paste0(getwd(),'/Code/Subroutines/Update_environment.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Mouse_movement.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Deer_movement.R')) # Good
#source(paste0(getwd(),'/Code/Subroutines/Other_movement.R')) # Not updated yet
source(paste0(getwd(),'/Code/Subroutines/Create_deer_paths.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_attachment.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Groom_attached_ticks.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_mating.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_timer.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_drop_off.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_molting.R'))
source(paste0(getwd(),'/Code/Subroutines/lay_eggs.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Transfer_pathogens.R'))
source(paste0(getwd(),'/Code/Subroutines/Tick_death.R'))
source(paste0(getwd(),'/Code/Subroutines/Host_timer.R'))
source(paste0(getwd(),'/Code/Subroutines/Kill_hosts.R'))
source(paste0(getwd(),'/Code/Master/Compile_results.R'))

# Model starting timing:
year=0
day=265 
daytime = "night"
season = "fall"

# Number of hourly timesteps
go_timesteps = (8760*5)

# Select network: either "all" or the network number
net_select = 6

# Parameter modification:
deer_attach_prob = .25
mouse_attach_prob = .25
deer_trans_param = 1#deer_infect_tick_v1
mouse_trans_param = 1#mouse_infect_tick_ha


# RUN model:
source(paste0(getwd(),'/Code/Master/Network_specific/Model_loop_6.R'))

# Save results:
if(i==go_timesteps){
  if(deer_infect_tick_v1<.1){pathogen_label="apriori"}
  if(deer_infect_tick_v1>=.1){pathogen_label=deer_infect_tick_v1*100}
  save.image(file = paste0(getwd(),"/Simulations/Attach_",substring(deer_attach_prob*100,1,3),
                           "/Pathogen_",substring(pathogen_label,1,3),"/Network_",net_select,
                           "/BI_attach_",deer_attach_prob*100,
                           "_path_trans_",substring(pathogen_label,1,3),".RData"))}
if(i>(8760*5)){
  if(deer_infect_tick_v1<.1){pathogen_label="apriori"}
  if(deer_infect_tick_v1>=.1){pathogen_label=deer_infect_tick_v1*100}
  save.image(file = paste0(getwd(),"/Simulations/Attach_",substring(deer_attach_prob*100,1,3),
                           "/Pathogen_",substring(pathogen_label,1,3),"/Network_",net_select,
                           "/post_BI_attach_",deer_attach_prob*100,
                           "_path_trans_",substring(pathogen_label,1,3),
                           i/8760,"_simyears.RData"))
}
# write.csv(deer_data2,paste0(getwd(),"/Simulations/Deer/Deer_results_network_",
#                             net_select,"_",Sys.Date(),"_",substring(Sys.time(),12,16),
#                             "_.csv"))
# write.csv(mouse_data2,paste0(getwd(),"/Simulations/Mice/Mouse_results_network_",
#                              net_select,"_",Sys.Date(),"_",substring(Sys.time(),12,16),
#                              "_.csv"))
# write.csv(tick_data2,paste0(getwd(),"/Simulations/Ticks/Tick_results_network_",
#                             net_select,"_",Sys.Date(),"_",substring(Sys.time(),12,16),
#                             "_.csv"))

# Save burn in:
# save.image(file = paste0(getwd(),"/Simulations/Burn_in/Burn_in_environment_",
#                          net_select,".RData"))
# write.csv(unnest_wider(deer_agents,tick_links,names_sep="_"),paste0(getwd(),"/Simulations/Burn_in/deer_burn_in_",
#                              net_select,"_.csv"))
# write.csv(unnest_wider(mouse_agents,tick_links,names_sep="_"),paste0(getwd(),"/Simulations/Burn_in/mouse_burn_in_",
#                               net_select,"_.csv"))
# write.csv(tick_agents,paste0(getwd(),"/Simulations/Burn_in/tick_burn_in_",
#                              net_select,"_.csv"))

