# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Bring in tick data and select patches to build model around:
source(paste0(getwd(),'/Code/Model_set_up/Clean_tick_data.R'))
source(paste0(getwd(),'/Code/Model_set_up/Select_sites.R'))

# Get all forest patches to calculate number of deer:
source(paste0(getwd(),'/Code/Model_set_up/Get_forest_patches.R')) # check to make sure works

# Calculate number of agents:
source(paste0(getwd(),'/Code/Model_set_up/Calculate_number_of_agents.R')) # Check to make sure works

# Assign links between forest patches:
source(paste0(getwd(),'/Code/Model_set_up/Calculate_lcp_links.R'))

# Number of agents to start model
eggs = 100
larvae = 0
fedlarvae = 100 # to start the model
nymphs = 0
adult_males = 20
adult_females = 20
other = 0

# Calculate number of agents:
source(paste0(getwd(),'/Code/Model_set_up/Calculate_number_of_agents.R'))
# Add other agents

# Create agent data frame:
source(paste0(getwd(),'/Code/Model_set_up/Create_agents.R'))
# Add other agents

# Number of hourly timesteps
go_timesteps = 8760

# Set up model:
source(paste0(getwd(),'/Code/Model_set_up/Configure.R'))

# Load model updating functions:
source(paste0(getwd(),'/Code/Model_set_up/Update_environment.R'))

# Load model subroutines:
source(paste0(getwd(),'/Code/Subroutines/Mouse_movement.R'))
source(paste0(getwd(),'/Code/Subroutines/Other_movement.R')) # Same as mouse currently

# Run model:
source(paste0(getwd(),'/Code/Master/Model_loop.R'))
