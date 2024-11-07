# If on running via a HPCC slurm script, set wd
setwd("/user/collinoc/Cluster_TBD_ABM/")

# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Set number of cores:
cores = 36

# Calculate model agents, environment, and network
use_cached_data = FALSE
source(paste0(getwd(),'/Code/Model_set_up/Calc_mod_setup.R')) # Almost complete, need to run lcps and calculate network
print("finished loading data")
error
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
