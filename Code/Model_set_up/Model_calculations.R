# If on running via a HPCC slurm script, set wd
setwd("/user/collinoc/Cluster_TBD_ABM/")

# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Calculate model agents, environment, and network
use_cached_data = TRUE
source(paste0(getwd(),'/Code/Model_set_up/Calc_mod_setup.R')) # Almost complete, need to run lcps and calculate network
print("finished loading data")

# Create agent data frame:
source(paste0(getwd(),'/Code/Model_set_up/Create_agents.R'))
# Add other agents