# If on running via a HPCC slurm script, set wd
setwd("/user/collinoc/Cluster_TBD_ABM/")

# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Set number of cores:
cores = 24

# Load data:
source(paste0(getwd(),'/Model_set_up/Load_model_environment.R'))

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
