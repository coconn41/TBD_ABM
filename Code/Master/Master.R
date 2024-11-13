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

#####
# Load data:
#####
# Agents and patches:
source(paste0(getwd(),'/Code/Model_set_up/Load_model_environment.R'))
# Sunlight times (for daily activity):
source(paste0(getwd(),'/Code/Subroutines/Sunlight_times.R'))

#####
# Load model subroutines:
#####
source(paste0(getwd(),'/Code/Subroutines/Update_environment.R'))
source(paste0(getwd(),'/Code/Subroutines/Mouse_movement.R'))
source(paste0(getwd(),'/Code/Subroutines/Other_movement.R')) # Same as mouse currently

# Number of hourly timesteps
go_timesteps = 8760

# RUN model:
source(paste0(getwd(),'/Code/Master/Model_loop.R'))
