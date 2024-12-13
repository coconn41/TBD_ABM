# If on running via a HPCC slurm script, set wd
setwd("/user/collinoc/Cluster_TBD_ABM/")

# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Set number of cores:
if(detectCores()>8){computer = "Cluster"
cores = 24}
if(detectCores()==8){computer = "Personal"
cores = 4}

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
source(paste0(getwd(),'/Code/Subroutines/Update_environment.R'))
source(paste0(getwd(),'/Code/Subroutines/Mouse_movement.R'))
source(paste0(getwd(),'/Code/Subroutines/Deer_movement.R'))
source(paste0(getwd(),'/Code/Subroutines/Other_movement.R')) # Not updated yet
source(paste0(getwd(),'/Code/Subroutines/Create_deer_paths.R'))
source(paste0(getwd(),'/Code/Subroutines/Tick_attachment.R'))

# Model starting timing:
year=0
day=1
daytime = "night"

# Number of hourly timesteps
go_timesteps = 8760

# Do in parallel?
parallelize = TRUE
source(paste0(getwd(),'/Code/Master/Parallel.R'))

# RUN model:
source(paste0(getwd(),'/Code/Master/Model_loop.R'))
