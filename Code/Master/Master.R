# Clear model environment
rm(list=ls())

# Environment dimensions
gridrows = 1000
gridcols = 1000

# Number of agents
eggs = 100
larvae = 0
fedlarvae = 100 # to start the model
nymphs = 0
adult_males = 20
adult_females = 20
mice = 12
deer = 2
other = 0

# Number of hourly timesteps
go_timesteps = 8760

# Set random number state
set.seed(1)

# Set up model:
source(paste0(getwd(),'/Code/Model_set_up/Configure.R'))

# Load model updating functions:
source(paste0(getwd(),'/Code/Model_set_up/Update_environment.R'))

# Load model subroutines:
source(paste0(getwd(),'/Code/Subroutines/Mouse_movement.R'))
source(paste0(getwd(),'/Code/Subroutines/Other_movement.R')) # Same as mouse currently

# Run model:
source(paste0(getwd(),'/Code/Master/Model_loop.R'))
