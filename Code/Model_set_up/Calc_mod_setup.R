if(use_cached_data == TRUE){fin_poly = read.csv(paste0(getwd(),'/Cached_data/fin_poly.csv'))}
if(use_cached_data == FALSE){
# Bring in tick data and select patches to build model around:
source(paste0(getwd(),'/Code/Model_set_up/Clean_tick_data.R'))
source(paste0(getwd(),'/Code/Model_set_up/Select_sites.R'))
# Get all forest patches to calculate number of deer:
source(paste0(getwd(),'/Code/Model_set_up/Get_forest_patches.R')) # check to make sure works

# Calculate number of agents:
source(paste0(getwd(),'/Code/Model_set_up/Calculate_number_of_agents.R')) # Check to make sure works

# Assign links between forest patches:
source(paste0(getwd(),'/Code/Model_set_up/Calculate_lcp_links.R'))}