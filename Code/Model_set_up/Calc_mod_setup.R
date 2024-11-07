if(use_cached_data == TRUE){fin_poly = read.csv(paste0(getwd(),'/Cached_data/fin_poly.csv'))}
if(use_cached_data == FALSE){
# Bring in tick data and select patches to build model around:
print("Cleaning tick data")
source(paste0(getwd(),'/Code/Model_set_up/Clean_tick_data.R'))
print("Selecting sites")
source(paste0(getwd(),'/Code/Model_set_up/Select_sites.R'))
# Get all forest patches to calculate number of deer:
print("Getting forest patches")
source(paste0(getwd(),'/Code/Model_set_up/Get_forest_patches.R'))

# Calculate number of agents:
print("Calculating number of agents")
source(paste0(getwd(),'/Code/Model_set_up/Calculate_number_of_agents.R'))

# Assign links between forest patches:
print("Calculating lcps")
source(paste0(getwd(),'/Code/Model_set_up/Calculate_lcp_links.R'))}