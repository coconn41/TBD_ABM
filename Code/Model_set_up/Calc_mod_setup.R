if(use_cached_data == TRUE){
  all_sites = read_sf(paste0(getwd(),'/Cached_data/all_sites.shp'))
  fin_all_patch = read_sf(paste0(getwd(),'/Cached_data/fin_all_patch.shp')) %>%
    rename(Location_ID = "Lctn_ID",
           loc_county = "lc_cnty",
           loc_name = "loc_nam",
           gridrows = "gridrws",
           gridcols = "gridcls",
           deer_agents = "dr_gnts",
           deer_p_ha = "der_p_h",
           mouse_agents = "ms_gnts",
           mice_p_ha = "mic_p_h",
           gridrows_adjusted = "grdrws_",
           gridcols_adjusted = "grdcls_",
           adjusted_ratio = "adjstd_",
           deer_agents_adjusted = "dr_gnt_",
           mouse_agents_adjusted = "ms_gnt_",
           patch_type = "ptch_ty") %>%
    filter(is.na(layer)==F)
  comps = read.csv(paste0(getwd(),'/Cached_data/comps.csv'))[,-1]
  Rgrid = terra::rast(paste0(getwd(),'/Cached_data/Resistance_grid.tiff'))
  crs(Rgrid) = "epsg:32618"
  nodes = st_centroid(all_sites)
  poly_tick_agents = read.csv(paste0(getwd(),'/Cached_data/poly_tick_agents.csv'))
  lcp_network = read_sf(paste0(getwd(),'/Cached_data/lcp_network.shp')) %>%
    rename(lcp_distance = 'lcp_dst',
           origin_ID = 'orgn_ID',
           destination_ID = 'dstn_ID',
           distance = 'distanc',
           inverse_sinuousity = 'inv_sns')
  }
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