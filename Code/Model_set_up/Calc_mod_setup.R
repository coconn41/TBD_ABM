if(use_cached_data == TRUE){
  all_sites = read_sf(paste0(getwd(),'/Cached_data/all_sites.shp')) %>%
    rename(Location_ID = "Lctn_ID",
           loc_county = "lc_cnty",
           loc_name = "loc_nam")
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
           deer_agents_adjusted = "dr_gnt_",
           mouse_agents_adjusted = "ms_gnt_",
           patch_type = "ptch_ty") %>%
    filter(is.na(layer)==F)
  reduced_patches = read_sf(paste0(getwd(),'/Cached_data/Reduced_patches.shp')) %>%
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
           deer_agents_adjusted = "dr_gnt_",
           mouse_agents_adjusted = "ms_gnt_",
           patch_type = "ptch_ty")
  comps = read.csv(paste0(getwd(),'/Cached_data/comps.csv'))[,-1]
  Rgrid = terra::rast(paste0(getwd(),'/Cached_data/Resistance_grid.tiff'))
  crs(Rgrid) = "epsg:32618"
  nodes = st_centroid(fin_all_patch)
  poly_tick_agents = read_sf(paste0(getwd(),'/Cached_data/poly_tick_agents.shp')) %>%
    rename(loc_county = "lc_cnty",
           loc_name = "loc_nam",
           gridrows = "gridrws",
           gridcols = "gridcls",
           gridrows_adjusted = "grdrws_",
           gridcols_adjusted = "grdcls_",
           Lifestage = "Lifestg",
           number_ticks_projected = "nm_tck_",
           Density_p_m = "Dnsty__")
  lcp_network = read_sf(paste0(getwd(),'/Cached_data/lcp_network.shp')) %>%
    rename(lcp_distance = 'lcp_dst',
           origin_ID = 'orgn_ID',
           destination_ID = 'dstn_ID',
           distance = 'distanc',
           inverse_sinuousity = 'inv_sns')
  
  site_df = data.frame(Site = c("Hanging Bog", # conn = .6249, ha = .11, v1 = -0.05
                                "Dwaas Kill Nature Preserve", #conn = .165, ha = .42, v1 = -0.26
                                "Allegany State Park", #conn = .85, ha = .12, v1 = .137
                                "Moss Lake", # conn = .417, ha = .426, v1 = .002
                                "Mohansic Golf Course", # conn = .49, ha = -2.42e, v1 = .012
                                "Veterans Memorial Park", # conn = .21, ha = -4e15, v1 = .07
                                "Pine Barrens Trails", # conn = .53, ha = -4, v1 =  -1.8
                                "Saratoga Spa State Park"), #conn = .176, ha = -3.74, v1 = -.005
                       Connectivity = rep(c("High","Low"),4),
                       ha_relationship = c(rep("Positive",4),
                                           rep("Negative",4)),
                       v1_relationship = c(rep("Negative",2),
                                           rep("Positive",4),
                                           rep("Negative",2)),
                       Predict = c("Yes","No","Maybe","Maybe","No","Yes","Maybe","Maybe"))
  Loc_metric_table_w_private = read_csv(paste0(getwd(),"/Data/TL_data/Loc_metric_table_w_private.csv"))[,-1] %>%
    st_as_sf(.,coords=c('longitude','latitude')) %>%
    st_set_crs(.,value=4326) %>%
    st_transform(.,crs=32618)
  selected_sites = Loc_metric_table_w_private %>%
    filter(loc_name %in% site_df$Site) %>%
    mutate(Site_type = "Node")
  network1 = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
    rename(lcp_distance = "lcp_dst",
           origin_ID = "orgn_ID",
           destination_ID = "dstn_ID",
           distance = "distanc",
           inverse_sinuousity = "invrs_s",
           network_ID = "ntwr_ID")
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