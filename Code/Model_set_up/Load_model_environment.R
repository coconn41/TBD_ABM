Host_agents = read.csv(paste0(getwd(),'/Cached_data/Host_agents.csv'))
Tick_agents = read.csv(paste0(getwd(),'/Cached_data/Tick_agents.csv'))
network1 = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
  rename(lcp_distance = "lcp_dst",
         origin_ID = "orgn_ID",
         destination_ID = "dstn_ID",
         distance = "distanc",
         inverse_sinuousity = "invrs_s",
         network_ID = "ntwr_ID")
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
# Assign network IDs to patches:
# Detect matches:
list_network = network1 %>%
  st_drop_geometry() %>% 
  pivot_longer(.,c(origin_ID,destination_ID)) %>%
  select(network_ID,value) %>%
  group_by(network_ID, value) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  group_by(value) %>%
  summarize(tot = n())
# patches are not duplicated across networks

network2 = network1 %>%
  st_drop_geometry() %>% 
  pivot_longer(.,c(origin_ID,destination_ID)) %>%
  select(network_ID,value) %>%
  group_by(network_ID, value) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  select(-tot) %>%
  rename(layer = "value")
reduced_patches = left_join(reduced_patches,network2)

remove(list_network,network2)
