########### Load simplified model from file

#####
# Load patches/network:
#####
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
  mutate(deer_agents = round(ifelse(hectare<=1,2,deer_density*hectare)),
         deer_p_ha = deer_density,
         mouse_agents = round(rtruncnorm(n=1,a=0,mean = hectare*mouse_density,sd = 5)),
         mice_p_ha = mouse_density) %>%
  mutate(deer_agents_adjusted = deer_agents,
         mouse_agents_adjusted = mouse_agents)

network1 = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
  rename(lcp_distance = "lcp_dst",
         origin_ID = "orgn_ID",
         destination_ID = "dstn_ID",
         distance = "distanc",
         inverse_sinuousity = "invrs_s",
         network_ID = "ntwr_ID")
network1 = network1 %>%
  bind_rows(.,network1 %>%
              mutate(destination_ID2 = origin_ID,
                     origin_ID2 = destination_ID,
                     destination_ID = destination_ID2,
                     origin_ID = origin_ID2) %>%
              select(-c(destination_ID2,origin_ID2))) %>%
  distinct() %>%
  st_drop_geometry() %>%
  #dplyr::select(-geometry) %>%
  group_by(origin_ID,destination_ID,network_ID) %>%
  summarise(lcp_distance = mean(lcp_distance),
            distance = mean(distance),
            inverse_sinuousity = mean(inverse_sinuousity)) %>%
  distinct()
#####
# Load jump probability df:
#####
jump_probability_df = read.csv(paste0(getwd(),"/Cached_data/jump_probability_df.csv"))[,-1]

#####
# Create agents:
#####
source(paste0(getwd(),'/code/Model_set_up/Create_agents_simplified.R'))