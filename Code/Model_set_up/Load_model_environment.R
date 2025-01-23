Host_agents = read.csv(paste0(getwd(),'/Cached_data/Host_agents.csv'))[,-1] %>%
  mutate(V1_infected = 0,
         Ha_infected = 0,
         Age = 0,
         Kill = 0,
         Ha_infection_timer = 0,
         V1_infection_timer = 0,
         tick_links = 0,
         locs = paste0(row,",",col,",",network_ID))
tick_agents = read.csv(paste0(getwd(),'/Cached_data/Tick_agents.csv'))[,-1] %>%
  mutate(tick_age_wks = ifelse(Lifestage=="Adult",65,# Starting age Summer solstice (Jun. 21st) to Sept. 22nd of next year
                               ifelse(Lifestage=="Nymph",39,NA))) %>% # Starting age Summer solstice (Jun. 21st) to March 20th of next year
  mutate(die = 0,
         linked_type = "N",
         num_ticks = 1,
         dropped = 0,
         time_since_fed = 0,
         attempted_pathogen_transfer = 0)
jump_probability_df = read.csv(paste0(getwd(),"/Cached_data/jump_probability_df.csv"))[,-1]

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
spat_network = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
  rename(lcp_distance = "lcp_dst",
         origin_ID = "orgn_ID",
         destination_ID = "dstn_ID",
         distance = "distanc",
         inverse_sinuousity = "invrs_s",
         network_ID = "ntwr_ID")
spat_network = spat_network %>%
  bind_rows(.,spat_network %>%
              mutate(destination_ID2 = origin_ID,
                     origin_ID2 = destination_ID,
                     destination_ID = destination_ID2,
                     origin_ID = origin_ID2) %>%
              select(-c(destination_ID2,origin_ID2))) %>%
  distinct()
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

# calculate network:



# patch_resource_df = reduced_patches %>%
#   select(layer,gridrows,gridcols) %>%
#   st_drop_geometry() %>%
#   distinct() %>%
#   #head(2) %>% # uncomment to use full dataset
#   uncount(gridrows) %>%
#   group_by(layer) %>%
#   mutate(row = 1:n()) %>%
#   ungroup() %>%
#   uncount(gridcols) %>%
#   group_by(layer,row) %>%
#   mutate(col = 1:n()) %>%
#   ungroup() %>%
#   mutate(vegetation = round(runif(n=nrow(.),-.5,5.5))) 
# Assign network IDs to patches:
# Detect matches:
list_network = network1 %>%
  #st_drop_geometry() %>% 
  pivot_longer(.,c(origin_ID,destination_ID)) %>%
  select(network_ID,value) %>%
  group_by(network_ID, value) %>%
  summarise(tot = n()) %>%
  ungroup() %>%
  group_by(value) %>%
  summarise(tot = n())
# patches are not duplicated across networks

network2 = network1 %>%
  #st_drop_geometry() %>% 
  pivot_longer(.,c(origin_ID,destination_ID)) %>%
  select(network_ID,value) %>%
  group_by(network_ID, value) %>%
  summarise(tot = n()) %>%
  ungroup() %>%
  select(-tot) %>%
  rename(layer = "value")

aspatial_network = network1 %>%
  #st_drop_geometry() %>%
  group_by(origin_ID) %>%
  mutate(sample_probability = inverse_sinuousity / sum(inverse_sinuousity))
  

reduced_patches = left_join(reduced_patches,network2)

deer_agents = Host_agents %>% 
  filter(Agent_type == "Deer") 

mouse_agents = Host_agents %>%
  filter(Agent_type == "Mouse")

remove(list_network,network2,Host_agents)
