# Change this to patches when ready to run full model and convert to the HPC cluster
# See above, include all non-site patches
Deer_data <- read_excel(paste0(getwd(),"/Data/Deer_data.xlsx")) %>%
  filter(huntyear %in% c(2006,2007,2008)) %>%
  group_by(wmu) %>%
  summarize(mean_total = mean(Total,na.rm=T))
Deer_data$total_kill = sum(Deer_data$mean_total)
fin_poly = ptch2
fin_nodes = fin_poly %>% 
  st_drop_geometry() %>% 
  rename(patch_area = 'area',
         WMU_area = 'area.1',
         wmu = "UNIT") %>%
  left_join(.,Deer_data) %>%
  left_join(all_sites %>%
              st_drop_geometry(),
            .,
            join_by(layer)) %>%
  left_join(.,ptch2 %>%
              st_drop_geometry() %>%
              rename(wmu = "UNIT",
                     patch_area = "area",
                     wmu_area = "area.1") %>%
              group_by(wmu) %>%
              summarize(total_patches_area = sum(patch_area,na.rm=T))) %>%
  dplyr::select(-metric.y) %>%
  rename(metric = "metric.x") %>%
  mutate(gridrows = round(sqrt(patch_area)),
         gridcols = round(sqrt(patch_area)),
         patch_percent = patch_area/total_patches_area,
         estimated_wmu_deer_killed = mean_total*patch_percent,
         hectare = patch_area*.0001) %>%
  mutate(deer_agents = ifelse(hectare<=1,2,round(((1/metric)*estimated_wmu_deer_killed)/(total_kill/1000000)))) %>%
  group_by(Location_ID,loc_county,loc_name,layer,metric,area_m2,
           patch_area,area_ha,gridrows,gridcols,hectare) %>%
  summarize(deer_agents = sum(deer_agents,na.rm = T)) %>% 
  mutate(deer_p_ha = deer_agents/hectare,
         mouse_agents = rtruncnorm(n=1,a=0,mean = hectare*50,sd = 5),
         mice_p_ha = mouse_agents/hectare) %>%
  mutate(gridrows_adjusted = ifelse(hectare>1,100,gridrows),
         gridcols_adjusted = ifelse(hectare>1,100,gricols),
         deer_agents_adjusted = ifelse(deer_p_ha>5,5,round(deer_p_ha)),
         mouse_agents_adjusted = round(mice_p_ha),
         patch_type = "Node")

fin_all_patch = fin_poly %>%
  st_drop_geometry() %>%
  filter(!c(layer %in% fin_nodes$layer)) %>%
  rename(patch_area = "area",
         wmu_area = "area.1",
         wmu = "UNIT") %>%
  group_by(wmu) %>%
  summarize(total_patches_area = sum(patch_area,na.rm=T)) %>%
  left_join(.,Deer_data) %>%
  left_join(.,fin_poly %>%
              rename(patch_area = "area",
                     wmu_area = "area.1",
                     wmu = "UNIT")) %>%
  mutate(area_m2 = patch_area,
         area_ha = patch_area/10000,
         gridrows = round(sqrt(patch_area)),
         gridcols = round(sqrt(patch_area)),
         patch_percent = patch_area/total_patches_area,
         estimated_wmu_deer_killed = mean_total*patch_percent,
         hectare = patch_area*.0001) %>%
  mutate(deer_agents = ifelse(hectare<=1,2,round(((1/metric)*estimated_wmu_deer_killed)/(total_kill/1000000)))) %>%
  group_by(total_kill,layer,metric,patch_area,
           area_m2,area_ha,gridrows,gridcols,hectare) %>%
  summarize(deer_agents = sum(deer_agents,na.rm = T)) %>% 
  mutate(deer_p_ha = deer_agents/hectare,
         mouse_agents = rtruncnorm(n=1,a=0,mean = hectare*50,sd = 5),
         mice_p_ha = mouse_agents/hectare) %>%
  mutate(gridrows_adjusted = ifelse(hectare>1,100,gridrows),
         gridcols_adjusted = ifelse(hectare>1,100,gridcols),
         deer_agents_adjusted = ifelse(deer_p_ha>5,5,round(deer_p_ha)),
         mouse_agents_adjusted = round(mice_p_ha)) %>%
  ungroup() %>%
  dplyr::select(-total_kill) %>%
  mutate(Location_ID = NA,
         loc_county = NA,
         loc_name = NA,
         patch_type = "Non-node") %>%
  bind_rows(fin_nodes,.) %>%
  left_join(.,all_patches %>%
              select(layer,geometry)) %>%
  ungroup() %>%
  select(-c(area_m2,patch_area,area_ha)) %>% 
  st_set_geometry(.,value='geometry') %>%
  st_transform(.,32618)

starting_site_data = selection_df %>%
  filter(Site %in% unique(all_sites$loc_name)) %>%
  group_by(Site,Lifestage) %>%
  filter(Date == min(Date)) %>%
  left_join(.,Collections,
            join_by(County,Site,Date)) %>%
  select(County,Date,Site,Target_genus,Target_species,
         Target_stage,Lifestage,tot_collected,
         ha,v1,coinf,und,tot_tested,`Target density`) %>%
  rename(Density_p_m = "Target density") %>%
  mutate(ha_perc = ha/tot_tested,
         v1_perc = v1/tot_tested)

poly_tick_agents = fin_all_patch %>%
  left_join(.,starting_site_data %>%
              rename(loc_county = "County",
                     loc_name = "Site"),
            join_by(loc_county,
                    loc_name)) %>%
  select(loc_county,loc_name,metric,gridrows,gridcols,hectare,
         gridrows_adjusted,gridcols_adjusted,Lifestage,Density_p_m,
         ha_perc,v1_perc) %>%
  filter(is.na(Lifestage)==F) %>%
  mutate(ha_perc = ifelse(is.na(ha_perc)==T,0,ha_perc),
         v1_perc = ifelse(is.na(v1_perc)==T,0,v1_perc),
         num_ticks_projected = ifelse(is.na(Density_p_m)==T,0,(hectare*10000)*Density_p_m))
  

write_sf(fin_all_patch,paste0(getwd(),'/Cached_data/fin_all_patch.shp'))
write_sf(poly_tick_agents,paste0(getwd(),'/Cached_data/poly_tick_agents.shp'))
# Estimate of 1 million deer in NYS:
# https://extapps.dec.ny.gov/docs/administration_pdf/deer2.pdf

# Halsey and Miller use 50 mice per hectare


# Need to assign number of eggs to start model to all patches based on apriori parameters

# Be sure to keep environment clean after each script