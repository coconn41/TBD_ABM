# Change this to patches when ready to run full model and convert to the HPC cluster

Deer_data <- read_excel(paste0(getwd(),"/Data/Deer_data.xlsx")) %>%
  filter(huntyear %in% c(2006,2007,2008)) %>%
  group_by(wmu) %>%
  summarize(mean_total = mean(Total,na.rm=T))
Deer_data$total_kill = sum(Deer_data$mean_total)
fin_poly = ptch2
fin_poly = fin_poly %>% 
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
         deer_agents = round(((1/metric)*estimated_wmu_deer_killed)/(total_kill/1000000)),
         hectare = patch_area*.0001) %>%
  group_by(Location_ID,loc_county,loc_name,layer,metric,area,
           patch_area,gridrows,gridcols,hectare) %>%
  summarize(deer_agents = sum(deer_agents,na.rm = T)) %>% 
  mutate(deer_p_ha = deer_agents/hectare,
         mouse_agents = rtruncnorm(n=1,a=0,mean = hectare*50,sd = 100),
         mice_p_ha = mouse_agents/hectare) %>%
  mutate(gridrows_adjusted = 1000,
         gridcols_adjusted = 1000,
         adjusted_ratio = gridrows/gridrows_adjusted,
         deer_agents_adjusted = deer_p_ha*(hectare/adjusted_ratio),
         mouse_agents_adjusted = rtruncnorm(n=1,a=0,mean = 100*50,sd = 100))

starting_site_data = selection_df %>%
  filter(Site %in% unique(deer_agents$Site)) %>%
  group_by(Site,Lifestage) %>%
  filter(Date == min(Date)) %>%
  left_join(.,Collections,
            join_by(County,Site,Date)) %>%
  select(County,Date,Site,Target_genus,Target_species,
         Target_stage,Lifestage,tot_collected,
         ha,v1,coinf,und,tot_tested,`Target density`) %>%
  rename(Density_p_m = "Target density") %>%
  mutate(num_ticks_projected = ifelse(is.na(Density_p_m)==T,
                                       tot_collected,Density_p_m*1000),
         ha_perc = ha/tot_tested,
         v1_perc = v1/tot_tested)

poly_tick_agents = fin_poly %>%
  left_join(.,starting_site_data %>%
              rename(loc_county = "County",
                     loc_name = "Site"),
            join_by(loc_county,
                    loc_name)) %>%
  select(loc_county,loc_name,metric,gridrows,gridcols,hectare,
         gridrows_adjusted,gridcols_adjusted,adjusted_ratio,Lifestage,
         ha_perc,v1_perc,num_ticks_projected) %>%
  filter(is.na(Lifestage)==F) %>%
  mutate(ha_perc = ifelse(is.na(ha_perc)==T,0,ha_perc),
         v1_perc = ifelse(is.na(v1_perc)==T,0,v1_perc),
         num_ticks_projected = ifelse(is.na(num_ticks_projected)==T,0,num_ticks_projected))
  

write.csv(fin_poly,paste0(getwd(),'/Cached_data/fin_poly.csv'))
write.csv(poly_tick_agents,paste0(getwd(),'/Cached_data/poly_tick_agents.csv'))
# Estimate of 1 million deer in NYS:
# https://extapps.dec.ny.gov/docs/administration_pdf/deer2.pdf

# Halsey and Miller use 50 mice per hectare


# Need to assign number of eggs to start model to all patches based on apriori parameters

# Be sure to keep environment clean after each script