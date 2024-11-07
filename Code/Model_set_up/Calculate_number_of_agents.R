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
            join_by(layer),
            relationship="many-to-many") %>%
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
  group_by(Location_ID,loc_county,loc_name,layer,metric,area,Site_type,
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

write.csv(fin_poly,paste0(getwd(),'/Cached_data/fin_poly.csv'))
# Estimate of 1 million deer in NYS:
# https://extapps.dec.ny.gov/docs/administration_pdf/deer2.pdf

# Halsey and Miller use 50 mice per hectare


# Need to assign number of eggs to start model to all patches based on apriori parameters

# Be sure to keep environment clean after each script