# Change this to patches when ready to run full model and convert to the HPC cluster
WMUS = read_sf(paste0(getwd(),'/Data/WMUs/Wildlife_Management_Units.shp')) %>%
  st_transform(.,crs=32618) %>%
  mutate(area = st_area(.)) %>%
  dplyr::select(UNIT,area)
attributes(WMUS$area)=NULL

Deer_data <- read_excel(paste0(getwd(),"/Data/Deer_data.xlsx")) %>%
  filter(huntyear %in% c(2006,2007,2008)) %>%
  group_by(wmu) %>%
  summarize(mean_total = mean(Total,na.rm=T))
Deer_data$total_kill = sum(Deer_data$mean_total)

WMU_int = st_intersection(WMUS,st_difference(fin_poly)) %>%
  rename(wmu_area = 'area',
         patch_area = 'area.1')

fin_poly = poly2
fin_poly2 = fin_poly %>%
  filter(area>mean(fin_poly$area))
fin_poly3 = fin_poly %>%
  filter(area<=mean(area,na.rm=T)&
           area>median(area,na.rm=T))
fin_poly4 = fin_poly3[sample(nrow(fin_poly3),nrow(fin_poly3)*.05),]
fin_poly = rbind(fin_poly2,fin_poly4)
remove(fin_poly2,fin_poly3,fin_poly4)


fin_poly = left_join(fin_poly %>% st_drop_geometry() %>%
                       rename(patch_area = 'area'),
                     WMU_int %>% st_drop_geometry(),
                     by = join_by('layer','patch_area')) %>%
  dplyr::select(layer,patch_area,wmu_area,UNIT) %>%
  rename(wmu = 'UNIT') %>%
  left_join(.,Deer_data) %>%
  mutate(gridrows = round(sqrt(patch_area)),
         gridcols = round(sqrt(patch_area)))
fin_poly = fin_poly %>%
  left_join(.,fin_poly %>%
              group_by(wmu) %>%
              summarize(total_patches_area = sum(patch_area,na.rm=T))) %>%
  mutate(patch_percent = patch_area/total_patches_area,
         estimated_wmu_deer_killed = mean_total*patch_percent,
         deer_agents = round(estimated_wmu_deer_killed/(total_kill/1000000)))
# Check why there are NA values in fin_poly

# Estimate of 1 million deer in NYS:
# https://extapps.dec.ny.gov/docs/administration_pdf/deer2.pdf


# Need to calculate number of mice based on uniform distribution of mouse density.

# Need to assign number of eggs to start model to all patches based on apriori parameters

# Be sure to keep environment clean after each script