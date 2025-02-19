load(paste0(getwd(),'/'))
source(paste0(getwd(),"/Code/Model_set_up/Load_libraries.R"))
library(tidyverse)
library(tmap)
library(gifski)

#####
# Calculate Ha percentage in mice:
#####
ggplot(data = mouse_data2 %>% filter(Ha_perc>0) %>% mutate(tot_mice = tot_ha_infected/Ha_perc) %>%
         group_by(timestep) %>% summarize(Ha_perc = sum(tot_ha_infected)/(tot_mice)), #%>%
         # mutate(Ha_perc = ifelse(Ha_perc == NaN,0,
         #                         ifelse(is.na(Ha_perc)==T,0,Ha_perc))),
       aes(x=timestep,y=Ha_perc))+
         geom_line()
#####
# Calculate V1 percentage in deer:
#####
ggplot(data = deer_data2 %>% filter(V1_perc>0) %>% mutate(tot_deer = tot_v1_infected/V1_perc) %>%
         group_by(timestep) %>% summarize(V1_perc = sum(tot_v1_infected)/sum(tot_deer)),
       aes(x=timestep,y=V1_perc))+
  geom_line()

#####
# Calculate pathogen prevalence in ticks
#####
ggplot(data = tick_data2 %>% 
         mutate(tot_v1_inf = total_ticks*(v1_perc/100),
                tot_ha_inf = total_ticks*(Ha_perc/100)) %>%
         group_by(timestep,Lifestage) %>%
         summarize(total_ticks = sum(total_ticks,na.rm=T),
                   v1_perc = sum(tot_v1_inf)/sum(total_ticks)*100,
                   Ha_perc = sum(tot_ha_inf)/sum(total_ticks)*100) %>%
         pivot_longer(c(v1_perc,Ha_perc)),
       aes(x=timestep,y=value,color=name))+
  geom_line()+
  facet_wrap(.~Lifestage,scales='free_y')
#####
# Calculate tick populations:
#####
ggplot(data = tick_data2 %>% group_by(timestep,Lifestage) %>% summarize(total_ticks = sum(total_ticks,na.rm=T)),
       aes(x=timestep,y=total_ticks))+
  geom_line()+
  facet_wrap(.~Lifestage,scales='free_y',ncol=1)+
  geom_vline(xintercept = c(0,2160,4296,6504,8760,
                            (2160+8760),(4296+8760),(6504+8760),(8760*2),
                            (2160+(8760*2)),(4296+(8760*2)),(6504+(8760*2)),(8760*3),
                            (2160+(8760*3)),(4296+(8760*3)),(6504+(8760*3)),(8760*4),
                            (2160+(8760*4)),(4296+(8760*4)),(6504+(8760*4)),(8760*5)))
#####
# Data cleaning for maps and gifs:
#####
tm_patches = read_sf(paste0(getwd(),'/Cached_data/Reduced_patches.shp')) %>%
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
  filter(layer %in% reduced_patches$layer) %>%
  filter(!duplicated(layer))

td_join_dat = tick_data2 %>%
  filter(Lifestage=="Adult") %>%
  mutate(tot_ha_pos = round(total_ticks*(Ha_perc/100)),
         tot_v1_pos = round(total_ticks*(v1_perc/100))) %>%
  group_by(layer,year,day_of_year) %>%
  summarize(tot_ha_pos = sum(tot_ha_pos,na.rm=T),
            tot_v1_pos = sum(tot_v1_pos,na.rm=T),
            total_ticks = round(mean(total_ticks,na.rm=T))) %>%
  mutate(simulation_day = (day_of_year+(year*365))-264,
         Ha_perc = tot_ha_pos/total_ticks,
         v1_perc = tot_v1_pos/total_ticks)

spatial_join_dat = left_join(td_join_dat,tm_patches) %>%
  st_set_geometry(.,value='geometry') %>%
  mutate(tick_density = total_ticks/(hectare*10000))
######
# GIF of v1 percentage in ticks
#####
m1=tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
tm_shape(spatial_join_dat)+
  tm_polygons(col='v1_perc',
              breaks = c(0,.5,1,1.5,2,4,8,10,100))+#Ha_perc
  tm_facets(along='simulation_day')

tmap_animation(tm = m1,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_v1.gif'),
               fps = 25)
#####
# Ha percentage in ticks GIF
#####
m2=tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
  tm_shape(spatial_join_dat)+
  tm_polygons(col='Ha_perc',
              breaks = c(0,.5,1,1.5,2,4,8,10,100))+
  tm_facets(along='simulation_day')

tmap_animation(tm = m2,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_ha.gif'),
               fps = 25)
#####
# Tick density GIF
#####
m3=tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
  tm_shape(spatial_join_dat)+
  tm_polygons(col='tick_density',
              breaks = c(0,0.001,0.0025,0.005,0.01,0.015,0.02,0.025,0.04,.2))+
  tm_facets(along='simulation_day')

tmap_animation(tm = m3,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_ticks.gif'),
               fps = 25)
