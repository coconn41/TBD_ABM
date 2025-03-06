# Debugging load:
load(paste0(getwd(),'/Debugging/Network_6/net_6_timestep_15000.RData'))
# Burn in load:
#load(paste0(getwd(),'/Simulations/Network_6/timestep_61320_attach_25_path_trans_100.RData'))
library(tidyverse)
library(tmap)
library(gifski)
library(tmaptools)
library(sf)
#####
# Rename at factor:
#####
tick_data2 = tick_data2 %>%
  filter(Lifestage!="") %>%
  mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
                               Lifestage=="Adult" ~ "Adults",
                               TRUE ~ Lifestage),
         Lifestage = factor(Lifestage,
                            levels = c("Eggs","Larvae","Nymphs","Adults")),
         simulation_day = (day_of_year+(year*365))-264,
         simulation_week = ceiling(simulation_day/7))
deer_data2 = deer_data2 %>%
  filter(season!="")
mouse_data2 = mouse_data2 %>%
  filter(season!="")
#####
# Calculate Ha percentage in mice:
#####
ggplot(data = mouse_data2 %>% 
         group_by(timestep) %>% 
         summarize(ha_perc = (sum(tot_ha_infected)/sum(tot_mice))*100), #%>%
         # mutate(Ha_perc = ifelse(Ha_perc == NaN,0,
         #                         ifelse(is.na(Ha_perc)==T,0,Ha_perc))),
       aes(x=timestep,y=ha_perc))+
         geom_line()
#####
# Calculate V1 percentage in deer:
#####
ggplot(data = deer_data2 %>%
         group_by(timestep) %>% 
         summarize(v1_perc = (sum(tot_v1_infected)/sum(tot_deer))*100),
       aes(x=timestep,y=v1_perc))+
  geom_line()

#####
# Calculate pathogen prevalence in ticks
#####
ggplot(data = tick_data2 %>% 
         # filter(Lifestage=="Larvae"|
         #          Lifestage=="Nymphs") %>%
         group_by(timestep,Lifestage) %>%
         summarize(total_ticks = sum(total_ticks,na.rm=T),
                   v1_perc = sum(tot_v1)/sum(total_ticks)*100,
                   ha_perc = sum(tot_ha)/sum(total_ticks)*100) %>%
         pivot_longer(c(v1_perc,ha_perc)),
       aes(x=timestep,y=value,color=name))+
  geom_line()+
  coord_cartesian(ylim = c(0,100))+
  facet_wrap(.~Lifestage,scales='free_y')#+
  geom_vline(xintercept = c(0,2160,4296,6504,8760,
                            (2160+8760),(4296+8760),(6504+8760),(8760*2),
                            (2160+(8760*2)),(4296+(8760*2)),(6504+(8760*2)),(8760*3)))
                           # (2160+(8760*3)),(4296+(8760*3)),(6504+(8760*3)),(8760*4),
                      #      (2160+(8760*4)),(4296+(8760*4)),(6504+(8760*4)),(8760*5)))

#####
# Calculate tick populations:
#####
ggplot(data = tick_data2 %>%group_by(timestep,Lifestage) %>% summarize(total_ticks = sum(total_ticks,na.rm=T)),
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
  group_by(Lifestage,layer,simulation_week,year,season,simulation_day) %>%
  summarize(tot_ha = sum(tot_ha,na.rm=T),
            tot_v1 = sum(tot_v1,na.rm=T),
            total_ticks = sum(total_ticks,na.rm=T)) %>%
  mutate(ha_perc = (tot_ha/total_ticks)*100,
         v1_perc = (tot_v1/total_ticks)*100)

td_join_dat = expand.grid(Lifestage = c("Eggs","Larvae","Nymphs","Adults"),
                 layer = unique(td_join_dat$layer),
                 simulation_day = min(td_join_dat$simulation_day):max(td_join_dat$simulation_day)) %>%
                 #simulation_week = min(td_join_dat$simulation_week):max(td_join_dat$simulation_week)) %>%
  mutate(total_ticks = 0,
         tot_v1 = 0,
         tot_ha = 0) %>% 
  rbind(.,td_join_dat %>%
          ungroup() %>%
          select(c(Lifestage,layer,simulation_day,total_ticks,tot_v1,tot_ha))) %>% 
  left_join(.,td_join_dat %>%
              ungroup() %>%
              select(simulation_day,season,year) %>%
              distinct(),by='simulation_day') %>%
  mutate(simulation_week = ceiling(simulation_day/7)) %>%
  group_by(Lifestage,layer,simulation_week,year,season) %>%
  summarize(tot_ha = sum(tot_ha,na.rm=T),
            tot_v1 = sum(tot_v1,na.rm=T),
            total_ticks = sum(total_ticks,na.rm=T),
            v1_perc = ifelse(total_ticks!=0,(tot_v1/total_ticks)*100,0),
            ha_perc = ifelse(total_ticks!=0,(tot_ha/total_ticks)*100,0))
  

spatial_join_dat = left_join(td_join_dat,tm_patches) %>%
  st_set_geometry(.,value='geometry') %>%
  mutate(tick_density = (total_ticks/(24*7))/(hectare*10000),
         ha_ERI = tick_density * (ha_perc/100),
         v1_ERI = tick_density * (v1_perc/100))

txt_df = spatial_join_dat %>%
  st_drop_geometry() %>%
  ungroup() %>%
  select(Lifestage,simulation_week,season,layer,year) %>%
  mutate(txt = paste0("Season = ",season,"\n year = ",year),
         lon = st_bbox(tm_patches)[3],
         lat = st_bbox(tm_patches)[2]) %>%
  st_as_sf(.,coords=c('lon','lat')) %>%
  st_set_crs(.,value=st_crs(tm_patches)) 
######
# GIF of v1 percentage in ticks
#####
m1=tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_borders()+
tm_shape(spatial_join_dat %>%
           filter(Lifestage != "Eggs"))+
  tm_polygons(col='v1_perc',
              title = "Ap-v1 (%)",
              breaks = c(0,.5,1,1.5,2,4,8,10,100),
              palette = get_brewer_pal("Blues",n=8))+
  tm_facets(by="Lifestage",
            nrow = 1,ncol=3,along='simulation_week')+
  tm_shape(txt_df %>%
             filter(Lifestage != "Eggs"))+
  tm_text(text = 'txt',
          size=.75,
          ymod=1,xmod=-2.5)+
  tm_facets(by="Lifestage",
            nrow = 1,ncol=3,along='simulation_week')

tmap_animation(tm = m1,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_v1.gif'),
               fps = 7)
#####
# Ha percentage in ticks GIF
#####
m2=tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
  tm_shape(spatial_join_dat %>%
             filter(Lifestage!="Eggs"))+
  tm_polygons(col='ha_perc',
              title = "Ap-ha (%)",
              palette = get_brewer_pal("Reds",n=8),
              breaks = c(0,.5,1,1.5,2,4,8,10,100))+
  tm_facets(by = "Lifestage", nrow = 1, ncol = 3, along='simulation_week')+
  tm_shape(txt_df %>%
             filter(Lifestage != "Eggs"))+
  tm_text(text = 'txt',
          size=.75,
          ymod=1,xmod=-2.5)+
  tm_facets(by="Lifestage",
            nrow = 1,ncol=3,along='simulation_week')

tmap_animation(tm = m2,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_ha.gif'),
               fps = 7)
#####
# Tick density GIF
#####
m3=tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
  tm_shape(spatial_join_dat)+
  tm_polygons(col='tick_density',
              title = "Tick density",
              palette = viridisLite::viridis(n=10,option='plasma'),
              breaks = c(0,0.001,0.0025,0.005,0.01,0.015,0.02,0.025,0.04,.2,100))+
  tm_facets(by = "Lifestage",
            nrow = 1,
            ncol = 4,
            along='simulation_week')+
  tm_shape(txt_df)+
  tm_text(text = 'txt',
          size=.75,
          ymod=1,xmod=-2.5)+
  tm_facets(by="Lifestage",
            nrow = 1,ncol=4,along='simulation_week')

  tmap_animation(tm = m3,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_ticks.gif'),
               fps = 7)
  
m4 = tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
  tm_shape(spatial_join_dat %>% filter(Lifestage!="Eggs"))+
  tm_polygons(col='ha_ERI',
              title = "Ap-ha ERI",
              palette = get_brewer_pal("Reds",n=7),
              breaks=c(0,.00005,.0001,.00015,.0002,.0004,.002,.02))+
  tm_facets(by = "Lifestage",
            nrow = 1,
            ncol = 3,
            along='simulation_week')+
  tm_shape(txt_df %>% filter(Lifestage!="Eggs"))+
  tm_text(text = 'txt',
          size=.75,
          ymod=1,xmod=-2.5)+
  tm_facets(by="Lifestage",
            nrow = 1,ncol=3,along='simulation_week')

tmap_animation(tm = m4,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_Ha_ERI.gif'),
               fps = 7)

m5 = tm_shape(tm_patches,bbox=st_bbox(tm_patches))+
  tm_polygons()+
  tm_shape(spatial_join_dat %>% filter(Lifestage!="Eggs"))+
  tm_polygons(col='v1_ERI',
              title = "Ap-v1 ERI",
              palette = get_brewer_pal("Blues",n=9),
              breaks=c(0,.0005,.001,.0015,.002,.003,.004,.005,.02,5))+
  tm_facets(by = "Lifestage",
            nrow = 1,
            ncol = 3,
            along='simulation_week')+
  tm_shape(txt_df%>% filter(Lifestage!="Eggs"))+
  tm_text(text = 'txt',
          size=.75,
          ymod=1,xmod=-2.5)+
  tm_facets(by="Lifestage",
            nrow = 1,ncol=3,along='simulation_week')

tmap_animation(tm = m5,
               filename = paste0(getwd(),'/Figures/Animations/Network_6_v1_ERI.gif'),
               fps = 7)
