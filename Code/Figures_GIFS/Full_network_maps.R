#####
# Load libraries:
#####
library(tidyverse)
library(sf)
library(tmap)
library(terra)
rm(list=ls())
#####
# Load networks:
#####
networks = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
  rename(lcp_distance = "lcp_dst",
         origin_ID = "orgn_ID",
         destination_ID = "dstn_ID",
         distance = "distanc",
         inverse_sinuousity = "invrs_s",
         network_ID = "ntwr_ID") %>%
  filter(network_ID %in% c(1,2,4,5,6,7,8)) 
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

match_df = networks %>%
  st_drop_geometry() %>%
  group_by(network_ID,origin_ID) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  select(-tot) %>%
  rename(layer = origin_ID) %>%
  bind_rows(.,networks %>%
              st_drop_geometry() %>%
              group_by(network_ID,destination_ID) %>%
              summarize(tot = n()) %>%
              ungroup() %>%
              select(-tot) %>%
              rename(layer = destination_ID)) %>%
  group_by(network_ID,layer) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  select(-tot)

patches = left_join(reduced_patches,match_df) %>%
  mutate(network_ID = case_when(network_ID==4 ~ 3,
                                network_ID==5 ~ 4,
                                network_ID==6 ~ 5,
                                network_ID==7 ~ 6,
                                network_ID==8 ~ 7,
                                TRUE ~ network_ID),
         network_ID = factor(paste0("Network ",network_ID),
                             levels = c("Network 1",
                                        "Network 2",
                                        "Network 3",
                                        "Network 4",
                                        "Network 5",
                                        "Network 6",
                                        "Network 7"))) %>%
  filter(is.na(network_ID)==F)

networks = networks %>%
  mutate(network_ID = case_when(network_ID==4 ~ 3,
                                network_ID==5 ~ 4,
                                network_ID==6 ~ 5,
                                network_ID==7 ~ 6,
                                network_ID==8 ~ 7,
                                TRUE ~ network_ID),
         network_ID = factor(paste0("Network ",network_ID),
                             levels = c("Network 1",
                                        "Network 2",
                                        "Network 3",
                                        "Network 4",
                                        "Network 5",
                                        "Network 6",
                                        "Network 7")))

#####
# Make maps:
#####

m1=tm_shape(networks %>% filter(network_ID == "Network 1"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 1")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 1"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(.55,.2),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 1")

m2=tm_shape(networks %>% filter(network_ID == "Network 2"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 2")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 2"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(-.05,1),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 2")

m3=tm_shape(networks %>% filter(network_ID == "Network 3"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 3")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 3"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(.55,.2),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 3")

m4=tm_shape(networks %>% filter(network_ID == "Network 4"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 4")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 4"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(0,1),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 4")

m5=tm_shape(networks %>% filter(network_ID == "Network 5"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 5")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 5"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(0,1),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 5")

m6=tm_shape(networks %>% filter(network_ID == "Network 6"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 6")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 6"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(.55,.2),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 6")

m7=tm_shape(networks %>% filter(network_ID == "Network 7"),
            bbox = st_bbox(patches %>% filter(network_ID=="Network 7")))+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_shape(patches %>% filter(network_ID == "Network 7"))+
  tm_polygons(fill='#1B9E77')+
  tm_scalebar(position = c(0,0.2),
              text.size = 1,
              width = 7.5)+
  tm_layout(panel.labels = "Network 7")

legend<-tm_shape(networks %>% filter(network_ID=="Network 1"))+
  tm_polygons()+
  tm_add_legend(title = "Key",
                type = c('fill'),
                col = c('#1B9E77'),
                labels = c('Forest patches'))+
  tm_add_legend(type = 'line',
                col = 'black',
                labels = "Least-cost paths")+
  tm_layout(legend.only=T)

Figure_1 <- tmap_arrange(m1,m2,m3,m4,m5,m6,m7,legend,
                         ncol = 3)

tmap_save(Figure_1,
          filename = paste0(getwd(),'/Figures/Figures/Full_network_maps.jpeg'),
          dpi = 300,
          width = 10,
          height = 10)


