library(tidyverse)
library(sf)
library(tmap)
library(terra)
rm(list=ls())

#####
# Load data
#####
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

match_df = network1 %>%
  st_drop_geometry() %>%
  group_by(network_ID,origin_ID) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  select(-tot) %>%
  rename(layer = origin_ID) %>%
  bind_rows(.,network1 %>%
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
  mutate(network_ID = factor(paste0("Network ",network_ID),
                             levels = c("Network 1",
                                        "Network 2",
                                        "Network 3",
                                        "Network 4",
                                        "Network 5",
                                        "Network 6",
                                        "Network 7",
                                        "Network 8")))
network1 = network1 %>%
  mutate(network_ID = factor(paste0("Network ",network_ID),
                             levels = c("Network 1",
                                        "Network 2",
                                        "Network 3",
                                        "Network 4",
                                        "Network 5",
                                        "Network 6",
                                        "Network 7",
                                        "Network 8")))

#####
# Download forest cover data:
#####
# stateurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
# 
# if(file.exists(paste0(getwd(),"/Cached_data/cb_2018_us_state_500k.shp"))==F){
#   download.file(stateurl, destfile = file.path(paste0(getwd(),'/Cached_data'),
#                                                "States.zip"))
#   unzip(file.path(paste0(getwd(),"/Cached_data/States.zip")),
#         exdir=paste0(getwd(),'/Cached_data'))}
# NYS = read_sf(paste0(getwd(),"/Cached_data/cb_2018_us_state_500k.shp")) %>%
#   filter(NAME=="New York") %>%
#   st_transform(.,crs=32618)
# 
# LCr = rast(paste0(getwd(),'/Cached_data/NLCD_NLCD_Land_Cover_2019.tif'))
# LCproj = terra::project(LCr,crs(NYS))
# 
# LCcrop = terra::crop(x = LCproj,
#                      y = NYS |>
#                        terra::vect(),
#                      mask = T)

#####
# Generate map:
#####

m1=tm_shape(network1)+
  tm_lines(col = 'black',
           lwd = 1.5)+
  tm_facets(by='network_ID',ncol = 3)+
tm_shape(patches)+
  tm_polygons(col='#1B9E77')+
  tm_facets(by='network_ID',ncol = 3)+
tm_shape(patches %>% 
           filter(patch_type == "Node"))+
  tm_polygons(col='#D95F02')+
  tm_facets(by='network_ID',ncol = 3)+
tm_add_legend(title = "Key",
              type = c('fill'),
              col = c('#D95F02','#1B9E77'),
              labels = c('Forest patches starting simulation with ticks',
                         'Forest patches starting simulation without ticks'))+
  tm_add_legend(type = 'line',
                col = 'black',
                label = "Least-cost paths")+
  tm_layout(legend.outside.position = c('bottom'),
            panel.label.size = 1.5,
            legend.text.size = .75);m1
tmap_save(m1,
          filename = paste0(getwd(),'/Figures/Figures/Network_maps.jpeg'),
          dpi = 300,
          height=7.5,
          width= 5)

