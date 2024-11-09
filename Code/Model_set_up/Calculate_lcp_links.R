if(use_cached_data==FALSE){
#####
# Roadway Data
#####
minor_roadways = c(7,8,9,17,18,19)
major_roadways = c(1,2,4,6,11,12,14,16)

for(i in 1:11){
  rwd=read_sf(paste0(getwd(),'/Data/Roadway_data/Roadway_data',i,".shp"))
  if(i==1){rwd2=rwd}
  if(i>1){rwd2 = rbind(rwd,rwd2)}
}

roadwaydat = rwd2 %>%
  mutate(Categorization = Ctgrztn) %>%
  st_transform(.,crs=st_crs(unq_WMUs))
remove(rwd,rwd2)
# roadwaydat = read_sf(paste0(getwd(),
#                             "/Data/Input_data/NYSDOT Functional Class 07_10_2023/NYSDOT_Functional_Class_07_10_2023.shp")) %>%
#   st_zm() %>%
#   mutate(Categorization = ifelse(FUNCTIONAL %in% minor_roadways,1,
#                                  ifelse(FUNCTIONAL %in% major_roadways,2,0))) %>%
#   st_transform(.,crs=st_crs(template))

road_vect = terra::vect(roadwaydat %>% filter(st_is_empty(.)==F))
road_pix  = terra::rasterize(x = road_vect,
                             y = LCcrop,
                             field = "Categorization")
#####
# Get nodes:
#####
nodes = st_centroid(fin_all_patch)

#####
# Process resistance raster:
#####
LC_forest = LCcrop
forest_values = c(41,42,43,51,52,71)
values(LC_forest)[!(values(LC_forest)%in%forest_values)] = 0
values(LC_forest)[values(LC_forest)%in%forest_values] = 1

LC_cropland = LCcrop
cropland_values = c(81,82)
values(LC_cropland)[!(values(LC_cropland)%in%cropland_values)] = 0
values(LC_cropland)[values(LC_cropland)%in%cropland_values] = 35

LC_wetland = LCcrop
wetland_values = c(90,95)
values(LC_wetland)[!(values(LC_wetland)%in%wetland_values)] = 0
values(LC_wetland)[values(LC_wetland)%in%wetland_values] = 100

LC_water = LCcrop
water_values = c(11)
values(LC_water)[!(values(LC_water)%in%wetland_values)] = 0
values(LC_water)[values(LC_water)%in%wetland_values] = 1000

LC_high_developed = LCcrop
high_developed_values = c(24)
values(LC_high_developed)[!(values(LC_high_developed)%in%high_developed_values)] = 0
values(LC_high_developed)[values(LC_high_developed)%in%high_developed_values] = 1000

LC_med_developed = LCcrop
med_developed_values = c(23)
values(LC_med_developed)[!(values(LC_med_developed)%in%med_developed_values)] = 0
values(LC_med_developed)[values(LC_med_developed)%in%med_developed_values] = 100

LC_low_developed = LCcrop
low_developed_values = c(21,22)
values(LC_low_developed)[!(values(LC_low_developed)%in%low_developed_values)] = 0
values(LC_low_developed)[values(LC_low_developed)%in%low_developed_values] = 27

Highways = road_pix
highway_values = c(2)
values(Highways)[!(values(Highways)%in%highway_values)] = 0
values(Highways)[values(Highways)%in%highway_values] = 533

minor_roads = road_pix
minor_road_values = c(1)
values(minor_roads)[!(values(minor_roads)%in%minor_road_values)] = 0
values(minor_roads)[values(minor_roads)%in%minor_road_values] = 100

Resistance_grid = sum(LC_forest,LC_cropland,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_wetland,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_water,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_high_developed,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_med_developed,na.rm=T)
Resistance_grid = sum(Resistance_grid,LC_low_developed,na.rm=T)
Resistance_grid = sum(Resistance_grid,Highways,na.rm=T)
Resistance_grid = sum(Resistance_grid,minor_roads,na.rm=T)
Resistance_grid[Resistance_grid==0]=NA
Resistance_grid=1/Resistance_grid # These are now conductances, the inverse was taken
Rgrid = raster::raster(Resistance_grid) 
raster::writeRaster(Rgrid,
                    paste0(getwd(),'/Cached_data/Resistance_grid.tiff'),
                    overwrite = TRUE)
#####
# Within distance comps
#####

myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

comps <- foreach::foreach(i = 1:nrow(fin_all_patch), .errorhandling = "remove", .combine = "rbind", .packages = c("sf", "terra")) %dopar% {
  comps1=t(st_is_within_distance(fin_all_patch[i,],fin_all_patch,dist=1675,sparse = F))
  comps1=which(comps1)
  dist_df = data.frame(row=rep(i,length(comps1)),
                       col=comps1)
  return(dist_df)
}

parallel::stopCluster(myCluster)

comps = comps %>%
  filter(row!=col)
comps2 = comps
fltr <- !duplicated(apply(comps, 1, function(x) paste0(sort(x), collapse = "")))
comps = comps[fltr, ]

write.csv(comps,paste0(getwd(),'/Cached_data/comps.csv'))
}
#####
# Calculate least-cost-paths
#####

myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

lcp_network <- foreach::foreach(i = 1:nrow(comps), .errorhandling = "remove", .combine = "rbind", .packages = c("sf","raster","gdistance","tmaptools","dplyr","leastcostpath","terra")) %dopar% {
  
  bbdf <- sf::st_bbox(all_sites[c(comps[i,1],comps[i,2]),]) %>%
    tmaptools::bb_poly(.,projection = sf::st_crs(all_sites)) %>%
    sf::st_as_sf() %>%
    st_buffer(.,dist=1000)
  
  tr1=leastcostpath::create_cs(terra::crop(x=Rgrid %>%
                                             terra::rast(),
                                           y = bbdf,
                                           mask=T)) 
  
  lcp <- leastcostpath::create_lcp(x = tr1,
                                   origin = nodes[comps[i,1],,drop=FALSE],
                                   destination = nodes[comps[i,2],, drop=FALSE]) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(length = sf::st_length(.),
                  origin_ID = all_sites[comps[i,1],]$layer,
                  destination_ID =all_sites[comps[i,2],]$layer) 
  
  return(lcp)
}

parallel::stopCluster(myCluster)

attributes(lcp_network$length) <- NULL
lcp_network=lcp_network[,-c(1:3)]

write_sf(lcp_network,paste0(getwd(),'/Cached_data/lcp_network.shp'))
