# tdir=tempdir()
# stateurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
# 
# if(file.exists(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))==F){
#   download.file(stateurl, destfile = file.path(tdir, "States.zip"))
#   unzip(file.path(tdir,"States.zip"),exdir=tdir)}
# NYS = read_sf(paste(tdir,"/cb_2018_us_state_500k.shp",sep="")) %>%
#   filter(NAME=="New York") %>%
#   st_transform(.,crs=32618)

WMUS = read_sf(paste0(getwd(),'/Data/WMUs/Wildlife_Management_Units.shp')) %>%
  st_transform(.,crs=32618) %>%
  mutate(area = st_area(.)) %>%
  dplyr::select(UNIT,area)
attributes(WMUS$area)=NULL

WMU_int = st_intersection(WMUS,st_difference(all_sites)) %>%
  rename(wmu_area = 'area',
         patch_area = 'area.1')
  
unq_WMUs = WMUS %>%
  filter(UNIT %in% WMU_int$UNIT) # Double check name of UNIT

for(i in 1:nrow(unq_WMUs)){
  ptch = st_intersection(all_patches,unq_WMUs[i,])
  if(i==1){ptch2 = ptch}
  if(i>1){ptch2 = rbind(ptch2,ptch)}
}

fin_poly = ptch2