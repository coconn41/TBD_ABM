tdir=tempdir()
stateurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
countyurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip"

if(file.exists(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))==F){
  download.file(countyurl,destfile = file.path(tdir,"Counties.zip"))
  unzip(file.path(tdir,"Counties.zip"),exdir=tdir)}
USA_Counties=read_sf(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))
Erie = USA_Counties %>%
  filter(STATEFP=="36") %>%
  filter(NAME == "Erie") %>%
  st_transform(.,crs=32618)

if(file.exists(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))==F){
  download.file(stateurl, destfile = file.path(tdir, "States.zip"))
  unzip(file.path(tdir,"States.zip"),exdir=tdir)}
NYS = read_sf(paste(tdir,"/cb_2018_us_state_500k.shp",sep="")) %>%
  filter(NAME=="New York") %>%
  st_transform(.,crs=32618)

LC = get_nlcd(template=Erie,
              label="NLCD",
              dataset='landcover',
              year=2019,
              landmass = 'L48',
              force.redo = T,
              extraction.dir = tdir)
LCr = rast(LC)
LCproj = terra::project(LCr,crs(Erie))

LCcrop = terra::crop(x = LCproj,
                     y = Erie |>
                       terra::vect(),
                     mask = T)

LC_forest_patches = LCcrop
values(LC_forest_patches)[values(LC_forest_patches)==42] = 41
values(LC_forest_patches)[values(LC_forest_patches)==43] = 41
values(LC_forest_patches)[values(LC_forest_patches)!=41] = NA

y = get_patches(LC_forest_patches,directions=4)
poly1 = as.polygons(terra::rast(y$layer_1$class_41))
poly2 = st_as_sf(poly1)
poly2$area = st_area(poly2) # Area is in m^2 by default
#poly2$area = set_units(poly2$area,ha)
attributes(poly2$area)=NULL

fin_poly = poly2
fin_poly2 = fin_poly %>%
  filter(area>mean(fin_poly$area))
fin_poly3 = fin_poly %>%
  filter(area<=mean(area,na.rm=T)&
           area>median(area,na.rm=T))
fin_poly4 = fin_poly3[sample(nrow(fin_poly3),nrow(fin_poly3)*.05),]
fin_poly = rbind(fin_poly2,fin_poly4)
remove(fin_poly2,fin_poly3,fin_poly4)
