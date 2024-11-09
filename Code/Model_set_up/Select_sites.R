all_patches = read_sf(paste0(getwd(),'/Data/Metric_2024-06-07.shp'))
all_patches$area = st_area(all_patches)
attributes(all_patches$area)=NULL
patches = all_patches %>%
  filter(layer %in% Loc_metric_table_w_private$layer) %>%
  left_join(.,Loc_metric_table_w_private %>%
              st_drop_geometry(),
            join_by(layer,metric)) %>%
  rename(area_m2 = 'area.x',
         area_ha = 'area.y')

myCluster <- parallel::makeCluster(cores)
doParallel::registerDoParallel(myCluster)

df2 = foreach::foreach(i = 1:nrow(patches),
                       .errorhandling = "remove",
                       .combine = "rbind",
                       .packages = c("sf")) %dopar% {
                         
                         dist=st_is_within_distance(patches[i,],patches,dist=1675)
                         df = data.frame(row.id = rep(i,length(dist[[1]])),
                                         col.id = unlist(dist))
                         return(df)
                       }

parallel::stopCluster(myCluster)

locs_within_distance = df2 %>%
  group_by(row.id) %>%
  summarize(tot = n()) %>%
  filter(tot>1) %>%
  mutate(Site = patches[row.id,]$loc_name)

possible_locs = selection_df %>%
  filter(Site %in% locs_within_distance$Site) %>%
  mutate(Year = as.numeric(substring(Date,1,4))) %>%
  pivot_longer(.,c(ha,v1)) %>%
  left_join(locs_within_distance) %>%
  select(-row.id) %>%
  rename(tot_adjacent_sites = 'tot') %>%
  left_join(.,Loc_metric_table_w_private,
            join_by(Site==loc_name,
                    County==loc_county))

regs = possible_locs %>%
  filter(is.na(value)==F) %>%
  group_by(Site,name) %>%
  do(model = lm(value~Year,data=.)) %>%
  mutate(coefficient = summary(model)$coefficients[2]) %>%
  dplyr::select(-model) %>%
  filter(coefficient != "NaN")

ha_rankings = possible_locs %>%
  filter(is.na(value==F),
         name=="ha") %>%
  group_by(Site,name) %>%
  summarize(tot = n(),
            minyear = min(Year),
            maxyear = max(Year)) %>%
  left_join(.,regs) %>%
  left_join(.,possible_locs) %>%
  filter(is.na(coefficient)==F) %>%
  mutate(yeardiff = maxyear-minyear) %>%
  ungroup() %>%
  group_by(Site) %>%
  summarize(tot_adjac = max(tot_adjacent_sites,na.rm=T),
            tot_visits = max(tot),
            yeardiff = max(yeardiff),
            betamax = max(coefficient),
            connmax = max(metric)) %>%
  mutate(Yearrank = rank(yeardiff),
         Betarank = rank(betamax),
         Adjacentrank = rank(tot_adjac),
         visitrank = rank(tot_visits),
         ranksum = Yearrank+Betarank+Adjacentrank+visitrank,
         final_rank = rank(ranksum)) %>%
  arrange(desc(final_rank)) %>%
  filter(tot_visits>=5)

v1_rankings = possible_locs %>%
  filter(is.na(value==F),
         name=="v1") %>%
  group_by(Site,name) %>%
  summarize(tot = n(),
            minyear = min(Year),
            maxyear = max(Year)) %>%
  left_join(.,regs) %>%
  left_join(.,possible_locs) %>%
  filter(is.na(coefficient)==F) %>%
  mutate(yeardiff = maxyear-minyear) %>%
  ungroup() %>%
  group_by(Site) %>%
  summarize(tot_adjac = max(tot_adjacent_sites,na.rm=T),
            tot_visits = max(tot),
            yeardiff = max(yeardiff),
            betamax = max(coefficient),
            connmax = max(metric)) %>%
  mutate(Yearrank = rank(yeardiff),
         Betarank = rank(betamax),
         Adjacentrank = rank(tot_adjac),
         visitrank = rank(tot_visits),
         ranksum = Yearrank+Betarank+Adjacentrank+visitrank,
         final_rank = rank(ranksum)) %>%
  arrange(desc(final_rank)) %>%
  filter(tot_visits>=5)

# combos: 
# hi pos neg Hangning Bog
# lo pos neg Dwaas Kill
# hi pos pos Allegany 
# lo pos pos Moss Lake
# hi neg pos Mohansic
# lo neg pos Veterans Memorial Park
# hi neg neg Pine Barrens
# lo neg neg Saratoga Spa State Park


site_df = data.frame(Site = c("Hanging Bog", # conn = .6249, ha = .11, v1 = -0.05
                              "Dwaas Kill Nature Preserve", #conn = .165, ha = .42, v1 = -0.26
                              "Allegany State Park", #conn = .85, ha = .12, v1 = .137
                              "Moss Lake", # conn = .417, ha = .426, v1 = .002
                              "Mohansic Golf Course", # conn = .49, ha = -2.42e, v1 = .012
                              "Veterans Memorial Park", # conn = .21, ha = -4e15, v1 = .07
                              "Pine Barrens Trails", # conn = .53, ha = -4, v1 =  -1.8
                              "Saratoga Spa State Park"), #conn = .176, ha = -3.74, v1 = -.005
                     Connectivity = rep(c("High","Low"),4),
                     ha_relationship = c(rep("Positive",4),
                                         rep("Negative",4)),
                     v1_relationship = c(rep("Negative",2),
                                         rep("Positive",4),
                                         rep("Negative",2)),
                     Predict = c("Yes","No","Maybe","Maybe","No","Yes","Maybe",))

selected_sites = Loc_metric_table_w_private %>%
  filter(loc_name %in% site_df$Site) %>%
  mutate(Site_type = "Node")

adjacent_sites = df2 %>%
  mutate(node_site = patches[row.id,]$loc_name,
         adj_site = patches[col.id,]$loc_name) %>%
  filter(node_site %in% site_df$Site)

all_sites = patches %>%
  filter(loc_name %in% unique(adjacent_sites$adj_site))

write.csv(all_sites,paste0(getwd(),'/Cached_data/all_sites.csv'))

rm(ha_rankings,v1_rankings,regs,possible_locs,locs_within_distance)