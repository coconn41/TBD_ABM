patches = read_sf(paste0(getwd(),'/Data/Metric_2024-06-07.shp'))
patches$area = st_area(patches)
attributes(patches$area)=NULL
patches = patches %>%
  filter(layer %in% Loc_metric_table_w_private$layer) %>%
  left_join(.,Loc_metric_table_w_private %>%
              st_drop_geometry(),
            join_by(layer,metric)) %>%
  rename(area_m2 = 'area.x',
         area_ha = 'area.y')
for(i in 1:nrow(patches)){
  print(i/nrow(patches)*100)
  dist_1=st_is_within_distance(patches[i,],patches,dist=1675)
  df = data.frame(row.id = rep(i,length(dist_1[[1]])),
                  col.id = unlist(dist_1))
  if(i==1){dist_2 = st_is_within_distance(patches[i,],patches,dist=1675)
  df2 = data.frame(row.id = rep(i,length(dist_1[[1]])),
                  col.id = unlist(dist_1))}
  if(i>1){df2 = rbind(df2,df)}
}

locs_within_distance = df2
  group_by(row.id) %>%
  summarize(tot = n()) %>%
  filter(tot>1) %>%
  mutate(Site = Loc_metric_table_w_private[row.id,]$loc_name)

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
  arrange(desc(final_rank))

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
  arrange(desc(final_rank))

sites = c("Albany Pine Bush Preserve - Siver Road", # connectivity = .30397980
                   "Clermont") # Connectivity = .65395957
rm(ha_rankings,v1_rankings,regs,possible_locs,locs_within_distance)