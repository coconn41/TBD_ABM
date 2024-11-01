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

locs_within_distance = df2 %>%
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

site_df = data.frame(Site = c("Burnt-Rossman State Forest- Westkill Road Trail",
                              "Louisa Pond",
                              "Garnsey Park",
                              "Allegany State Park",
                              "Gargoyle Park",
                              "Green Lakes State Park",
                              NA,
                              "Mohansic Golf Course",
                              "Martin Van Buren"),
                     Connectivity = rep(c("High","Medium","Low"),3),
                     ha_relationship = c(rep("Positive",6),
                                         rep("Negative",3)),
                     v1_relationship = c(rep("Negative",3),
                                         rep("Positive",6)),
                     Predict = c("Yes","Maybe","No","Maybe","Yes","Maybe","No","Maybe","Yes"))
#site_df2 = site_df %>% filter(Predict %in% c("Yes","Maybe"))

selected_sites = Loc_metric_table_w_private %>%
  filter(loc_name %in% site_df$Site) %>%
  mutate(Site_type = "Node")

adjacent_sites = df2 %>%
  mutate(node_site = Loc_metric_table_w_private[row.id,]$loc_name,
         adj_site = Loc_metric_table_w_private[col.id,]$loc_name) %>%
  filter(node_site %in% site_df$Site,
         node_site!=adj_site)

adjacent_sites = Loc_metric_table_w_private %>%
  filter(loc_name %in% adjacent_sites$adj_site) %>%
  mutate(Site_type = "Adjacent")

all_sites = rbind(selected_sites,adjacent_sites)

rm(ha_rankings,v1_rankings,regs,possible_locs,locs_within_distance)