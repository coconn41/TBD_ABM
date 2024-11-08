# Upon testing, will see if adjusted or full host agents will run due to size

deer_agents = fin_poly %>%
  filter(is.na(deer_agents_adjusted)==F) %>%
  uncount(round(deer_agents_adjusted)) %>%
  select(loc_county,loc_name,layer,metric,
         Site_type,gridrows_adjusted,gridcols_adjusted) %>%
  rename(County = "loc_county",
         Site = "loc_name") %>%
  mutate(row = round(runif(n=nrow(.),
                     min = 0,
                     max = gridrows_adjusted)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows_adjusted))) # use this to place the deer in their respective site
  
mouse_agents = fin_poly %>%
  filter(is.na(mouse_agents_adjusted)==F) %>%
  uncount(round(mouse_agents_adjusted)) %>%
  select(loc_county,loc_name,layer,metric,
         Site_type,gridrows_adjusted,gridcols_adjusted) %>%
  rename(County = "loc_county",
         Site = "loc_name")  %>%
  mutate(row = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows_adjusted)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows_adjusted)))



egg_sac_agents = data.frame(ID = NA,
                        age = NA,
                        num_eggs = NA,
                        row = NA,
                        col = NA)
  
larval_agents = data.frame(ID = NA,
                           age = NA,
                           sex = NA,
                           row = NA,
                           col = NA)
  
nymphal_agents = starting_site_data %>%
  uncount(round(num_nymphs_projected)) %>%
  mutate(ha_infected = ifelse(is.na(ha_perc)==T,0,
                              rbinom(n=1,size=1,prob=ha_perc)),
         v1_infected = ifelse(is.na(v1_perc)==T,0,
                              rbinom(n=1,size=1,prob=v1_perc))) %>%
  select(County,Site,Lifestage,ha_infected,v1_infected) %>%
  left_join(.,fin_poly %>%
              rename(County = "loc_county",
                     Site = "loc_name") %>%
              group_by(Site) %>%
              filter(deer_p_ha == min(deer_agents_adjusted)),#random var to dedup
            join_by(County,Site))
  
adult_agents
