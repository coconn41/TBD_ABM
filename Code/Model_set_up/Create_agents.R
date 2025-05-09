# Creating agents will reduce extra agents outside of the network. Previous
# steps calculated all patches in the entire intersected WMU

# Detect matches:
list_network = network1 %>%
  st_drop_geometry() %>% 
  pivot_longer(.,c(origin_ID,destination_ID)) %>%
  select(network_ID,value) %>%
  group_by(network_ID, value) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  group_by(value) %>%
  summarize(tot = n())
# patches are not duplicated across networks

network2 = network1 %>%
  st_drop_geometry() %>% 
  pivot_longer(.,c(origin_ID,destination_ID)) %>%
  select(network_ID,value) %>%
  group_by(network_ID, value) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  select(-tot) %>%
  rename(layer = "value")

deer_agents = fin_all_patch %>%
  filter(is.na(deer_agents)==F) %>%
  uncount(round(deer_agents)) %>%
  select(loc_county,loc_name,layer,metric,
         gridrows,gridcols) %>%
  rename(County = "loc_county",
         Site = "loc_name") %>%
  mutate(Agent_type = "Deer",
         row = round(runif(n=nrow(.),
                     min = 0,
                     max = gridrows)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridcols))) %>%
  left_join(.,network2) %>%
  filter(is.na(network_ID)==F)

reducer = deer_agents$layer

reduced_patches = fin_all_patch %>%
  filter(layer %in% reducer)

sf::write_sf(reduced_patches,paste0(getwd(),'/Cached_data/Reduced_patches.shp'))

mouse_agents = reduced_patches %>%
  filter(is.na(mouse_agents)==F) %>%
  uncount(round(mouse_agents)) %>%
  select(loc_county,loc_name,layer,metric,
         gridrows,gridcols) %>%
  rename(County = "loc_county",
         Site = "loc_name")  %>%
  mutate(Agent_type = "Mouse",
         row = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridcols))) %>%
  left_join(.,network2) %>%
  filter(is.na(network_ID)==F)

Host_agents = rbind(deer_agents, mouse_agents) %>%
  mutate(Agent_ID = 1:nrow(.))

write.csv(Host_agents,paste0(getwd(),'/Cached_data/Host_agents.csv'))

egg_sac_agents = data.frame(ID = NA,
                        age = NA,
                        num_eggs = NA,
                        row = NA,
                        col = NA,
                        gridrow = NA,
                        gridcol = NA)
  
larval_agents = data.frame(ID = NA,
                           age = NA,
                           sex = NA,
                           row = NA,
                           col = NA,
                           gridrow = NA,
                           gridcol = NA)
  
nymphal_agents = poly_tick_agents %>% 
  filter(Lifestage == "Nymph") %>%
  uncount(round(num_ticks_projected)) %>%
  group_by(loc_name) %>%
  mutate(ha_infected = ifelse(is.na(ha_perc)==T,0,
                              rbinom(n=n(),size=1,prob=ha_perc))) %>%
  mutate(v1_infected = ifelse(is.na(v1_perc)==T,0,
                              ifelse(ha_infected==1,0,
                              rbinom(n=n(),size=1,prob=v1_perc)))) %>%
  ungroup() %>%
  mutate(Agent_type = "Nymph",
         row = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridcols)),
         links = 0,
         time_on_host = 0,
         fed = 0,
         mated = 0,
         sex = sample(c('male','female'),nrow(.),replace=T),
         time_since_mating = 0) %>%
  select(loc_county,loc_name,Lifestage,ha_infected,v1_infected,
         Agent_type,row,col,links,time_on_host,fed,
         mated,sex,time_since_mating) %>%
  left_join(.,fin_all_patch %>%
              filter(is.na(loc_name)==F) %>%
              st_drop_geometry(),
            join_by(loc_county,loc_name)) %>%
  left_join(.,network2)

adult_agents = poly_tick_agents %>% 
  filter(Lifestage == "Adult") %>%
  uncount(round(num_ticks_projected)) %>%
  group_by(loc_name) %>%
  mutate(ha_infected = ifelse(is.na(ha_perc)==T,0,
                              rbinom(n=n(),size=1,prob=ha_perc))) %>%
  mutate(v1_infected = ifelse(is.na(v1_perc)==T,0,
                              ifelse(ha_infected==1,0,
                                     rbinom(n=n(),size=1,prob=v1_perc)))) %>%
  ungroup() %>%
  mutate(Agent_type = "Adult",
         row = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridcols)),
         links = 0,
         time_on_host = 0,
         fed = 0,
         mated = 0,
         sex = sample(c('male','female'),nrow(.),replace=T),
         time_since_mating = 0) %>%
  select(loc_county,loc_name,Lifestage,ha_infected,v1_infected,
         Agent_type,row,col,links,time_on_host,fed,
         mated,sex,time_since_mating) %>%
  left_join(.,fin_all_patch %>%
              filter(is.na(loc_name)==F) %>%
              st_drop_geometry(),
            join_by(loc_county,loc_name)) %>%
  left_join(.,network2)

Tick_agents = rbind(nymphal_agents,adult_agents) %>%
  mutate(Agent_ID = seq(from = max(Host_agents$Agent_ID)+1,
                        length.out = nrow(.)),
         Infection_status = ifelse(ha_infected==1,"ha",
                                   ifelse(v1_infected==1,"v1","None"))) %>%
  select(loc_county,loc_name,layer,metric,
         gridrows,gridcols,Agent_type,row,col,network_ID,
         Agent_ID,Lifestage,Infection_status,links,time_on_host,fed,
         mated,sex,time_since_mating) %>%
  rename(County = "loc_county",
         Site = "loc_name")

write.csv(Tick_agents,paste0(getwd(),'/Cached_data/Tick_agents.csv'))
         