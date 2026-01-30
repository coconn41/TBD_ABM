######### Create agents simplified:
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
#####
# Create deer
#####
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
                           max = gridcols)),
         V1_infected = 0,
         V1_infection_timer = 0,
         Age = runif(n = n(),min=1,max=(11*24*365))) %>%
  left_join(.,network2) %>%
  filter(is.na(network_ID)==F) %>%
  filter(network_ID %in% net_select)
#####
# Remove duplicate layers in patches
#####
reducer = deer_agents$layer
reduced_patches = fin_all_patch %>%
  filter(layer %in% reducer)
#####
# Create mice
#####
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
                           max = gridcols)),
         Ha_infected = 0,
         Ha_infection_timer = 0,
         Age = runif(n = n(),min = 1,max = (2*24*365))) %>%
  left_join(.,network2) %>%
  filter(is.na(network_ID)==F)

#####
# Combine and save host agents
#####
Host_agents = rbind(deer_agents %>%
                      select(-c(V1_infected,V1_infection_timer)),
                    mouse_agents %>%
                      select(-c(Ha_infected,Ha_infection_timer))) %>%
  mutate(Agent_ID = 1:nrow(.))
mouse_agents = Host_agents %>% filter(Agent_type=="Mouse")
deer_agents = Host_agents %>% filter(Agent_type=="Deer")
write.csv(Host_agents,paste0(getwd(),'/Cached_data/Host_agents.csv'))

#####
# Create tick agents
#####
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

nymph_agents = reduced_patches %>%
  mutate(ticks_p_ha = round(hectare*tick_density)) %>%
  uncount(ticks_p_ha) %>%
  select(loc_county,loc_name,layer,metric,
         gridrows,gridcols) %>%
  rename(County = "loc_county",
         Site = "loc_name")  %>%
  mutate(Agent_type = "Nymph",
         row = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridcols))) %>%
  left_join(.,network2) %>%
  filter(is.na(network_ID)==F) %>%
  mutate(Agent_ID = c((max(Host_agents$Agent_ID)+1):(max(Host_agents$Agent_ID)+n())),
         Lifestage = "Nymph",
         time_on_host = 0,
         fed = 0,
         Infection_status = "None",
         ha_infected = 0,
         v1_infected = 0,
         links = 0,
         mated = 0,
         sex = sample(c('male','female'),nrow(.),replace=T),
         time_since_mating = 0,
         molt_death_immune = 0,
         tick_age_wks = 39,
         die = 0,
         linked_type = "N",
         num_ticks = 1,
         dropped = 0,
         time_since_fed = 0,
         attempted_pathogen_transfer = 0,
         molt = 0,
         att_prob = deer_attach_prob)# %>%
 # select(-c(County,Site,metric))
if(pathogens == F){nymph_agents <- nymph_agents %>%
  select(-c(Infection_status,ha_infected,v1_infected,attempted_pathogen_transfer))}
# nymph_agents <- nymph_agents %>%
#   rename(loc_name = "Site",
#          loc_county = "County") %>%
#   left_join(.,fin_all_patch %>%
#               filter(is.na(loc_name)==F) %>%
#               st_drop_geometry(),
#             join_by(loc_county,loc_name)) %>%
#   left_join(.,network2 %>%
#               filter(network_ID==net_select),
#             join_by(layer))



adult_agents = reduced_patches %>%
  mutate(ticks_p_ha = round(hectare*tick_density)) %>%
  uncount(ticks_p_ha) %>%
  select(loc_county,loc_name,layer,metric,
         gridrows,gridcols) %>%
  rename(County = "loc_county",
         Site = "loc_name")  %>%
  mutate(Agent_type = "Adult",
         row = round(runif(n=nrow(.),
                           min = 0,
                           max = gridrows)),
         col = round(runif(n=nrow(.),
                           min = 0,
                           max = gridcols))) %>%
  left_join(.,network2) %>%
  filter(is.na(network_ID)==F) %>%
  mutate(Agent_ID = c((max(nymph_agents$Agent_ID)+1):(max(nymph_agents$Agent_ID)+n())),
         Lifestage = "Adult",
         time_on_host = 0,
         fed = 0,
         Infection_status = "None",
         ha_infected = 0,
         v1_infected = 0,
         links = 0,
         mated = 0,
         sex = sample(c('male','female'),nrow(.),replace=T),
         time_since_mating = 0,
         tick_age_wks = 65,
         die = 0,
         molt_death_immune = 0,
         linked_type = "N",
         num_ticks = 1,
         dropped = 0,
         time_since_fed = 0,
         attempted_pathogen_transfer = 0,
         molt = 0,
         att_prob = deer_attach_prob)

if(pathogens == F){adult_agents <- adult_agents %>%
  select(-c(Infection_status,ha_infected,v1_infected,attempted_pathogen_transfer))}


Tick_agents = rbind(nymph_agents,adult_agents) 

write.csv(Tick_agents,paste0(getwd(),'/Cached_data/Tick_agents.csv'))

tick_agents <- adult_agents;rm(adult_agents)
