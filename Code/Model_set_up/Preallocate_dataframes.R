tick_agents_num_row = 10000000

tick_agents = data.frame(County = character(tick_agents_num_row),
                         Site = character(tick_agents_num_row),
                         layer = integer(tick_agents_num_row),
                         metric = integer(tick_agents_num_row),
                         gridrows = integer(tick_agents_num_row),
                         gridcols = integer(tick_agents_num_row),
                         Agent_type = character(tick_agents_num_row),
                         row = integer(tick_agents_num_row),
                         col = integer(tick_agents_num_row),
                         network_ID = integer(tick_agents_num_row),
                         Agent_ID = integer(tick_agents_num_row),
                         Lifestage = character(tick_agents_num_row),
                         Infection_status = character(tick_agents_num_row),
                         links = integer(tick_agents_num_row),
                         time_on_host = integer(tick_agents_num_row),
                         fed = integer(tick_agents_num_row),
                         mated = integer(tick_agents_num_row),
                         sex = character(tick_agents_num_row),
                         time_since_mating = integer(tick_agents_num_row),
                         tick_age_wks = integer(tick_agents_num_row),
                         die = integer(tick_agents_num_row),
                         linked_type = character(tick_agents_num_row),
                         num_ticks = integer(tick_agents_num_row),
                         dropped = integer(tick_agents_num_row),
                         time_since_fed = integer(tick_agents_num_row),
                         attempted_pathogen_transfer = integer(tick_agents_num_row),
                         transfer_type = character(tick_agents_num_row),
                         molt = integer(tick_agents_num_row))

loaded_data = read.csv(paste0(getwd(),'/Cached_data/Tick_agents.csv'))[,-1] %>%
  mutate(tick_age_wks = ifelse(Lifestage=="Adult",65,# Starting age Summer solstice (Jun. 21st) to Sept. 22nd of next year
                               ifelse(Lifestage=="Nymph",39,NA))) %>% # Starting age Summer solstice (Jun. 21st) to March 20th of next year
  mutate(die = 0,
         linked_type = "N",
         num_ticks = 1,
         dropped = 0,
         time_since_fed = 0,
         attempted_pathogen_transfer = 0)

nymph_agents = loaded_data %>% filter(Lifestage=="Nymph")
loaded_data = loaded_data %>% filter(Lifestage!="Nymph")

tick_agents[1:nrow(loaded_data), 1:ncol(loaded_data)] <- loaded_data  
