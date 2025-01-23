lay_eggs = function(tick_agents){
  
  tick_agents <<- tick_agents %>%
    mutate(Lifestage = ifelse(Lifestage=="Adult" & 
                                sex == "female" & 
                                mated == 1 & 
                                day >= lay_egg,# find tick egg laying timing
                              "Eggs",Lifestage))
  
  new_eggs = tick_agents %>%
    filter(Lifestage=="Eggs") %>%
    mutate(Infection_status = ifelse(Lifestage=="Eggs","None",Infection_status),
         num_ticks = ifelse(Lifestage == "Eggs",eggs_per_female,1),#Double check the 1000 eggs estimate
         transfer_type = ifelse(Lifestage=="Eggs","None",transfer_type),
         attempted_pathogen_transfer = ifelse(Lifestage=="Eggs",0,attempted_pathogen_transfer),
         time_since_fed = ifelse(Lifestage=="Eggs",0,time_since_fed),
         dropped = ifelse(Lifestage=="Eggs",0,dropped),
         linked_type = ifelse(Lifestage=="Eggs","None",linked_type),
         time_since_mating = ifelse(Lifestage=="Eggs",0,time_since_mating),
         tick_age_wks = ifelse(Lifestage=="Eggs",0,tick_age_wks),
         links = ifelse(Lifestage=="Eggs",0,links),
         sex = "none",
         mated = ifelse(Lifestage=="Eggs",0,mated),
         Agent_ID = (max(.$Agent_ID)+1):(max$Agent_ID+nrow(new_eggs)))
  
  tick_agents <<- tick_agents %>%
    rbind(.,new_eggs)
           }