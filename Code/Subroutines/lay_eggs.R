lay_eggs = function(tick_agents){
  
  tick_agents <- tick_agents %>%
    mutate(tick_age_wks = ifelse(Lifestage=="Adult" & # This is a dummy to separate new eggs from old
                                   sex == "female" & 
                                   mated == 1 & 
                                   day >= lay_egg,# find tick egg laying timing)
                                 -1,tick_age_wks),
           Lifestage = ifelse(Lifestage=="Adult" & 
                                sex == "female" & 
                                mated == 1 & 
                                day >= lay_egg,# find tick egg laying timing
                              "Eggs",Lifestage))
  
  num_new_eggs = length(which(tick_agents$tick_age_wks==-1))
  
  tick_agents <<- tick_agents %>%
    mutate(Infection_status = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                     "None",Infection_status),
           tick_age_wks = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                 0,tick_age_wks),
           num_ticks = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                              eggs_per_female,num_ticks),#Double check the 1000 eggs estimate
           transfer_type = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                  "None",transfer_type),
           attempted_pathogen_transfer = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                                0,attempted_pathogen_transfer),
           time_since_fed = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                   0,time_since_fed),
           dropped = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                            0,dropped),
           linked_type = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                "None",linked_type),
           time_since_mating = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                      0,time_since_mating),
           tick_age_wks = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                 0,tick_age_wks),
           links = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                          0,links),
           sex = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                        "none",sex),
           mated = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                          0,mated),
         Agent_ID = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                           (max(.$Agent_ID)+1):(max(.$Agent_ID)+num_new_eggs),Agent_ID))}
         