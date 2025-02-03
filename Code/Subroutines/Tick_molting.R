sexes = c("male","female")
tick_molting = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(molt = case_when(Lifestage == "Eggs" & 
                              season != "fall" &
                              season != "winter" &
                                day > egg_to_larvae ~ 1, #find age to molt, dummy age in for now
                            Lifestage == "Larvae" & 
                              dropped == 1 & 
                                day >= larvae_to_nymph_min &
                              day <= larvae_to_nymph_max ~ 1,# find age to molt, dummy age in for now
                            Lifestage == "Nymph" & 
                              dropped == 1 & 
                              season == "fall" ~ 1,
                             # day >= nymph_to_adult_min &
                              #day <= nymph_to_adult_max ~ 1,# find age to molt, dummy age in for now
                            TRUE ~ 0)) %>%
    mutate(fed = ifelse(molt==1,0,fed),
           time_since_fed = ifelse(molt==1,0,fed),
           dropped = ifelse(molt==1,0,dropped),
           die = ifelse(molt==1,0,die),
           time_since_mating = ifelse(molt==1,0,time_since_mating),
           mated = ifelse(molt==1,0,mated),
           links = ifelse(molt==1,0,links),
           time_on_host = ifelse(molt==1,0,time_on_host),
           molt_death_immune = ifelse(molt==1,1,0),
           Lifestage = ifelse(molt==1 & Lifestage=="Eggs","Larvae",
                              ifelse(molt==1 & Lifestage=="Larvae","Nymph",
                                     ifelse(molt==1 & Lifestage=="Nymph","Adult",Lifestage)))) %>%
    mutate(sex = ifelse(sex=="none" & Lifestage == "Adult",
                        sample(sexes,size=1),sex)) %>%
    mutate(molt = 0)
}
