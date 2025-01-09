tick_molting = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(molt = case_when(Lifestage == "Eggs" & tick_age_wks >, #find age to molt
                            Lifestage == "Larvae" & fed == 1 & tick_age_wks > ,# find age to molt
                            Lifestage == "Nymph" & fed == 1 & tick_age_wks > ,# find age to molt
                            TRUE ~ 0)) %>%
    mutate(fed = 0,
           time_since_fed = 0,
           num_ticks = 1,
           dropped = 0,
           die = 0,
           time_since_mating = 0,
           mated = 0,
           links = 0, 
           time_on_host = 0)
}