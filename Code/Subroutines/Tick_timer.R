tick_timer = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(tick_age_wks = ifelse(Lifestage!="Eggs",tick_age_wks+(1/168),0), # hours per week
           time_since_mating = ifelse(mated==1,time_since_mating+1,0),
           time_since_fed = ifelse(fed==1,time_since_fed + 1,0),
           time_on_host = ifelse(links>0&dropped==0,time_on_host+1,
                                 ifelse(dropped==1,time_on_host,0)),
           fed = ifelse(Lifestage == "Larvae" & time_on_host >= (3*24),1,
                        ifelse(Lifestage == "Nymph" & time_on_host >= (5*24),1,
                               ifelse(Lifestage == "Adult" & sex == "female" & time_on_host >= (10*24),1,0)))) 
}
