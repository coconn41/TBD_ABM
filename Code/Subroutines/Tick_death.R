tick_death = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(replete_death = ifelse(Lifestage=="Larvae" & fed == 1 & dropped == 1,
                                  rbinom(n = 1, size = 1, prob = L_rep_DR),
                                  ifelse(Lifestage == "Nymph" & fed == 1 & dropped == 1,
                                         rbinom(n=1, size=1, prob = N_rep_DR),
                                         ifelse(Lifestage=="Adult" & fed == 1 & dropped == 1,
                                                rbinom(n=1,size=1,A_rep_DR),
                                                0)))) %>%
    mutate(un_replete_death = ifelse(Lifestage == "Larvae" & fed == 0 & dropped == 0 & tick_age_wks >=40,
                                     rbinom(n = 1, size = 1, prob = L_unfed_DR_o40),
                                     ifelse(Lifestage == "Larvae" & fed == 0 & dropped == 0 & tick_age_wks <40,
                                            rbinom(n = 1, size = 1, prob = L_unfed_DR_lt40),
                                            ifelse(Lifestage == "Nymph" & fed == 0 & dropped == 0 & tick_age_wks >=40,
                                                   rbinom(n = 1, size = 1 ,prob = N_unfed_DR_o40),
                                                   ifelse(Lifestage == "Nymph" & fed == 0 & dropped == 0 & tick_age_wks <40,
                                                          rbinom(n = 1, size = 1, N_unfed_DR_lt40),
                                                          ifelse(Lifestage == "Adult" & fed == 0 & dropped == 0 & tick_age_wks >=40,
                                                                 rbinom(n = 1, size = 1, prob = A_unfed_DR_o40),
                                                                 ifelse(Lifestage == "Adult" & fed == 0 & dropped == 0 & tick_age_wks <40,
                                                                        rbinom(n = 1, size = 1, prob = A_unfed_DR_lt40),0))))))) %>%
    mutate(death = ifelse(sum(replete_death,un_replete_death)>=1,1,0)) %>%
    filter(num_ticks >= 0,
           death == 0) %>%
    dplyr::select(-c(replete_death,un_replete_death,death))
}
