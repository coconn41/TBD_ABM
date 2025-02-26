load(paste0(getwd(),'/Simulations/Attach_25/Pathogen_25/Network_6/BI_attach_25_path_trans_25.RData'))
library(dplyr)
library(ggplot2)
library(tmaptools)

cohort_data = tick_data2 %>%
  filter(year!=0) %>%
 mutate(simulation_day = (day_of_year+(year*365))-264) %>%
  group_by(simulation_day,Lifestage,day_of_year,year) %>% # simulation_day
  reframe(total_ticks = round(mean(total_ticks))) %>%
  mutate(cohort = case_when(year == 1 & Lifestage == "Adult" & day_of_year < 190 ~ 1,
                            year == 1 & Lifestage == "Nymph" ~ 2,
                            year == 1 & Lifestage == "Adult" & day_of_year >= 190 ~ 2,
                            year == 1 & Lifestage %in% c("Eggs","Larvae") ~ 3,
                            TRUE ~ -1))
  
for(i in 2:max(cohort_data$year)){
  cohort_data = cohort_data %>%
    mutate(cohort = case_when(cohort == -1 & year == i & Lifestage == "Adult" & day_of_year < 190 ~ 1+i,
                              cohort == -1 & year == i & Lifestage == "Nymph" ~ 2+i,
                              cohort == -1 & year == i & Lifestage == "Adult" & day_of_year >= 190 ~ 2+i,
                              cohort == -1 & year == i & Lifestage %in% c("Eggs","Larvae") ~ 3+i,
                              TRUE ~ cohort))
}

cohort_data = expand.grid(#day_of_year = 1:365,
                          simulation_day = min(cohort_data$simulation_day):max(cohort_data$simulation_day),
                              Lifestage = c("Eggs","Larvae","Nymph","Adult"),
                              cohort = unique(cohort_data$cohort)) %>%
  left_join(.,cohort_data) %>%
  mutate(total_ticks = ifelse(is.na(total_ticks)==T,0,total_ticks))

ggplot(data = cohort_data %>% filter(cohort > 2),
       aes(x = simul, y = total_ticks,
           color=Lifestage,alpha=as.factor(cohort), group=interaction(Lifestage,as.factor(cohort))))+
  geom_line(linewidth=1)+
  facet_wrap(.~Lifestage,scales="free_y")+
  scale_color_manual(values = c(get_brewer_pal("Set2",n=4)))+
  scale_alpha_manual("Cohort",values = seq(.4,1,by=.1))+
  ylab("Total ticks")+
  xlab("Day of year")+
  theme_bw()

ggplot(data = cohort_data %>%
         filter(cohort > 2) %>%
         mutate(Lifestage = case_when(Lifestage == "Adult" ~ "Adults",
                                      Lifestage == "Nymph" ~ "Nymphs",
                                      TRUE ~ Lifestage),
                Lifestage = factor(Lifestage,
                                   levels = c("Eggs","Larvae","Nymphs","Adults")),
                cohort = factor(cohort,levels=c(3:8))),
       aes(x = simulation_day, y = total_ticks))+
           #color=Lifestage,alpha=as.factor(cohort), group=interaction(Lifestage,as.factor(cohort))))+
  geom_line(linewidth=1)+
 # ggh4x::facet_grid2(Lifestage ~ cohort, scales = "free_x",independent = "x")+
  facet_grid(Lifestage~year,scales="free")+
  scale_color_manual(values = c(get_brewer_pal("Set2",n=5)))+
  scale_alpha_manual("Cohort",values = seq(.4,1,by=.1))+
  ylab("Total ticks")+
  xlab("Day of year")+
  theme_bw()


