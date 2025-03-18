#####
# Load libraries
#####
library(tidyverse)

#####
# Get all starting values:
#####
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))
set.seed(1)
source(paste0(getwd(),'/Code/Model_set_up/Load_model_environment.R'))
starting_tick_data = tick_agents %>% 
  bind_rows(.,nymph_agents) %>%
  filter(network_ID %in% c(1,6,7)) %>%
  mutate(ha_pos = ifelse(Infection_status=="ha",1,0),
         v1_pos = ifelse(Infection_status=="v1",1,0)) %>%
  group_by(network_ID,Lifestage) %>%
  summarize(tot = n(),
            ha_pos = sum(ha_pos),
            v1_pos = sum(v1_pos)) %>%
  mutate(ha_perc = (ha_pos/tot)*100,
         v1_perc = (v1_pos/tot)*100)
write.csv(starting_tick_data,
          paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/starting_tick_data.csv'))

#####
# Pull in and clean network 6:
#####
load(paste0(getwd(),'/Simulations/Network_6/timestep_65700_attach_25_path_trans_100alt.RData'))
tick_data2 = tick_data2 %>%
  filter(Lifestage!="") %>%
  mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
                               Lifestage=="Adult" ~ "Adults",
                               TRUE ~ Lifestage),
         Lifestage = factor(Lifestage,
                            levels = c("Eggs","Larvae","Nymphs","Adults")),
         simulation_day = (day_of_year+(year*365))-264,
         simulation_week = ceiling(simulation_day/7))
deer_data2 = deer_data2 %>%
  filter(season!="")
mouse_data2 = mouse_data2 %>%
  filter(season!="")

cohort_data = tick_data2 %>%
  filter(year!=0) %>%
  mutate(simulation_day = (day_of_year+(year*365))-264) %>%
  group_by(simulation_day,Lifestage,day_of_year,year) %>% # simulation_day
  reframe(total_ticks = round(mean(total_ticks)),
          tot_v1 = round(mean(tot_v1)),
          tot_ha = round(mean(tot_ha))) %>%
  mutate(cohort = case_when(year == 1 & Lifestage == "Adults" ~ 0,
                            year == 1 & Lifestage == "Nymphs" ~ 0,
                            year == 1 & Lifestage %in% c("Eggs","Larvae") ~ 1,
                            year == 2 & Lifestage == "Larvae" & day_of_year <= 111 ~ 1,
                            year == 2 & Lifestage == "Adults" & day_of_year < 190 ~ 0,
                            TRUE ~ -1))

for(i in 2:max(cohort_data$year)){
  cohort_data = cohort_data %>%
    mutate(cohort = case_when(cohort == -1 & year == i & Lifestage == "Adults" & day_of_year < 190 ~ i-2,
                              cohort == -1 & year == i & Lifestage == "Nymphs" ~ i-1,
                              cohort == -1 & year == i & Lifestage == "Adults" & day_of_year >= 190 ~ i-1,
                              cohort == -1 & year == i & Lifestage == "Eggs" ~ i,
                              cohort == -1 & year == i & day_of_year <= 171 & Lifestage == "Larvae" ~ i-1,
                              cohort == -1 & year == i & day_of_year > 171 & Lifestage == "Larvae" ~ i,
                              TRUE ~ cohort))
}
for(i in 1:max(cohort_data$cohort)){
  ch = cohort_data %>%
    filter(cohort==i)
  if(nrow(ch)==0){next}
  eg = expand.grid(simulation_day = min(ch$simulation_day):max(ch$simulation_day),
                   Lifestage = c("Eggs","Larvae","Nymphs","Adults"),
                   cohort = i)
  ch = ch %>%
    left_join(eg,.) %>%
    mutate(total_ticks = ifelse(is.na(day_of_year)==T,0,total_ticks))
  
  minls = ch %>%
    filter(is.na(day_of_year)==F,
           Lifestage!="Adults",
           Lifestage!="Eggs") %>%
    group_by(Lifestage) %>%
    summarize(mn = min(simulation_day),
              # mn2 = min(day_of_year),
              yr = min(year)) %>%
    ungroup() %>%
    mutate(simulation_day = mn,
           cohort = i,
           # day_of_year = mn2,
           year = yr) %>%
    select(-c(mn,yr))
  
  minls2 = ch %>%
    filter(is.na(day_of_year)==F,
           #  day_of_year>250+(365*(i-1)),
           Lifestage=="Adults") %>%
    summarize(mn = min(simulation_day),
              # mn2 = min(day_of_year),
              yr = min(year)) %>%
    ungroup() %>%
    mutate(simulation_day = mn,
           cohort = i,
           #day_of_year = mn2,
           year = yr) %>%
    select(-c(mn,yr)) %>%
    mutate(Lifestage="Adults")
  
  
  newdat = minls %>%
    rbind(.,minls2) %>%
    left_join(.,ch) %>%
    mutate(Lifestage = case_when(Lifestage=="Larvae" ~ "Eggs",
                                 Lifestage=="Nymphs" ~ "Larvae",
                                 Lifestage=="Adults" ~ "Nymphs",
                                 TRUE ~ Lifestage))
  ch = ch %>%
    bind_rows(.,newdat)
  
  if(i==1){ch2 = ch
  newdat2 = newdat}
  if(i>1){ch2 = rbind(ch2,ch)
  newdat2 = rbind(newdat,newdat2)}
}

cohort_data = cohort_data %>%
  filter(cohort<=5,
         cohort!=0)

write.csv(cohort_data,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_ticks.csv'))
write.csv(tick_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_ticks_all.csv'))
write.csv(deer_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_deer.csv'))
write.csv(mouse_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_mice.csv'))
          
#####
# Pull in and clean network 1:
#####
load(paste0(getwd(),'/Simulations/Network_1/timestep_65700_attach_25_path_trans_100alt.RData'))
tick_data2 = tick_data2 %>%
  filter(Lifestage!="") %>%
  mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
                               Lifestage=="Adult" ~ "Adults",
                               TRUE ~ Lifestage),
         Lifestage = factor(Lifestage,
                            levels = c("Eggs","Larvae","Nymphs","Adults")),
         simulation_day = (day_of_year+(year*365))-264,
         simulation_week = ceiling(simulation_day/7))
deer_data2 = deer_data2 %>%
  filter(season!="")
mouse_data2 = mouse_data2 %>%
  filter(season!="")

cohort_data = tick_data2 %>%
  filter(year!=0) %>%
  mutate(simulation_day = (day_of_year+(year*365))-264) %>%
  group_by(simulation_day,Lifestage,day_of_year,year) %>% # simulation_day
  reframe(total_ticks = round(mean(total_ticks)),
          tot_v1 = round(mean(tot_v1)),
          tot_ha = round(mean(tot_ha))) %>%
  mutate(cohort = case_when(year == 1 & Lifestage == "Adults" ~ 0,
                            year == 1 & Lifestage == "Nymphs" ~ 0,
                            year == 1 & Lifestage %in% c("Eggs","Larvae") ~ 1,
                            year == 2 & Lifestage == "Larvae" & day_of_year <= 111 ~ 1,
                            year == 2 & Lifestage == "Adults" & day_of_year < 190 ~ 0,
                            TRUE ~ -1))

for(i in 2:max(cohort_data$year)){
  cohort_data = cohort_data %>%
    mutate(cohort = case_when(cohort == -1 & year == i & Lifestage == "Adults" & day_of_year < 190 ~ i-2,
                              cohort == -1 & year == i & Lifestage == "Nymphs" ~ i-1,
                              cohort == -1 & year == i & Lifestage == "Adults" & day_of_year >= 190 ~ i-1,
                              cohort == -1 & year == i & Lifestage == "Eggs" ~ i,
                              cohort == -1 & year == i & day_of_year <= 171 & Lifestage == "Larvae" ~ i-1,
                              cohort == -1 & year == i & day_of_year > 171 & Lifestage == "Larvae" ~ i,
                              TRUE ~ cohort))
}
for(i in 1:max(cohort_data$cohort)){
  ch = cohort_data %>%
    filter(cohort==i)
  if(nrow(ch)==0){next}
  eg = expand.grid(simulation_day = min(ch$simulation_day):max(ch$simulation_day),
                   Lifestage = c("Eggs","Larvae","Nymphs","Adults"),
                   cohort = i)
  ch = ch %>%
    left_join(eg,.) %>%
    mutate(total_ticks = ifelse(is.na(day_of_year)==T,0,total_ticks))
  
  minls = ch %>%
    filter(is.na(day_of_year)==F,
           Lifestage!="Adults",
           Lifestage!="Eggs") %>%
    group_by(Lifestage) %>%
    summarize(mn = min(simulation_day),
              # mn2 = min(day_of_year),
              yr = min(year)) %>%
    ungroup() %>%
    mutate(simulation_day = mn,
           cohort = i,
           # day_of_year = mn2,
           year = yr) %>%
    select(-c(mn,yr))
  
  minls2 = ch %>%
    filter(is.na(day_of_year)==F,
           #  day_of_year>250+(365*(i-1)),
           Lifestage=="Adults") %>%
    summarize(mn = min(simulation_day),
              # mn2 = min(day_of_year),
              yr = min(year)) %>%
    ungroup() %>%
    mutate(simulation_day = mn,
           cohort = i,
           #day_of_year = mn2,
           year = yr) %>%
    select(-c(mn,yr)) %>%
    mutate(Lifestage="Adults")
  
  
  newdat = minls %>%
    rbind(.,minls2) %>%
    left_join(.,ch) %>%
    mutate(Lifestage = case_when(Lifestage=="Larvae" ~ "Eggs",
                                 Lifestage=="Nymphs" ~ "Larvae",
                                 Lifestage=="Adults" ~ "Nymphs",
                                 TRUE ~ Lifestage))
  ch = ch %>%
    bind_rows(.,newdat)
  
  if(i==1){ch2 = ch
  newdat2 = newdat}
  if(i>1){ch2 = rbind(ch2,ch)
  newdat2 = rbind(newdat,newdat2)}
}

cohort_data = cohort_data %>%
  filter(cohort<=5,
         cohort!=0)

write.csv(cohort_data,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_ticks.csv'))
write.csv(tick_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_ticks_all.csv'))
write.csv(deer_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_deer.csv'))
write.csv(mouse_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_mice.csv'))

#####
# Pull in and clean network 7:
#####

load(paste0(getwd(),'/Simulations/Network_7/BI_attach_25_path_trans_100alt.RData'))
tick_data2 = tick_data2 %>%
  filter(Lifestage!="") %>%
  mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
                               Lifestage=="Adult" ~ "Adults",
                               TRUE ~ Lifestage),
         Lifestage = factor(Lifestage,
                            levels = c("Eggs","Larvae","Nymphs","Adults")),
         simulation_day = (day_of_year+(year*365))-264,
         simulation_week = ceiling(simulation_day/7))
deer_data2 = deer_data2 %>%
  filter(season!="")
mouse_data2 = mouse_data2 %>%
  filter(season!="")

cohort_data = tick_data2 %>%
  filter(year!=0) %>%
  mutate(simulation_day = (day_of_year+(year*365))-264) %>%
  group_by(simulation_day,Lifestage,day_of_year,year) %>% # simulation_day
  reframe(total_ticks = round(mean(total_ticks)),
          tot_v1 = round(mean(tot_v1)),
          tot_ha = round(mean(tot_ha))) %>%
  mutate(cohort = case_when(year == 1 & Lifestage == "Adults" ~ 0,
                            year == 1 & Lifestage == "Nymphs" ~ 0,
                            year == 1 & Lifestage %in% c("Eggs","Larvae") ~ 1,
                            year == 2 & Lifestage == "Larvae" & day_of_year <= 111 ~ 1,
                            year == 2 & Lifestage == "Adults" & day_of_year < 190 ~ 0,
                            TRUE ~ -1))

for(i in 2:max(cohort_data$year)){
  cohort_data = cohort_data %>%
    mutate(cohort = case_when(cohort == -1 & year == i & Lifestage == "Adults" & day_of_year < 190 ~ i-2,
                              cohort == -1 & year == i & Lifestage == "Nymphs" ~ i-1,
                              cohort == -1 & year == i & Lifestage == "Adults" & day_of_year >= 190 ~ i-1,
                              cohort == -1 & year == i & Lifestage == "Eggs" ~ i,
                              cohort == -1 & year == i & day_of_year <= 171 & Lifestage == "Larvae" ~ i-1,
                              cohort == -1 & year == i & day_of_year > 171 & Lifestage == "Larvae" ~ i,
                              TRUE ~ cohort))
}
for(i in 1:max(cohort_data$cohort)){
  ch = cohort_data %>%
    filter(cohort==i)
  if(nrow(ch)==0){next}
  eg = expand.grid(simulation_day = min(ch$simulation_day):max(ch$simulation_day),
                   Lifestage = c("Eggs","Larvae","Nymphs","Adults"),
                   cohort = i)
  ch = ch %>%
    left_join(eg,.) %>%
    mutate(total_ticks = ifelse(is.na(day_of_year)==T,0,total_ticks))
  
  minls = ch %>%
    filter(is.na(day_of_year)==F,
           Lifestage!="Adults",
           Lifestage!="Eggs") %>%
    group_by(Lifestage) %>%
    summarize(mn = min(simulation_day),
              # mn2 = min(day_of_year),
              yr = min(year)) %>%
    ungroup() %>%
    mutate(simulation_day = mn,
           cohort = i,
           # day_of_year = mn2,
           year = yr) %>%
    select(-c(mn,yr))
  
  minls2 = ch %>%
    filter(is.na(day_of_year)==F,
           #  day_of_year>250+(365*(i-1)),
           Lifestage=="Adults") %>%
    summarize(mn = min(simulation_day),
              # mn2 = min(day_of_year),
              yr = min(year)) %>%
    ungroup() %>%
    mutate(simulation_day = mn,
           cohort = i,
           #day_of_year = mn2,
           year = yr) %>%
    select(-c(mn,yr)) %>%
    mutate(Lifestage="Adults")
  
  
  newdat = minls %>%
    rbind(.,minls2) %>%
    left_join(.,ch) %>%
    mutate(Lifestage = case_when(Lifestage=="Larvae" ~ "Eggs",
                                 Lifestage=="Nymphs" ~ "Larvae",
                                 Lifestage=="Adults" ~ "Nymphs",
                                 TRUE ~ Lifestage))
  ch = ch %>%
    bind_rows(.,newdat)
  
  if(i==1){ch2 = ch
  newdat2 = newdat}
  if(i>1){ch2 = rbind(ch2,ch)
  newdat2 = rbind(newdat,newdat2)}
}

cohort_data = cohort_data %>%
  filter(cohort<=5,
         cohort!=0)

write.csv(cohort_data,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_ticks.csv'))
write.csv(tick_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_ticks_all.csv'))
write.csv(deer_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_deer.csv'))
write.csv(mouse_data2,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_mice.csv'))
