#####
# Load libraries:
#####
library(tidyverse)
library(tmaptools)
#####
# Load data
#####
Networks = paste0("Network_",rep(1:8))
Rounds = paste0("Round_",rep(1:2))
for(i in unique(Networks)){
  for(j in unique(Rounds)){
  files = list.files(paste0(getwd(),'/Complete_parameter_sweeps/',
                            i,'/',
                            j,'/'))[-1]
  if(length(files)==0){next}
  files = paste0(i,"/",j,"/",files)
if(i==Networks[1]&j==Rounds[1]){files2=files}else(files2=c(files2,files))
  }
}
files = files2;rm(files2)
ind = 0
for(j in unique(files)){
  ind = ind+1
  
  df <- read.csv(paste0(getwd(),'/Complete_parameter_sweeps/',j)) 
  if(min(df$deer_density)==0.1 & 
     min(df$mouse_density)==0.1 & 
     min(df$tick_density) == 10 &
     min(df$deer_carrying_capacity) == 25 &
     min(df$mouse_carrying_capacity) == 25 &
     min(df$deer_attach_prob) == 0.1 & 
     min(df$mouse_attach_prob) == 0.1){
    for(k in 1:7){
      if(k==1){df$Parameter_val = 0.1
      df$Parameter_name = "deer_density"}
      if(k==2){df$Parameter_val = 0.1
      df$Parameter_name = "mouse_density"}
      if(k==3){df$Parameter_val = 10
      df$Parameter_name = "tick_density"}
      if(k==4){df$Parameter_val = 25
      df$Parameter_name = "deer_carrying_capacity"}
      if(k==5){df$Parameter_val = 25
      df$Parameter_name = "mouse_carrying_capacity"}
      if(k==6){df$Parameter_val = 0.1
      df$Parameter_name = "mouse_attach_prob"}
      if(k==7){df$Parameter_val = 0.1
      df$Parameter_name = "deer_attach_prob"}
      newdf = df
      if(k==1){newdf2 = newdf}
      if(k>1){newdf2 = rbind(newdf2,newdf)}
    }
    df = newdf2}else{
    df = df %>%
    mutate(Parameter_val = case_when(deer_density > 0.1 ~ deer_density,
                                     mouse_density > 0.1 ~ mouse_density,
                                     tick_density > 10 ~ tick_density,
                                     mouse_carrying_capacity > 25 ~ mouse_carrying_capacity,
                                     deer_carrying_capacity > 25 ~ deer_carrying_capacity,
                                     deer_attach_prob > 0.1 ~ deer_attach_prob,
                                     mouse_attach_prob > 0.1 ~ mouse_attach_prob),
           Parameter_name = case_when(deer_density > 0.1 ~ "deer_density",
                                      mouse_density > 0.1 ~ "mouse_density",
                                      tick_density > 10 ~ "tick_density",
                                      mouse_carrying_capacity > 25 ~ "mouse_carrying_capacity",
                                      deer_carrying_capacity > 25 ~ "deer_carrying_capacity",
                                      deer_attach_prob > 0.1 ~ "deer_attach_prob",
                                      mouse_attach_prob > 0.1 ~ "mouse_attach_prob"))}
  if(ind==1){df2 = df}
  if(ind>1){df2 = rbind(df,df2)}
}

##### Previous code:
df2 = df2 %>%
  mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
                               Lifestage=="Adult" ~ "Adults",
                               TRUE ~ Lifestage),
         Lifestage = factor(Lifestage,
                            levels = c("Eggs","Larvae","Nymphs","Adults")),
         simulation_day = (day_of_year+(year*365))-264,
         simulation_week = ceiling(simulation_day/7)) %>%
  select(-c(deer_density,mouse_density,tick_density,mouse_attach_prob,
            deer_attach_prob,mouse_carrying_capacity,deer_carrying_capacity))

cohort_data = df2 %>%
  filter(year!=0) %>%
  mutate(simulation_day = (day_of_year+(year*365))-264) %>%
  group_by(simulation_day,Lifestage,day_of_year,year,
           Parameter_name,Parameter_val) %>% # simulation_day
  reframe(total_ticks = round(mean(total_ticks))) %>%
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

ch2 = ch2 %>%
  filter(is.na(day_of_year)==F) %>%
  mutate(Lifestage = factor(Lifestage,levels=c("Eggs","Larvae","Nymphs","Adults")))

ch = ch2 %>%
  anti_join(.,newdat2)

ggplot(data = ch2,
       aes(x=simulation_day,y=total_ticks,color=Lifestage,group=cohort))+
  geom_line(linewidth=1)+
  facet_grid(.~paste0("Cohort ",cohort),scale='free_x') +
  scale_color_manual(values = get_brewer_pal(n=4,"Set2"))+
  ylab("Total ticks")+
  xlab("Simulation day")+
  theme_classic()

ch3 <- ch2 %>%
  group_by(Parameter_name) %>%
  mutate(
    param_rank = dense_rank(Parameter_val)
  ) %>%
  ungroup()

p1=ggplot(data = ch3,
       aes(x=simulation_day,
           y=total_ticks,
           color=factor(param_rank),
           group=interaction(cohort,Parameter_val)))+
  geom_line(linewidth=1)+
  facet_grid(Parameter_name~param_rank,scales='free_y')+
  scale_color_viridis_d(
    option = "C",
    begin = 0.2,
    end = 0.9,
    name = "Relative level")+
  ylab("Total ticks")+
  xlab("Simulation day")+
  geom_hline(yintercept=0)+
  theme_classic();p1
ggsave(plot = p1,
       paste0(getwd(),'/Figures/Figures/Parameter_sweep_1.jpeg'),
       dpi = 300)

pal=get_brewer_pal(n=3,"Blues",c(.3,1))
pal=get_brewer_pal(n=3,"Greens",c(.3,1))
pal
