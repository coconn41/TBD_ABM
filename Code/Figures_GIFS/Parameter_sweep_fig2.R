#####
# Load libraries:
#####
library(tidyverse)
library(tmaptools)
library(ggnewscale)
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
  j2 = substring(j,19,1000)
  if(grepl("overpop",j)==T){df$model_end = "Overpopulation"}
  if(grepl("dieoff",j)==T){df$model_end = "Dieoff"}
  if(grepl("finished",j)==T){df$model_end = "Finished"}
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
  select(-c(mouse_density,mouse_attach_prob,
            mouse_carrying_capacity,deer_carrying_capacity))

cohort_data = df2 %>%
  filter(year!=0) %>%
  mutate(simulation_day = (day_of_year+(year*365))-264) %>%
  group_by(network_ID,simulation_day,Lifestage,day_of_year,year,
           deer_density,tick_density,deer_attach_prob,
           model_end) %>% # simulation_day
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

ch3 = ch2 %>%
  left_join(.,ch2 %>%
              group_by(deer_density,tick_density,deer_attach_prob) %>%
              summarize(xintercept = max(simulation_day,na.rm=T)))

ch3 = ch3 %>%
  mutate(deer_attach_prob_lab = paste0("Deer Attach Prob = ",deer_attach_prob),
         tick_density_lab = paste0("Tick Density = ",tick_density),
         deer_density_lab = paste0("Deer Density = ",deer_density),
         network_ID_lab = paste0("Network ",network_ID))

p1=ggplot(data = ch3,
          aes(x=simulation_day,
              y=total_ticks,
              color=Lifestage,
              group = cohort))+
  geom_line(linewidth=1)+
  new_scale_color()+
  geom_vline(data=ch3,aes(xintercept = xintercept, col=model_end),lwd=1)+
  scale_color_manual(values = c('#E41A1C','#4DAF4A','#FC8D62'))+
  #facet_wrap(network_ID~deer_attach_prob*tick_density*deer_density)+
  facet_grid(network_ID_lab~deer_attach_prob_lab*tick_density_lab*deer_density_lab,scales='free_y')+
  #facet_grid(deer_attach_prob*tick_density ~ deer_density,scales='free_y')+
  ylab("Total ticks")+
  xlab("Simulation day")+
  geom_hline(yintercept=0)+
  theme_classic();p1
ggsave(plot = p1,
       paste0(getwd(),'/Figures/Figures/Parameter_sweep_2.jpeg'),
       dpi = 300)

p2 = ggplot(data = ch3 %>% filter(deer_density == 0.2,
                                  tick_density == 10,
                                  deer_attach_prob == .5),
            aes(x=simulation_day,
                y=total_ticks,
                color=Lifestage,
                group = cohort))+
  geom_line(linewidth=1)+
  ylab("Total ticks")+
  xlab("Simulation day")+
  geom_hline(yintercept=0)+
  theme_classic();p2

ggplot(data = ch3 %>% filter(deer_density == 0.2,
                             tick_density == 10,
                             deer_attach_prob == .5),
       aes(x=simulation_day,y=total_ticks,color=Lifestage,group=cohort))+
  geom_line(linewidth=1)+
  facet_grid(.~paste0("Cohort ",cohort),scale='free_x') +
  scale_color_manual(values = get_brewer_pal(n=4,"Set2"))+
  ylab("Total ticks")+
  xlab("Simulation day")+
  theme_classic()
