#load(paste0(getwd(),'/Simulations/Attach_25/Pathogen_25/Network_6/BI_attach_25_path_trans_25.RData'))
load(paste0(getwd(),'/Debugging/Network_6/net_6_timestep_65000.RData'))
library(tidyverse)
library(ggplot2)
library(tmaptools)

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

# newdat_filter = newdat2 %>%
#   filter(is.na(day_of_year)==F,
#          )

# ch_barplot = ch %>%
#   filter(Lifestage!="Eggs",
         

ch = ch2 %>%
  anti_join(.,newdat2) %>%
  mutate(tot_v1 = ifelse(is.na(tot_v1)==T,0,tot_v1),
         tot_ha = ifelse(is.na(tot_ha)==T,0,tot_ha),
         uninfected = total_ticks - tot_v1 - tot_ha) %>%
  pivot_longer(c(tot_v1,tot_ha,uninfected)) %>%
  mutate(name = case_when(name == "uninfected" ~ "Uninfected",
                          name == "tot_ha" ~ "Ap-ha",
                          name == "tot_v1" ~ "Ap-v1",
                          TRUE ~ "NA"),
        name = factor(name,levels=c("Uninfected","Ap-ha","Ap-v1")))

perc_plot = ch2 %>%
  anti_join(.,newdat2) %>%
  filter(Lifestage!="Eggs") %>%
  mutate(perc_v1 = (tot_v1/total_ticks)*100,
         perc_ha = (tot_ha/total_ticks)*100) %>%
  pivot_longer(c(perc_ha,perc_v1)) %>%
  mutate(name = case_when(name == "perc_ha" ~ "Ap-ha (%)",
                          name == "perc_v1" ~ "Ap-v1 (%)",
                          TRUE ~ "NA"))
ggplot(data = ch2,
       aes(x=simulation_day,y=total_ticks,color=Lifestage,group=cohort))+
  geom_line(linewidth=1)+
  facet_wrap(.~paste0("Cohort ",cohort),scale='free_x') +
  scale_color_manual(values = get_brewer_pal(n=4,"Set2"))+
  ylab("Total ticks")+
  xlab("Simulation day")+
  theme_classic()

# ggplot(data = ch2,
#        aes(x=simulation_day,y=total_ticks,color=Lifestage,group=cohort,alpha=cohort))+
#   geom_line(linewidth=1)+
#   #facet_wrap(.~paste0("Cohort ",cohort),scale='free_x') +
#   scale_color_manual(values = get_brewer_pal(n=4,"Set2"))+
#   ylab("Total ticks")+
#   xlab("Simulation day")+
#   theme_classic()

#####
# Percentage plots
#####

ggplot(data = perc_plot,
       aes(x=simulation_day,y=value,color=name))+
  geom_smooth()+
  facet_wrap(.~Lifestage)+
  geom_vline(xintercept = ((8760*5)/24))+
  scale_color_manual("Pathogen",values = get_brewer_pal(n=3,"Set1")[-3])+
  theme_bw()

ggplot(data = perc_plot %>%
         group_by(cohort) %>%
         mutate(cohort_day = (simulation_day-min(simulation_day))+1),
       aes(x=cohort_day,y=value,color=name))+
  geom_smooth()+
  facet_wrap(.~cohort,scales='free_x')+
  scale_color_manual("Pathogen",values = get_brewer_pal(n=3,"Set1")[-3])+
  theme_bw()+
  xlab("Cohort day")+
  ylab("Prevalence (%)")

ggplot(data = perc_plot %>%
         group_by(cohort) %>%
         mutate(cohort_day = (simulation_day-min(simulation_day))+1),
       aes(x=cohort_day,y=value,color=name,group=interaction(name,cohort)))+
  geom_smooth()+
  scale_color_manual("Pathogen",values = get_brewer_pal(n=3,"Set1")[-3])+
  theme_bw()+
  xlab("Cohort day")+
  ylab("Prevalence (%)")

#####
# Split by cohort
#####

ggplot(data = ch %>%
         filter(Lifestage != "Eggs"),
       aes(x=simulation_day,fill=name,y=value))+#,group=Lifestage)) +
  geom_bar(position='stack',stat='identity',width=1)+
  scale_fill_manual("Infection status",
                    values = c("grey50",
                               get_brewer_pal(n=3,"Reds")[3],
                               get_brewer_pal(n=3,"Blues")[3]))+
  ylab("Total ticks")+
  xlab("Simulation day")+
  geom_vline(xintercept = subset(newdat,newdat$Lifestage!="Eggs")$simulation_day)+
  facet_wrap(.~paste0("Cohort ",cohort),scale='free_x')+
  theme_bw()

#####
# Split by even / odd:
#####

ggplot(data = ch %>%
         filter(Lifestage != "Eggs") %>%
         mutate(cohort2 = ifelse(cohort%%2==0,"Even","Odd"),
                cohort2 = factor(cohort2,levels=c("Odd","Even"))),
       aes(x=simulation_day,fill=name,y=value))+#,group=Lifestage)) +
  geom_bar(position='stack',stat='identity',width=1)+
  scale_fill_manual("Infection status",
                    values = c("grey50",
                               get_brewer_pal(n=3,"Reds")[3],
                               get_brewer_pal(n=3,"Blues")[3]))+
  ylab("Total ticks")+
  xlab("Simulation day")+
  geom_vline(xintercept = subset(newdat,newdat$Lifestage!="Eggs")$simulation_day)+
  facet_wrap(.~cohort2,scale="free_x")+
  theme_bw()       



#####
# Yearly prevalence plots:
#####

nymph_perc = tick_data2 %>%
  filter(Lifestage=="Nymphs") %>%
  group_by(timestep,year,Lifestage,network_ID,day_of_year,season) %>%
  summarize(tot_ha = sum(tot_ha),
            tot_v1 = sum(tot_v1),
            total_ticks = sum(total_ticks)) %>%
  group_by(year) %>%
  mutate(lowtime = min(timestep)) %>%
  filter(timestep==lowtime) %>%
  mutate(ha_perc = ((tot_ha)/total_ticks)*100,
         v1_perc = ((tot_v1)/total_ticks)*100)

adult_perc = tick_data2 %>%
  filter(Lifestage=="Adults",
         day_of_year>220) %>%
  group_by(timestep,year,Lifestage,network_ID,day_of_year,season) %>%
  summarize(tot_ha = sum(tot_ha),
            tot_v1 = sum(tot_v1),
            total_ticks = sum(total_ticks)) %>%
  group_by(year) %>%
  mutate(lowtime = min(timestep)) %>%
  filter(timestep==lowtime) %>%
  mutate(ha_perc = ((tot_ha)/total_ticks)*100,
         v1_perc = ((tot_v1)/total_ticks)*100)

tot_percs = rbind(nymph_perc,adult_perc) %>%
  pivot_longer(c(ha_perc,v1_perc)) %>%
  mutate(keep = case_when(year>=2 & Lifestage=="Nymphs" ~ 1,
                          year>=3 & Lifestage=="Adults" ~ 1,
                          TRUE ~ 0)) %>%
  filter(keep==1)

ggplot(data = tot_percs,
       aes(x=year,y=value))+
  geom_bar(stat='identity')+
  facet_grid(name~Lifestage)

