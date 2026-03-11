library(tidyverse)
rm(list=ls())

# # save networks:
# 
# load(paste0(getwd(),'/Simulations/Network_1/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run1.RData'))
# unq = unique(tick_data2$layer)
# dat = data.frame(layer = unq,
#            network_ID = rep(1,length(unq)))
# write.csv(dat,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1.csv'))
# load(paste0(getwd(),'/Simulations/Network_6/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run1.RData'))
# unq = unique(tick_data2$layer)
# dat = data.frame(layer = unq,
#                  network_ID = rep(6,length(unq)))
# write.csv(dat,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6.csv'))
# load(paste0(getwd(),'/Simulations/Network_7/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run1.RData'))
# unq = unique(tick_data2$layer)
# dat = data.frame(layer = unq,
#                  network_ID = rep(7,length(unq)))
# write.csv(dat,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7.csv'))
# 
# #####
# # TRACK UNIQUE STARTING PATCHES 66900 508514 624001 623983
# #####
# load(paste0(getwd(),'/Simulations/Network_1/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run1.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1_run1.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_1/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run2.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1_run2.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_1/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run3.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1_run3.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_1/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run4.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1_run4.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_1/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run5.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1_run5.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_6/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run1.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6_run1.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_6/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run2.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6_run2.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_6/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run3.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6_run3.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_6/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run4.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6_run4.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_6/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run5.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6_run5.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_7/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run1.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7_run1.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_7/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run2.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7_run2.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_7/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run3.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7_run3.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_7/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run4.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7_run4.csv'))
# 
# load(paste0(getwd(),'/Simulations/Network_7/Multi_runs/timestep_65700_attach_25_path_trans_100alt_run5.RData'))
# tick_data3 = tick_data2 %>%
#   filter(layer %in% c(66900,508514,624001,623983))
# write.csv(tick_data3,paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7_run5.csv'))
# ind=0
# for(i in c(1,6,7)){
#   for(j in c(1:5)){
#     ind=ind+1
#     if(i==7&j==4){break}
#     dat = read.csv(paste0(getwd(),'/Code/Figures_GIFS/Init_layer_runs/layer_dat_net',i,'_run',j,'.csv'))
#     if(ind==1){dat2 = dat}
#     if(ind>1){dat2 = rbind(dat,dat2)}
#   }
# }
# 
# tick_data2 = dat2 %>%
#   filter(Lifestage!="") %>%
#   mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
#                                Lifestage=="Adult" ~ "Adults",
#                                TRUE ~ Lifestage),
#          Lifestage = factor(Lifestage,
#                             levels = c("Eggs","Larvae","Nymphs","Adults")),
#          simulation_day = (day_of_year+(year*365))-264,
#          simulation_week = ceiling(simulation_day/7))
# 
# cohort_data = tick_data2 %>%
#   filter(year!=0) %>%
#   mutate(simulation_day = (day_of_year+(year*365))-264) %>%
#   group_by(simulation_day,Lifestage,day_of_year,year,network_ID) %>% # simulation_day
#   reframe(total_ticks = round(mean(total_ticks)),
#           tot_v1 = round(mean(tot_v1)),
#           tot_ha = round(mean(tot_ha))) %>%
#   mutate(cohort = case_when(year == 1 & Lifestage == "Adults" ~ 0,
#                             year == 1 & Lifestage == "Nymphs" ~ 0,
#                             year == 1 & Lifestage %in% c("Eggs","Larvae") ~ 1,
#                             year == 2 & Lifestage == "Larvae" & day_of_year <= 111 ~ 1,
#                             year == 2 & Lifestage == "Adults" & day_of_year < 190 ~ 0,
#                             TRUE ~ -1))
# 
# 
# for(i in 2:max(cohort_data$year)){
#   cohort_data = cohort_data %>%
#     mutate(cohort = case_when(cohort == -1 & year == i & Lifestage == "Adults" & day_of_year < 190 ~ i-2,
#                               cohort == -1 & year == i & Lifestage == "Nymphs" ~ i-1,
#                               cohort == -1 & year == i & Lifestage == "Adults" & day_of_year >= 190 ~ i-1,
#                               cohort == -1 & year == i & Lifestage == "Eggs" ~ i,
#                               cohort == -1 & year == i & day_of_year <= 171 & Lifestage == "Larvae" ~ i-1,
#                               cohort == -1 & year == i & day_of_year > 171 & Lifestage == "Larvae" ~ i,
#                               TRUE ~ cohort))
# }
# full_cohort_data = cohort_data
# for(k in c(1,6,7)){
#   cohort_data = full_cohort_data %>%
#     filter(network_ID==k)
# for(i in 1:max(cohort_data$cohort)){
#   ch = cohort_data %>%
#     filter(cohort==i)
#   if(nrow(ch)==0){next}
#   eg = expand.grid(simulation_day = min(ch$simulation_day):max(ch$simulation_day),
#                    Lifestage = c("Eggs","Larvae","Nymphs","Adults"),
#                    cohort = i)
#   ch = ch %>%
#     left_join(eg,.) %>%
#     mutate(total_ticks = ifelse(is.na(day_of_year)==T,0,total_ticks))
#   
#   minls = ch %>%
#     filter(is.na(day_of_year)==F,
#            Lifestage!="Adults",
#            Lifestage!="Eggs") %>%
#     group_by(Lifestage) %>%
#     summarize(mn = min(simulation_day),
#               # mn2 = min(day_of_year),
#               yr = min(year)) %>%
#     ungroup() %>%
#     mutate(simulation_day = mn,
#            cohort = i,
#            # day_of_year = mn2,
#            year = yr) %>%
#     select(-c(mn,yr))
#   
#   minls2 = ch %>%
#     filter(is.na(day_of_year)==F,
#            #  day_of_year>250+(365*(i-1)),
#            Lifestage=="Adults") %>%
#     summarize(mn = min(simulation_day),
#               # mn2 = min(day_of_year),
#               yr = min(year)) %>%
#     ungroup() %>%
#     mutate(simulation_day = mn,
#            cohort = i,
#            #day_of_year = mn2,
#            year = yr) %>%
#     select(-c(mn,yr)) %>%
#     mutate(Lifestage="Adults")
#   
#   
#   newdat = minls %>%
#     rbind(.,minls2) %>%
#     left_join(.,ch) %>%
#     mutate(Lifestage = case_when(Lifestage=="Larvae" ~ "Eggs",
#                                  Lifestage=="Nymphs" ~ "Larvae",
#                                  Lifestage=="Adults" ~ "Nymphs",
#                                  TRUE ~ Lifestage))
#   ch = ch %>%
#     bind_rows(.,newdat)
#   
#   if(i==1){ch2 = ch
#   newdat2 = newdat}
#   if(i>1){ch2 = rbind(ch2,ch)
#   newdat2 = rbind(newdat,newdat2)}
# }
# if(k==1){ch3 = ch2
#   newdat3 = newdat2}
# if(k>1){ch3 = rbind(ch3,ch2)
#   newdat3 = rbind(newdat3,newdat2)}
# }
# 
# ch2 = ch3 %>%
#   filter(is.na(day_of_year)==F) %>%
#   mutate(Lifestage = factor(Lifestage,levels=c("Eggs","Larvae","Nymphs","Adults")))
# 
# # newdat_filter = newdat2 %>%
# #   filter(is.na(day_of_year)==F,
# #          )
# 
# # ch_barplot = ch %>%
# #   filter(Lifestage!="Eggs",
# 
# 
# ch = ch2 %>%
#   anti_join(.,newdat2) %>%
#   mutate(tot_v1 = ifelse(is.na(tot_v1)==T,0,tot_v1),
#          tot_ha = ifelse(is.na(tot_ha)==T,0,tot_ha),
#          uninfected = total_ticks - tot_v1 - tot_ha) %>%
#   pivot_longer(c(tot_v1,tot_ha,uninfected)) %>%
#   mutate(name = case_when(name == "uninfected" ~ "Uninfected",
#                           name == "tot_ha" ~ "Ap-ha",
#                           name == "tot_v1" ~ "Ap-v1",
#                           TRUE ~ "NA"),
#          name = factor(name,levels=c("Uninfected","Ap-ha","Ap-v1")))
# 
# perc_plot = ch2 %>%
#   anti_join(.,newdat2) %>%
#   filter(Lifestage!="Eggs") %>%
#   mutate(perc_v1 = (tot_v1/total_ticks)*100,
#          perc_ha = (tot_ha/total_ticks)*100) %>%
#   pivot_longer(c(perc_ha,perc_v1)) %>%
#   mutate(name = case_when(name == "perc_ha" ~ "Ap-ha (%)",
#                           name == "perc_v1" ~ "Ap-v1 (%)",
#                           TRUE ~ "NA"))

# rm(list=ls())
# source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))
# set.seed(1)
# source(paste0(getwd(),'/Code/Model_set_up/Load_model_environment.R'))

# starting_at_patches = tick_agents %>%
#   select(Lifestage,layer,County,Site,network_ID) %>%
#   rbind(nymph_agents%>%
#           select(Lifestage,layer,County,Site,network_ID)) %>%
#   filter(network_ID %in% c(1,6,7)) %>%
#   group_by(Lifestage,layer,County,Site,network_ID) %>%
#   summarize(tot = n()) %>%
#   filter(is.na(County)==F) 
# 
# unique_starting_patches = unique(starting_at_patches$layer) 
# 
# areas = reduced_patches %>%
#   filter(layer %in% starting_at_patches$layer,
#          is.na(loc_county)==F) %>%
#   select(hectare,layer,loc_county,loc_name) %>%
#   rename(Site = 'loc_name',
#          County = 'loc_county')
# 
# starting_at_patches = starting_at_patches %>%
#   left_join(.,areas)
# starting_at_patches$area = units::set_units(starting_at_patches$hectare,ha)
# starting_at_patches$area = units::set_units(starting_at_patches$area,m2)
# attributes(starting_at_patches$area)=NULL
# 
# starting_at_patches = starting_at_patches %>%
#   ungroup() %>%
#   group_by(network_ID,Lifestage) %>%
#   summarize(density = sum(tot,na.rm=T)/sum(area,na.rm=T))
# 
# # layer_dat = bind_rows(read.csv(paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net1.csv')),
# #                   read.csv(paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net6.csv'))) %>%
# #                     bind_rows(.,read.csv(paste0(getwd(),'/Code/Figures_GIFS/layer_dat_net7.csv'))) %>%
# #   filter(layer!=0)
# 
# tm_patches = read_sf(paste0(getwd(),'/Cached_data/Reduced_patches.shp')) %>%
#   rename(Location_ID = "Lctn_ID",
#          loc_county = "lc_cnty",
#          loc_name = "loc_nam",
#          gridrows = "gridrws",
#          gridcols = "gridcls",
#          deer_agents = "dr_gnts",
#          deer_p_ha = "der_p_h",
#          mouse_agents = "ms_gnts",
#          mice_p_ha = "mic_p_h",
#          gridrows_adjusted = "grdrws_",
#          gridcols_adjusted = "grdcls_",
#          deer_agents_adjusted = "dr_gnt_",
#          mouse_agents_adjusted = "ms_gnt_",
#          patch_type = "ptch_ty") %>%
#   filter(layer %in% reduced_patches$layer) %>%
#   filter(!duplicated(layer)) %>%
#   left_join(.,layer_dat) %>%
#   filter(is.na(network_ID)==F)
# 
# tm_patches$area = tm_patches %>%
#   st_area()
# attributes(tm_patches$area)=NULL
# 
# dens_calc = tm_patches %>%
#   st_drop_geometry() %>% 
#   group_by(network_ID) %>%
#   summarize(tot_area = sum(area,na.rm = T)) %>%
#   dplyr::select(network_ID,tot_area)


starting_dat = read.csv(paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/starting_tick_data.csv')) %>%
  mutate(network_ID = factor(ifelse(network_ID==6,"Network 2",
                                    ifelse(network_ID==7,"Network 3","Network 1")),
                             levels = c("Network 1","Network 2","Network 3")),
         EoO = factor(ifelse(Lifestage=="Adult","Even years","Odd years"),
                      levels = c("Even years","Odd years")))


for(i in c(1,6,7)){
  #combine all
   #all_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
  #                                 i,"_all_combined.csv"))
  # mouse_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
  #                                 i,"_mouse_combined.csv"))
  # deer_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
  #                                 i,"_deer_combined.csv"))
  ticks_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
                                    i,"_ticks_combined.csv")) %>%
    mutate(network_ID = i)
  if(i == 1){ticks_combined1 = ticks_combined
  # mouse_combined1 = mouse_combined
  # deer_combined1 = deer_combined
  # all_combined1 = all_combined
  }
  if(i > 1){ticks_combined1 = rbind(ticks_combined,ticks_combined1)
  #mouse_combined1 = rbind(mouse_combined,mouse_combined1)
  #deer_combined1 = rbind(deer_combined,deer_combined1)
  #all_combined1 = rbind(all_combined,all_combined1)
  }
}

ticks_dat_fin = ticks_combined1 %>%
  mutate(Lifestage = factor(Lifestage,levels=c("Eggs","Larvae","Nymphs","Adults")),
         network_ID = factor(ifelse(network_ID==6,"Network 2",
                                    ifelse(network_ID==7,"Network 3","Network 1")),
                             levels = c("Network 1","Network 2","Network 3"))) %>%
  ungroup() %>%
  group_by(network_ID,simulation_day,Lifestage,cohort) %>%
  summarize(total_ticks = mean(total_ticks,na.rm=T)) %>%
  mutate(EoO = factor(ifelse(cohort %% 2 == 1, "Even years","Odd years"),
                 levels=c("Even years","Odd years")),
         cohort = factor(case_when(cohort == 1 ~ "Cohort 1",
                                   cohort == 2 ~ "Cohort 2",
                                   cohort == 3 ~ "Cohort 3",
                                   cohort == 4 ~ "Cohort 4",
                                   cohort == 5 ~ "Cohort 5",
                                   TRUE ~ NA),levels=c("Cohort 1",
                                                       "Cohort 3",
                                                       "Cohort 5",
                                                       "Cohort 2",
                                                       "Cohort 4")),)
ticks_dat_fin2 = ticks_combined1 %>%
  mutate(Lifestage = factor(Lifestage,levels=c("Eggs","Larvae","Nymphs","Adults")),
         EoO = factor(ifelse(cohort %% 2 == 1, "Even years","Odd years"),
                      levels=c("Even years","Odd years")),
         cohort = factor(case_when(cohort == 1 ~ "Cohort 1",
                                   cohort == 2 ~ "Cohort 2",
                                   cohort == 3 ~ "Cohort 3",
                                   cohort == 4 ~ "Cohort 4",
                                   cohort == 5 ~ "Cohort 5",
                                   TRUE ~ NA),levels=c("Cohort 1",
                                                       "Cohort 3",
                                                       "Cohort 5",
                                                       "Cohort 2",
                                                       "Cohort 4")),
         network_ID = factor(ifelse(network_ID==6,"Network 2",
                                    ifelse(network_ID==7,"Network 3","Network 1")),
                             levels = c("Network 1","Network 2","Network 3")))  

# This is the end of the burn-in period:
ticks_dat_fin %>% group_by(EoO) %>% summarize(min = min(simulation_day))

p1=ggplot(data = ticks_dat_fin,
       aes(x = simulation_day,
           y = total_ticks))+
  geom_col(aes(#group = cohort,#interaction(cohort,run_number),
                color = Lifestage,
                fill = Lifestage))+
  # geom_point(data = ticks_dat_fin2,
  #           aes(x = simulation_day,
  #               y = total_ticks,
  #               group = interaction(cohort,run_number)),
  #           size=1,
  #           alpha=.5)+
  # geom_smooth(data = ticks_dat_fin2,
  #             aes(x = simulation_day,
  #                 y = total_ticks),se=T,span=.1,method='loess',
  #                # group = interaction(EoO,network_ID)),#,
  #                # group = EoO),
  #             inherit.aes=F)+
  # geom_hline(data = starting_dat,
  #            aes(y=))
  facet_grid(network_ID ~ EoO,scales='free')+
  geom_vline(xintercept = 577,
             linewidth=1.5)+ # burn-in
  theme_bw()+
  scale_fill_manual("Life stage",values = c(tmaptools::get_brewer_pal(n=4,"Dark2")))+
  scale_color_manual("Life stage",values = c(tmaptools::get_brewer_pal(n=4,"Dark2")))+
  ylab("Total ticks")+
  xlab("Simulation day")+
  theme(text = element_text(size=30));p1



ggsave(p1,filename=paste0(getwd(),'/Figures/Figures/Cohort_plot.jpeg'),
       dpi=300,
       height = 10,
       width=14,units='in')

# ggplot(data = ticks_combined1 %>%
#          mutate(Lifestage = factor(Lifestage,levels=c("Eggs","Larvae","Nymphs","Adults")),
#                 network_ID = factor(ifelse(network_ID==6,"Network 2",
#                                            ifelse(network_ID==7,"Network 3","Network 1")),
#                                     levels = c("Network 1","Network 2","Network 3"))) %>%
#          filter(Lifestage %in% c("Nymphs","Adults")),
#        aes(x=simulation_day,
#            y = total_ticks))+
#   geom_line(aes(group = interaction(cohort,run_number),
#             color = Lifestage))+
#   #geom_smooth(method='loess',span=.1)+
#   facet_grid(network_ID ~ .,scales='free_y')+
#   theme_bw()+
#   scale_color_manual("Life stage",values = c(tmaptools::get_brewer_pal(n=4,"Dark2")))+
#   ylab("Total ticks")+
#   xlab("Simulation day")


