Loc_metric_table_w_private = read_csv(paste0(getwd(),"/Data/TL_data/Loc_metric_table_w_private.csv"))[,-1] %>%
  st_as_sf(.,coords=c('longitude','latitude')) %>%
  st_set_crs(.,value=4326) %>%
  st_transform(.,crs=32618)

#####
# Load collection data and clean
#####
Collections = read_excel(paste0(getwd(),"/Data/TL_data/Babesia Tick Collections_10_15.xlsx")) %>%
  rename(Nymphs=`Nymph IS`,
         Females = FemaleIS,
         Males=`Male IS`,
         n_coll = `# Collectors`,
         avg_target = `Avg target`) %>%
  mutate(Adults = Males + Females,
         Year = as.numeric(substring(Date,1,4)),
         Month = as.numeric(substring(Date,6,7)))


Duplicate_visits = Collections %>%
  group_by(Site,Date) %>%
  summarize(tot = n()) %>%
  filter(tot>1) %>%
  mutate(ind = paste0(Site,Date))

# Need to combine into single visits:

Dup_colls = Collections %>%
  mutate(ind = paste0(Site,Date)) %>%
  filter(ind %in% Duplicate_visits$ind) %>%
  group_by(Site,Date) %>%
  summarize(Larvae = sum(Larvae),
            Nymphs = sum(Nymphs),
            Females = sum(Females),
            Males = sum(Males),
            Adults = Males + Females,
            `Nymph DV` = sum(`Nymph DV`),
            `Female DV` = sum(`Female DV`),
            `Male DV` = sum(`Male DV`),
            Other = NA,
            Unknowns = sum(Unknowns),
            `Site Totals` = sum(`Site Totals`),
            n_coll = sum(n_coll),
            Site = unique(Site),
            County = unique(County),
            Date = unique(Date),
            Month = as.numeric(substring(Date,6,7)),
            Year = as.numeric(substring(Date,1,4)),
            Town = unique(Town),
            ID = NA)

Collections = Collections %>%
  mutate(ind = paste0(Site,Date)) %>%
  filter(!ind%in%c(Duplicate_visits$ind)) %>%
  bind_rows(.,Dup_colls)

month_breakdown = data.frame(Month = c(1:12),
                             Target_Month = c("Adult",
                                              "Adult",
                                              "Adult",
                                              "Adult",
                                              "Both",
                                              "Nymph",
                                              "Nymph",
                                              "Nymph",
                                              "Nymph",
                                              "Adult",
                                              "Adult",
                                              "Adult"))


# Strings to categorize on:
# I. scapularis fall-questing adult target
# I. scapularis spring-questing adult target
# Nymph I. scapularis target
perc_fn = function(rt,Collections){
  paste0("Running total = ",rt, " of ",nrow(Collections), " Collections (",(round(rt/nrow(Collections)*100,2))," %)")
}
Clean_check1 = Collections %>%
  filter(., grepl('I. scapularis fall-questing adult target', Notes)) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = "Adult")
rt = nrow(Clean_check1)
perc_fn(rt,Collections)
Clean = Clean_check1

ltc = Collections %>%
  filter(!c(ID%in%c(Clean_check1$ID)))

Clean_check2 = Collections %>%
  filter(., grepl('I. scapularis spring-questing adult target', Notes)) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = "Adult")
rt = rt + nrow(Clean_check2)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check2)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check2$ID)))

Clean_check3 = Collections %>%
  filter(., grepl('Nymph I. scapularis target', Notes)) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = "Nymph")
rt = rt + nrow(Clean_check3)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check3)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check3$ID)))

Clean_check4 = ltc %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage =  ifelse((n_coll*avg_target!=0)&(n_coll*avg_target==Nymphs),"Nymph",
                                ifelse((n_coll*avg_target!=0)&(n_coll*avg_target)==Adults,"Adult",NA))) %>%
  filter(is.na(Target_stage)==F)
rt = rt + nrow(Clean_check4)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check4)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check4$ID)))

Clean_check5 = ltc %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage =  ifelse((n_coll*avg_target!=0)&round(n_coll*avg_target==Nymphs),"Nymph",
                                ifelse((n_coll*avg_target!=0)&round(n_coll*avg_target)==Adults,"Adult",NA))) %>%
  filter(is.na(Target_stage)==F)
rt = rt + nrow(Clean_check5)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check5)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check5$ID)))

Clean_check6 = ltc %>%
  filter(., grepl('longicornis', Notes)) %>%
  mutate(Target_genus = "Haemaphysalis",
         Target_species = "longicornis",
         Target_stage = "Non_IS")
rt = rt + nrow(Clean_check6)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check6)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check6$ID)))

Clean_check7 = ltc %>%
  filter(., grepl('variabilis', Notes)) %>%
  mutate(Target_genus = "Dermacentor",
         Target_species = "variabilis",
         Target_stage = "Non_IS") %>%
  filter(ID != 2187)

rt = rt + nrow(Clean_check7)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check7)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check7$ID)))

Clean_check8 = ltc %>%
  filter(ID == 2187) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = "Nymph")

rt = rt + nrow(Clean_check8)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check8)
ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check8$ID)))

Clean_check9 = ltc %>%
  filter(., grepl('americanum', Notes)) %>%
  mutate(Target_genus = "Amblyomma",
         Target_species = "americanum",
         Target_stage = "Non_IS") 

rt = rt + nrow(Clean_check9)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check9)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check9$ID)))

Clean_check10 = ltc %>%
  filter(., grepl('Amblyomma', Notes)) %>%
  mutate(Target_genus = "Amblyomma",
         Target_species = "americanum",
         Target_stage = "Non_IS") 

rt = rt + nrow(Clean_check10)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check10)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check10$ID)))

Clean_check11 = ltc %>%
  filter(., grepl("Larvae target stage", Notes)) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = "Larvae")

rt = rt + nrow(Clean_check11)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check11)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check11$ID)))

Clean_check12 = ltc %>%
  left_join(.,month_breakdown) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = ifelse(Target_Month=="Nymph","Nymph",
                               ifelse(Target_Month=="Adult","Adult",NA))) %>%
  filter(is.na(Target_stage)==F) %>%
  dplyr::select(-Target_Month)

rt = rt + nrow(Clean_check12)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check12)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check12$ID)))


Clean_check13 = ltc %>%
  filter(., grepl('Spring-questing adult', Notes)) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = "Adult")
rt = rt + nrow(Clean_check13)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check13)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check13$ID)))

Clean_check14 = ltc %>%
  left_join(.,month_breakdown) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage = ifelse(Nymphs > Adults,"Nymphs",
                               ifelse(Adults>Nymphs,"Adults",NA))) %>%
  filter(is.na(Target_stage)==F) %>%
  dplyr::select(-c(Target_Month))
rt = rt + nrow(Clean_check14)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check14)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check14$ID)))


Clean_check15 = ltc %>%
  left_join(.,month_breakdown) %>%
  mutate(Day = as.numeric(substring(Date,9,10))) %>%
  mutate(Target_genus = "Ixodes",
         Target_species = "scapularis",
         Target_stage =  ifelse(Day<=15,"Adult","Nymph")) %>%
  dplyr::select(-c(Target_Month,Day))
rt = rt + nrow(Clean_check15)
perc_fn(rt,Collections)
Clean = rbind(Clean,Clean_check15) %>%
  dplyr::select(County,Date,Site,Adults,Nymphs,Target_genus,Target_species,Target_stage)

ltc = ltc %>%
  filter(!c(ID%in%c(Clean_check15$ID)))

nrow(Collections)==nrow(ltc)+nrow(Clean)

#write.csv(Clean,file=paste0(getwd(),"/Data/TL_data/Cleaned_collections_by_date/Coll_clean_",substring(Sys.time(),1,10)))
Long_clean = Clean %>%
  rename(Adult = Adults,
         Nymph = Nymphs) %>%
  pivot_longer(c(Adult,Nymph),
               names_to = "Lifestage",
               values_to = "tot_collected") %>%
  filter(Lifestage==Target_stage)

remove(Clean_check1,Clean_check2,Clean_check3,Clean_check4,Clean_check5,Clean_check6,
       Clean_check7,Clean_check8,Clean_check9,Clean_check10,Clean_check11,Clean_check12,
       Clean_check13,Clean_check14,Clean_check15,month_breakdown,ltc)

fulldf = read_excel(paste0(getwd(),"/Data/TL_data/Tick_collection_table_all data_10_15.xlsx"))

Testing_results_co_infections = fulldf %>%
  rename(Date = tick_coll_date,
         Site = loc_name) %>%
  filter(tick_genus == "Ixodes",
         tick_species == "scapularis",
         tick_stage %in% c("Male","Female","Nymph"),
         tick_no_spec == 1,
         is.na(pcrl_final_result_ANA)==F) %>% #removes untested ticks
  mutate(ANA_result = ifelse(pcrl_final_result_ANA=="P",1,0),#Treats Indeterminates as negative
         ha = ifelse(is.na(ANA_genotype)==T,0,
                     ifelse(ANA_genotype=="ha",1,NA)),
         v1 = ifelse(is.na(ANA_genotype)==T,0,
                     ifelse(ANA_genotype=="v1"|ANA_genotype=="V1",1,0)),
         coinf = ifelse(ANA_genotype=="ha/v1",1,0),
         und = ifelse(is.na(ANA_genotype)==T,0,
                      ifelse(ANA_genotype=="Undetermined",1,0)),
         Lifestage = ifelse(tick_stage=="Male"|
                              tick_stage=="Female","Adult",tick_stage)) %>% 
  group_by(County,Date,Site,Lifestage) %>%
  summarize(ha = sum(ha,na.rm=T),
            v1 = sum(v1,na.rm=T),
            coinf = sum(coinf,na.rm=T),
            und = sum(und,na.rm=T),
            tot_tested = n())

selection_df = left_join(Long_clean,Testing_results_co_infections) %>%
  mutate(tot_tested = ifelse(is.na(tot_tested)==T,0,tot_tested))

