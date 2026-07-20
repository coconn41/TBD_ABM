library(tidyverse)
library(sf)
library(mgcv)
library(readr)
#####
# Load packages loop:
#####
ind <- 0
for(i in 1:8){
  if(i==3){next}
  files <- list.files(paste0(getwd(),"/Simulations/Network_",i,"/Results"))[-1]
  for(j in 1:length(files)){
    print(paste0("Network ",i,", file ",j))
    ind <- ind + 1
    df <- read_csv(paste0(getwd(),"/Simulations/Network_",
    i,"/Results/",files[j])) %>%
      select(-c("...1",Agent,network_ID)) %>%
      mutate(sim_number = ind)
    if(str_detect(files[j],"finished")==TRUE){df$outcome = "finished"}
    if(str_detect(files[j],"dieoff")==TRUE){df$outcome = "dieoff"}
    if(str_detect(files[j],"overpop")==TRUE){df$outcome = "overpop"}
    if(ind == 1){df2 <- df}
    if(ind > 1){df2 <- rbind(df,df2)
    print(paste0(nrow(df2)," total rows"))}
  }
}
write.csv(df2,file = paste0(getwd(),'/Simulations/Full_sim_df.csv'))
