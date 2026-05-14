library(tidyverse)
#####
# Load data
#####
if(use_cached_sweep_results == T){df2 <- read_csv(paste0(getwd(),"/Complete_parameter_sweeps/Pulled_sweep_data.csv"))
}else{
Networks = paste0("Network_",rep(1:8))
for(i in unique(Networks)){
    files = list.files(paste0(getwd(),'/Complete_parameter_sweeps/',
                              i,'/Round_1/'))[-1]
    if(length(files)==0){next}
    files = paste0(i,"/Round_1/",files)
    if(i==Networks[1]){files2=files}else(files2=c(files2,files))
}
files = files2;rm(files2)
basevals = c("dd_0.1_",
             "md_0.1_",
             "td_10_",
             "mcc_25_",
             "dcc_25_",
             "dap_0.1_",
             "map_0.1_")
ind = 0
for(i in unique(files)){
  ind = ind + 1 
  df <- read.csv(paste0(getwd(),'/Complete_parameter_sweeps/',i))
  max_ts = max(df$timestep,na.rm = T)
  fin_tt = sum(subset(df,timestep == max(df$timestep, na.rm = T))$total_ticks)
  df1 = data.frame(Network = as.numeric(substring(i,9,9)),
                   deer_density = as.numeric(str_extract(i, "(?<=dd_)\\d+\\.?\\d*")),
                   mouse_density = as.numeric(str_extract(i, "(?<=md_)\\d+\\.?\\d*")),
                   tick_density = as.numeric(str_extract(i, "(?<=td_)\\d+\\.?\\d*")),
                   mouse_carrying_capacity = as.numeric(str_extract(i, "(?<=mcc_)\\d+\\.?\\d*")),
                   deer_carrying_capacity = as.numeric(str_extract(i, "(?<=dcc_)\\d+\\.?\\d*")),
                   deer_attach_prob = as.numeric(str_extract(i, "(?<=dap_)\\d+\\.?\\d*")),
                   mouse_attach_prob = as.numeric(str_extract(i, "(?<=map_)\\d+\\.?\\d*")),
                   max_timestep = ifelse(nrow(df)==0,0,max_ts),
                   final_total_ticks = ifelse(nrow(df)==0,1000000,fin_tt),
                   end = ifelse(fin_tt<1000000 & max_ts != (8760*10),"dieoff",
                                ifelse(fin_tt<1000000 & max_ts == (8760*10),"finished","overpopulation")))
  if(ind == 1){df2 = df1}
  if(ind > 1){df2 = rbind(df2,df1)}
}
write.csv(df2,paste0(getwd(),'/Complete_parameter_sweeps/Pulled_sweep_data.csv'))
}
Parameter_set = df2 %>% filter(end == "finished")
