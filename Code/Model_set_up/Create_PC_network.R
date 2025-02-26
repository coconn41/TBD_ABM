# Create PC network

for(i in 1:8){ 
  network_df = network1 %>%
    filter(network_ID==i) %>%
    st_drop_geometry() %>%
    dplyr::select(-c(distance,inverse_sinuousity,network_ID)) %>%
    group_by(origin_ID,destination_ID) %>%
    summarize(lcp_distance = sum(lcp_distance),.groups="drop") %>%
    left_join(.,network1 %>%
                filter(network_ID==i) %>%
                st_drop_geometry() %>%
                dplyr::select(-c(inverse_sinuousity,network_ID)) %>%
                group_by(origin_ID,destination_ID) %>%
                summarize(distance = sum(distance),.groups="drop"))
  nam <- paste("network_ID_", i, sep = "")
  assign(nam, network_df)
  nam2 <- paste("network_graph_",i,sep="")
  network_graph_df = igraph::graph_from_data_frame(network_df,directed=TRUE,vertices = NULL)
  E(network_graph_df)$distance = (network_df$lcp_distance)
  assign(nam2, network_graph_df)
  
  for(a in 1:length(V(network_graph_df))){
    list_i = shortest_paths(network_graph_df,
                            from=V(network_graph_df)[a],
                            to=V(network_graph_df))
    bind = 0
    for(b in 1:length(list_i$vpath)){
      if(length(as.numeric(attributes(list_i$vpath[[b]])$names))==1){next}
      bind=bind+1
      if(bind==1){value=as.numeric(attributes(list_i$vpath[[b]][2])$names)} 
      if(bind>1){value2=as.numeric(attributes(list_i$vpath[[b]][2])$names)
      value=c(value2,value)}
    }
    if(a==1){direct_df = data.frame(origin_ID = rep(as.numeric(attributes(list_i$vpath[[b]][1])$names),
                                                    length(unique(value))),
                                    destination_ID = unique(value),
                                    network_ID = rep(i,length(unique(value))))}
    if(a>1){direct_df2 = data.frame(origin_ID = rep(as.numeric(attributes(list_i$vpath[[b]][1])$names),
                                                    length(unique(value))),
                                    destination_ID = unique(value),
                                    network_ID = rep(i,length(unique(value))))
    direct_df = rbind(direct_df,direct_df2)}
  }
  if(i==1){direct_df3 = direct_df}
  if(i>1){direct_df4 = direct_df
  direct_df3 = rbind(direct_df4,direct_df3)}
}
rm(direct_df,direct_df2,direct_df4)
jump_probability_df = direct_df3 %>%
  left_join(.,network1) %>%
  filter(is.na(lcp_distance)==F) %>%
  group_by(origin_ID,network_ID) %>%
  summarize(tot_sin = sum(inverse_sinuousity)) %>%
  left_join(.,direct_df3 %>%
              left_join(.,network1) %>%
              filter(is.na(lcp_distance)==F)) %>%
  mutate(probability = inverse_sinuousity/tot_sin)
write.csv(jump_probability_df,paste0(getwd(),'/Cached_data/jump_probability_df.csv'))
