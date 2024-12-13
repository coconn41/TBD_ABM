mouse_movement = function(mouse_agents,daytime){if(daytime=="day"){mouse_agents <<- mouse_agents %>%
  mutate(movement = case_when(row==gridrows & !col%in%c(1,gridcols) ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
                                   row==1 & !col%in%c(1,gridcols) ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
                                   col==gridcols & !row%in%c(1,gridrows) ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
                                   col==1 & !row%in%c(1,gridrows) ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
                                   row==gridrows & col==gridcols ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
                                   row==gridrows & col==1 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
                                   row==1 & col==gridcols ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
                                   row==1 & col==1 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
                                   TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
         # movement = case_when(possibilities==2 ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
         #                      possibilities==3 ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
         #                      possibilities==4 ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
         #                      possibilities==5 ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
         #                      possibilities==6 ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
         #                      possibilities==7 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
         #                      possibilities==8 ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
         #                      possibilities==9 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
         #                      TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
         row = case_when(movement==1 ~ row-1,
                         movement==2 ~ row-1,
                         movement==3 ~ row-1,
                         movement==7 ~ row+1,
                         movement==8 ~ row+1,
                         movement==9 ~ row+1,
                         TRUE ~ row),
         col = case_when(movement==1 ~ col-1,
                         movement==3 ~ col+1,
                         movement==4 ~ col-1,
                         movement==6 ~ col+1,
                         movement==7 ~ col-1,
                         movement==9 ~ col+1,
                         TRUE ~ col),
         locs = paste0(row,",",col,",",network_ID))}
}
#To test, be sure to change the function assignment from "mouse_agents" to "test_mice"
# test_mice = head(mouse_agents,1000)
# test_mice$gridcols=1000
# test_mice$gridrows=1000
# test_mice$row = round(runif(1000,min=1,max=1000))
# test_mice$col = round(runif(1000,min=1,max=1000))
# for(i in 1:300){
#   mouse_movement(test_mice,daytime)
#   df = data.frame(x = test_mice$col,
#                   y = test_mice$row,
#                   id = c(1:1000),
#                   time = rep(i,1000))
#   if(i==1){df2 = df}
#   if(i>1){df2 = rbind(df,df2)}
# }
# 
# ggplot(data = df2,
#        aes(x=x,
#            y=y,
#            group=as.factor(id)))+
#   geom_point()+
#   transition_time(time)
#   
