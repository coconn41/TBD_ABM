library(terra)
library(ggplot2)
library(dplyr)
library(ggpubr)

#####
# Deer example
#####

baserast = rast(nrows = 250,
                ncols = 250,
                xmin = 0,
                xmax = 250,
                ymin = 0, 
                ymax = 250)
values(baserast) = 0
df = as.data.frame(baserast,xy=TRUE)

set.seed(3)
agent = data.frame(x = c(125,runif(n=1,min=125-100,max=125+100)),
                   y = c(125,runif(n=1,min=125-100,max=125+100)),
                   timestep = c(1,2))

lines = data.frame(x = c(25,25,225,225,25),
                   y = c(25,225,225,25,25))

p1=ggplot(data = df,
       aes(x,y))+
  geom_tile(fill='forestgreen')+
 # geom_tile(col='black',alpha=.1)+
  coord_fixed()+
  geom_point(data = agent,aes(x,y),size=1,color="#854a15")+
  geom_point(data = agent,aes(x,y),color="#854a15")+
  geom_text(data = agent[1,],aes(x,y,label = paste0("t = ",timestep)),fontface = 'italic',nudge_y = -10)+
  geom_text(data = agent[2,],aes(x,y,label = paste0("t = ",timestep)),fontface = 'italic',nudge_y = 10)+
  geom_line(data = agent,aes(x,y),color="#854a15")+
  geom_path(data = lines,aes(y,x),linewidth=1,color='black')+
  scale_x_continuous(breaks=c(1,seq(50,250,by=50)))+
  scale_y_continuous(breaks=c(1,seq(50,250,by=50)))+
  xlab("Grid cells (columns)")+
  ylab("Grid cells (rows)")+
  theme_classic()+
  theme(line = element_blank())+
  ggtitle("A.");p1


#####
# Mouse example:
#####

baserast2 = rast(nrow=7,
                ncol=7,
                xmin=0,
                xmax=7,
                ymin=0,
                ymax=7)

values(baserast2) = 0
df2 = as.data.frame(baserast2,xy=TRUE)

set.seed(2)
agent2 = data.frame(col = 3.5,
                    row = 3.5,
                    gridcols = 6.5,
                    gridrows = 6.5,
                    timestep = 0)
agent1 = agent2
for(i in 1:5){
  agent2 = agent2 %>%
    mutate(movement = case_when(row==gridrows & !col%in%c(1,gridcols) ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
                                row==1 & !col%in%c(1,gridcols) ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
                                col==gridcols & !row%in%c(1,gridrows) ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
                                col==1 & !row%in%c(1,gridrows) ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
                                row==gridrows & col==gridcols ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
                                row==gridrows & col==1 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
                                row==1 & col==gridcols ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
                                row==1 & col==1 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
                                TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
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
           timestep = i)
  if(i==1){agent3 = agent2}
  if(i>1){agent3 = rbind(agent3,agent2)}
}
agent3 = agent3 %>% 
  dplyr::select(-movement) %>%
  bind_rows(agent1,.) %>%
  mutate(row = row+.5,
         col = col+.5)

df2$x = df2$x+.5
df2$y = df2$y+.5
p2=ggplot(data = df2,
       aes(x,y))+
  geom_tile(fill='forestgreen')+
  geom_tile(col='black',alpha=.1)+
  coord_fixed()+
  geom_point(data = agent3[c(1,6),],aes(x=col,y=row),size=2.5,color='grey')+
  geom_point(data = agent3[c(2:5),],aes(x=col,y=row),size=1.25,color='grey')+
  geom_path(data = agent3,aes(x=col,y=row),alpha=.5,color='grey',linewidth=1)+
  geom_text(data = agent3 %>% filter(timestep%in%c(1,3,4)),aes(x=col,y=row,label=paste0("t = ",timestep)),fontface = 'italic',nudge_y = .22)+
  geom_text(data = agent3 %>% filter(timestep%in%c(0,2,5)),aes(x=col,y=row,label=paste0("t = ",timestep)),fontface = 'italic',nudge_y = -.22)+
  scale_x_continuous(breaks=c(1:7))+
  scale_y_continuous(breaks=c(1:7))+
  xlab("Grid cells (columns)")+
  ylab("Grid cells (rows)")+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())+
  ggtitle("B.")

p3 = ggarrange(p1,p2)
ggsave(p3,
       filename = paste0(getwd(),"/Figures/Figures/Movement_fig.jpeg"),
       dpi = 300)  
