library(dplyr)
library(ggplot2)
library(ggthemes)
dt<-read.csv("./ÆV¤¯·ËÂI¦ì.csv")

plotmap<-function(X1,Y1,xbase,ybase){
  if(xbase==1 & ybase==1){
  plot_center<-filter(dt,x1==X1,y1==Y1,x2==1,y2==1)
  plot_NW<-filter(dt,x1==X1-1,y1==Y1,x2==2,y2==2)
  plot_NW<-mutate(plot_NW,x3=x3-500,y3=y3+500)
  plot_N<-filter(dt,x1==X1,y1==Y1,x2==1,y2==2)
  plot_N<-mutate(plot_N,y3=y3+500)
  plot_NE<-filter(dt,x1==X1,y1==Y1,x2==2,y2==2)
  plot_NE<-mutate(plot_NE,x3=x3+500,y3=y3+500)
  plot_E<-filter(dt,x1==X1,y1==Y1,x2==2,y2==1)
  plot_E<-mutate(plot_E,x3=x3+500)
  plot_SE<-filter(dt,x1==X1,y1==Y1-1,x2==2,y2==2)
  plot_SE<-mutate(plot_SE,x3=x3+500,y3=y3-500)
  plot_S<-filter(dt,x1==X1,y1==Y1-1,x2==1,y2==2)
  plot_S<-mutate(plot_S,y3=y3-500)
  plot_SW<-filter(dt,x1==X1-1,y1==Y1-1,x2==2,y2==2)
  plot_SW<-mutate(plot_SW,x3=x3-500,y3=y3-500)
  plot_W<-filter(dt,x1==X1-1,y1==Y1,x2==2,y2==1)
  plot_W<-mutate(plot_W,x3=x3-500)
  }else{
    stop()
  }
  plotall<-bind_rows(plot_center,plot_NW,plot_N,plot_NE,plot_E,plot_SE,plot_S,plot_SW,plot_W)
  plotall<-filter(plotall,x3>=-100 & x3<=600 & y3>=-100 & y3<=600)
 
  ggplot(plotall,aes(x=x3,y=y3))+
    geom_point(aes(size=dbh),shape=1,stroke=1.1,show.legend = F)+geom_text(aes(label=tag),hjust=-0.1,vjust=1.2,size=3)+
    geom_rect(aes(ymax=500,ymin=0,xmax=500,xmin=0),alpha=0,size=1,colour="black")+
    geom_segment(x=0,y=250,xend=500,yend=250,colour="black")+
    geom_segment(x=250,y=0,xend=250,yend=500,colour="black")+
    theme(panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust = 0.5))+
    ggtitle("(26,17)\n (1,1)")+
    scale_x_continuous(limits = c(-150,650))+
    scale_y_continuous(limits = c(-150,650))+
    coord_fixed()
}





