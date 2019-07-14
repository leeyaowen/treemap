library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
dt<-read.csv(file.choose(),stringsAsFactors = FALSE)
#dt<-read.csv("./valley1output_20190629.csv",stringsAsFactors = FALSE)


#X1=x1,Y1=y1,xbase=x2,ybase=y2
plotmap<-function(X1,Y1,xbase,ybase){
  
  #防呆
  if(X1>max(dt$x1,na.rm = TRUE)|X1<min(dt$x1,na.rm = TRUE)){
    stop("X1 not in range!")
  }else if(Y1>max(dt$y1,na.rm = TRUE)|Y1<min(dt$y1,na.rm = TRUE)){
    stop("Y1 not in range!")
  }else{
    
  }
  
  #設定周圍小樣方
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
  }else if(xbase==1 & ybase==2){
    plot_center<-filter(dt,x1==X1,y1==Y1,x2==1,y2==2)
    plot_NW<-filter(dt,x1==X1-1,y1==Y1+1,x2==2,y2==1)
    plot_NW<-mutate(plot_NW,x3=x3-500,y3=y3+500)
    plot_N<-filter(dt,x1==X1,y1==Y1+1,x2==1,y2==1)
    plot_N<-mutate(plot_N,y3=y3+500)
    plot_NE<-filter(dt,x1==X1+1,y1==Y1+1,x2==1,y2==1)
    plot_NE<-mutate(plot_NE,x3=x3+500,y3=y3+500)
    plot_E<-filter(dt,x1==X1,y1==Y1,x2==2,y2==2)
    plot_E<-mutate(plot_E,x3=x3+500)
    plot_SE<-filter(dt,x1==X1,y1==Y1,x2==2,y2==1)
    plot_SE<-mutate(plot_SE,x3=x3+500,y3=y3-500)
    plot_S<-filter(dt,x1==X1,y1==Y1,x2==1,y2==1)
    plot_S<-mutate(plot_S,y3=y3-500)
    plot_SW<-filter(dt,x1==X1-1,y1==Y1,x2==2,y2==1)
    plot_SW<-mutate(plot_SW,x3=x3-500,y3=y3-500)
    plot_W<-filter(dt,x1==X1-1,y1==Y1,x2==2,y2==2)
    plot_W<-mutate(plot_W,x3=x3-500)
  }else if(xbase==2 & ybase==2){
    plot_center<-filter(dt,x1==X1,y1==Y1,x2==2,y2==2)
    plot_NW<-filter(dt,x1==X1,y1==Y1+1,x2==1,y2==1)
    plot_NW<-mutate(plot_NW,x3=x3-500,y3=y3+500)
    plot_N<-filter(dt,x1==X1,y1==Y1+1,x2==2,y2==1)
    plot_N<-mutate(plot_N,y3=y3+500)
    plot_NE<-filter(dt,x1==X1+1,y1==Y1+1,x2==1,y2==1)
    plot_NE<-mutate(plot_NE,x3=x3+500,y3=y3+500)
    plot_E<-filter(dt,x1==X1+1,y1==Y1,x2==1,y2==2)
    plot_E<-mutate(plot_E,x3=x3+500)
    plot_SE<-filter(dt,x1==X1+1,y1==Y1,x2==1,y2==1)
    plot_SE<-mutate(plot_SE,x3=x3+500,y3=y3-500)
    plot_S<-filter(dt,x1==X1,y1==Y1,x2==2,y2==1)
    plot_S<-mutate(plot_S,y3=y3-500)
    plot_SW<-filter(dt,x1==X1,y1==Y1,x2==1,y2==1)
    plot_SW<-mutate(plot_SW,x3=x3-500,y3=y3-500)
    plot_W<-filter(dt,x1==X1,y1==Y1,x2==1,y2==2)
    plot_W<-mutate(plot_W,x3=x3-500)
  }else if(xbase==2 & ybase==1){
    plot_center<-filter(dt,x1==X1,y1==Y1,x2==2,y2==1)
    plot_NW<-filter(dt,x1==X1,y1==Y1,x2==1,y2==2)
    plot_NW<-mutate(plot_NW,x3=x3-500,y3=y3+500)
    plot_N<-filter(dt,x1==X1,y1==Y1,x2==2,y2==2)
    plot_N<-mutate(plot_N,y3=y3+500)
    plot_NE<-filter(dt,x1==X1+1,y1==Y1,x2==1,y2==2)
    plot_NE<-mutate(plot_NE,x3=x3+500,y3=y3+500)
    plot_E<-filter(dt,x1==X1+1,y1==Y1,x2==1,y2==1)
    plot_E<-mutate(plot_E,x3=x3+500)
    plot_SE<-filter(dt,x1==X1+1,y1==Y1-1,x2==1,y2==2)
    plot_SE<-mutate(plot_SE,x3=x3+500,y3=y3-500)
    plot_S<-filter(dt,x1==X1,y1==Y1-1,x2==2,y2==2)
    plot_S<-mutate(plot_S,y3=y3-500)
    plot_SW<-filter(dt,x1==X1,y1==Y1-1,x2==1,y2==2)
    plot_SW<-mutate(plot_SW,x3=x3-500,y3=y3-500)
    plot_W<-filter(dt,x1==X1,y1==Y1,x2==1,y2==1)
    plot_W<-mutate(plot_W,x3=x3-500)
  }else{
    stop()
  }
  
  #結合周圍小樣方資料，限制輸出圖檔範圍
  plotall<-bind_rows(plot_center,plot_NW,plot_N,plot_NE,plot_E,plot_SE,plot_S,plot_SW,plot_W)
  plotall<-filter(plotall,x3>=-100 & x3<=600 & y3>=-100 & y3<=600)
  
  if(nrow(plotall)==0){
    plotfake<-data.frame(x1=X1,y1=Y1,x2=xbase,y2=ybase,tag="abc",sp="abc",dbh=0,x3=NA,y3=NA,stringsAsFactors = FALSE)
    plotall<-bind_rows(plotall,plotfake)
  }else{
    
  }
  
  #確認出圖
  print(paste("正在產生(",X1,",",Y1,")","(",xbase,",",ybase,")",sep=""))
  
  #設定格線
  xyline<-seq(50,450,50)
  
  #出圖
  p<-ggplot(plotall,aes(x=x3,y=y3))+
    theme(panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(face = "bold",hjust = 0.3),plot.caption = element_text(hjust = 0.8,size = 18))+
    ggtitle(paste("Quadrat No.(",X1,",",Y1,")","(",xbase,",",ybase,")", sep=""))+
    scale_x_continuous(limits = c(-150,650))+
    scale_y_continuous(limits = c(-150,650))+
    coord_fixed()
  
  #畫格線與外框
  for (i in 1:length(xyline)) {
    p<-p+geom_segment(x=xyline[i],y=0,xend=xyline[i],yend=500,colour="gray80",size=0.2)+
      geom_segment(x=0,y=xyline[i],xend=500,yend=xyline[i],colour="gray80",size=0.2)
  }
  p<-p+geom_segment(x=250,y=0,xend=250,yend=500,colour="black",size=0.5)+
    geom_segment(x=0,y=250,xend=500,yend=250,colour="black",size=0.5)+
    geom_rect(aes(ymax=500,ymin=0,xmax=500,xmin=0),alpha=0,size=0.7,colour="black")+
    labs(caption = "(     /     )")
  
  #畫點與字，存檔
  if(plotall[1,"sp"]!="abc"){
    p<-p+geom_point(aes(size=dbh),shape=1,stroke=0.4,show.legend = F,colour=ifelse(plotall$dbh>0,"black","gray50"))+
      scale_size_continuous(range = c(0, 20), limits = c(0,105))+
      geom_text_repel(aes(label=tag),hjust=-0.1,vjust=1.2,size=2.5,colour=ifelse(plotall$dbh>0,"black","gray50"))
    ggsave(filename=paste("Line",X1, "_", "plot(",X1,",",Y1,")","(",xbase,",",ybase,").pdf", sep=""), width = 210, height = 297, units = "mm")
  }else{
    ggsave(filename=paste("Line",X1, "_", "plot(",X1,",",Y1,")","(",xbase,",",ybase,").pdf", sep=""), width = 210, height = 297, units = "mm")
  }
}

#樣方迴圈
outputx2<-matrix(c(1,2))
outputy2<-matrix(c(1,2))
outquadrat<-function(X1,Y1){
  for (i in 1:length(outputx2)) {
    for (j in 1:length(outputy2)) {
      plotmap(X1,Y1,i,j)
    }
  }
}

#樣區迴圈
outputNX1<-matrix(c(0:14))
outputNY1<-matrix(c(7:20))
for(i in 1:length(outputNX1)){
  for(j in 1:length(outputNY1)){
    outquadrat(outputNX1[i],outputNY1[j])
  }
}
