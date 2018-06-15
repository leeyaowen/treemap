library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

dt<-read.csv("./ljcplot.csv")

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("LJC Map Plot"),
   
    sidebarPanel(
      selectInput("X1","x1",choices = c(26:55)),
      selectInput("Y1","y1",choices = c(17:38)),
      selectInput("xbase","x2",choices = c(1,2)),
      selectInput("ybase","y2",choices = c(1,2)),
      width = 3
    ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("MapPlot")
      )
   )


# Define server 
server <- function(input, output) {
  plotmap<-function(X1,Y1,xbase,ybase){
    
    if(X1>max(dt$x1)|X1<min(dt$x1)){
      stop("X1 not in range!")
    }else if(Y1>max(dt$y1)|Y1<min(dt$y1)){
      stop("Y1 not in range!")
    }else{
      
    }
    
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
    
    plotall<-bind_rows(plot_center,plot_NW,plot_N,plot_NE,plot_E,plot_SE,plot_S,plot_SW,plot_W)
    plotall<-filter(plotall,x3>=-100 & x3<=600 & y3>=-100 & y3<=600)

    xyline<-seq(50,450,50)

    p<-ggplot(plotall,aes(x=x3,y=y3))+
      theme(panel.background = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(face = "bold",hjust = 0.3),plot.caption = element_text(hjust = 0.8,size = 18))+
      ggtitle(paste("Quadrat No.(",X1,",",Y1,")","(",xbase,",",ybase,")", sep=""))+
      scale_x_continuous(limits = c(-150,650))+
      scale_y_continuous(limits = c(-150,650))+
      coord_fixed()

    for (i in 1:length(xyline)) {
      p<-p+geom_segment(x=xyline[i],y=0,xend=xyline[i],yend=500,colour="gray60",size=0.2)+
        geom_segment(x=0,y=xyline[i],xend=500,yend=xyline[i],colour="gray60",size=0.2)
    }
    p<-p+geom_segment(x=250,y=0,xend=250,yend=500,colour="black",size=0.5)+
      geom_segment(x=0,y=250,xend=500,yend=250,colour="black",size=0.5)+
      geom_rect(aes(ymax=500,ymin=0,xmax=500,xmin=0),alpha=0,size=0.7,colour="black")

    p<-p+geom_point(aes(size=dbh),shape=1,stroke=0.8,show.legend = F,colour=ifelse(plotall$dbh>0,"black","gray60"))+
      scale_size_continuous(range = c(0,20))+
      geom_text(aes(label=tag),hjust=-0.1,vjust=1.2,size=2.5,colour=ifelse(plotall$dbh>0,"black","gray60"))+
      labs(caption = "(     /     )")
    p
  }
  output$MapPlot<-renderPlot({
    plotmap(as.integer(input$X1),as.integer(input$Y1),as.integer(input$xbase),as.integer(input$ybase))
    },height = 620,width = 620)
}

# Run the application 
shinyApp(ui = ui,server=server)

