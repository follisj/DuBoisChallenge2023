## challenge 5

## packages
library(tidyverse)
library(ggfx)
library(patchwork)
library(ggtext)

## function to create boxes
plot.boxes2 <- function(pal.color,pal.color2,seed){
  ggplot()+
    geom_polygon(data=data.frame(
      x=c(0,0,1,1),y=c(0,1,1,0)),
      aes(x,y),fill=pal.color
    )+
    with_blur(geom_line(data=data.frame(
      x=runif(1000,0,1),
      y=c(runif(250,0,.35),runif(250,.25,.55),runif(250,.5,.85),runif(250,.75,1)),
      group=rep(1:500,each=2)
    ),
    aes(x,y,group=group),col=pal.color2,size=3,alpha=.05),sigma=unit(2,'mm'))+
    geom_line(data=data.frame(
      x=runif(444,0,1),
      y=seq(0.102,.988,.002),
      group=rep(1:222,each=2)
    ),
    aes(x,y,group=group),col=pal.color,alpha=.25,size=1)+
    theme_void()
}

## color palette
## https://redketchup.io/color-picker - for matching colors
## https://hslpicker.com/#d49ba6 - to get the lighter hues

pal.color <- data.frame(
  x=c("#005736", "#ECBB65","#BE0023","#003E8B",
               "#060905","#362114","#005736","#BE0023"),
               y=c("#9fcbba","white","#d49ba6","#6d95c5",
                          "gray30","#86614A","#9fcbba","#d49ba6"),
                          y1=c(12,24,36,48,60,72,84,92)         
)

## create the boxes for the viz
box.names <- c("GM","GR","BA","UN","BC","DR","PU","BU")
for(i in 1:length(box.names)){
  xxx.name <- box.names[i]
  assign(xxx.name,plot.boxes2(pal.color$x[i],pal.color$y[i],pal.color$y1[i]))
}

## dataset for the category labels
data <- data.frame(
  cats = c("Estimated capital",
           "Capital évalué",
           "General merchandise stores",
           "Magazines de provisions",
           "d'objects divers",
           "Grocers",
           "Epiciers",
           "Bankers",
           "Banquiers",
           "Undertakers","Entrepreneurs de pompes",
           "funebres",
           "Building contractors",
           "Entrepreneurs de batiments",
           "Druggists",
           "Pharmaciens",
           "Publishers",
           "Editeurs",
           "Building and loan associations",
           "Institutions financiers co-oper-",
           "atives"),
  x=c(rep(1.5,21)),
  y=c(9.83,9.57,
      8.83,8.57,8.4,
      7.66,7.4,
      6.66,6.4,
      5.66,5.4,5.23,
      4.46,4.2,
      3.46,3.2,
      2.46,2.2,
      1.46,1.2,1.03),
  hjust=c(0,0,0,0,-.25,0,0,0,0,0,0,-.25,0,0,0,0,0,0,0,0,-.25)
)

## dataset for the lines to the boxes
data2 <- data.frame(
  x=c(4.2,8.7,
      4.2,8.7,
      4.2,5.7,
      4.2,4.6,
      4.2,4.7,
      4.2,8.2,8.2,11.7,
      4.2,4.7,
      4.2,4.4,4.4,8.7),
  y=c(8.7,8.7,
      7.53,7.53,
      6.53,6.53,
      5.53,5.53,
      4.33,4.33,
      3.23,3.23,2.33,2.33,
      2.33,2.33,
      1.33,1.33,.33,.33),
  group=c("1","1","2","2","3","3","4","4","5","5",
          "6","6","6","6","7","7","8","8","8","8")
)

## code for the viz
## used html for the title and subtitles

ggplot(data,aes(x=x,y=y))+
  annotate("rect",xmin=4.2,xmax=16,ymin=0,ymax=11,alpha=.05)+ #create shaded area under plot
  geom_text(aes(label=cats,hjust=hjust),size=3.5,alpha=.75)+
  geom_text(aes(x=6.5,y=9.65,label="$8,784,637\n45,516,254 FRANCS."),size=4,alpha=.05)+
  geom_line(data=data2,aes(x=x,y=y,col=group))+
  scale_color_manual(values=pal.color$x)+
  xlim(1,16)+
  ylim(0,11)+
  labs(
    title="<span style='font-size:24pt'>Negro business men in the United States.<span>
    <br>
    <span style='font-size:6pt'>------------------------------------------------------------------</span>
    <br>
    <span style='font-size:16pt'>Nègres Americains dans les affairs.</span>
    <br>
    <span style='font-size:6pt'>------------------------------------------------------------------</span>
    <br>
    <span style='font-size:12pt'>Done by Atlanta University.</span>"
  )+
  theme_void()+
  theme(legend.position = "none",
        plot.title=element_markdown(hjust=.5),
        plot.background = element_rect(fill="#E8D8CA"),
        panel.background = element_rect(fill="#E8D8CA",color="#E8D8CA"))+
  inset_element(GM,.47,.72,.945,.9)+
  inset_element(GR,.47,.28,.945,.73)+
  inset_element(BA,.315,.5825,.365,.63)+
  inset_element(UN,.25,.41,.48,.587)+
  inset_element(BC,.25,.319,.5,.4083)+
  inset_element(DR,.6,.075,.8,.28)+
  inset_element(PU,.25,.09,.48,.31)+
  inset_element(BU,.49,.069,.59,.12)

#ggsave("duboischallenge2023_ch5.jpg",width=1191,height=1253,units="px")
## tried to save but it wasn't saving properly so I manually saved from plot zoom window
