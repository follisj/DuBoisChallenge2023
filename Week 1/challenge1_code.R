## challenge 1
# https://redketchup.io/color-picker

## libraries used
library(tidyverse)
library(jpeg)
library(png)
library(grid)
library(geomtextpath)

## load images
img=rasterGrob(readJPEG("expend.jpg"),width=1,height=1.6)
img2=rasterGrob(readPNG("income.png"),width=.65,height=1.5)

## get and format data
data.c1 <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2023/challenge01/data.csv")
data.c1 <- data.c1 %>%
  mutate(Class=ifelse(Class=="Over $1000","    1000\nAND OVER",Class),
         Tax=replace(Tax,Class=="$100-200",0.1),
         Other=replace(Other,Class=="$100-200",9.9))

## code to produce the viz - data prep and generating the viz
data.c1 %>% 
  add_row(Class="",Actual.Average=NA,Rent=20,Food=20,Clothes=20,Tax=20,Other=20) %>%
  pivot_longer(cols= !c("Class","Actual.Average"),names_to="cat",values_to="pct") %>%
  mutate(
    cat=ordered(cat,levels=c("Other","Tax","Clothes","Food","Rent")),
    Class=ordered(Class,levels=c(
      "    1000\nAND OVER",
      "$750-1000",
      "$500-750",
      "$400-500",
      "$300-400",
      "$200-300",
      "$100-200"
    ))) %>%
  group_by(Class) %>%
  mutate(csums=cumsum(pct),cdiff=csums-lag(csums)) %>% 
  mutate(cdiff2=ifelse(is.na(cdiff),csums/2,csums-cdiff/2)) %>%
  mutate(cdiff2=ifelse(pct==0,NA,cdiff2),cdiff2=ifelse(Class=="",NA,cdiff2)) %>% 
  ## generate viz
  ggplot(aes(pct,Class))+
  
  ## create barplot and labels
  geom_col(aes(fill=cat),width=.35,col="grey30",linewidth=.15)+
  geom_text(data=.%>% filter(cat=="Rent"),
            aes(x=cdiff2,y=Class,label=paste0(pct,"%")),col="white",fontface="bold",size=5)+
  geom_text(data=.%>% filter(cat != "Rent") %>% filter(pct != 0.1),
            aes(x=cdiff2,y=Class,label=paste0(pct,"%")),fontface="bold",size=5)+
  
  ## create table
  geom_segment(aes(x=-5,y=.5,xend=-5,yend=7.75),linewidth=.1)+
  geom_segment(aes(x=-15,y=.5,xend=-15,yend=7.75),linewidth=.1)+
  geom_segment(aes(x=-25,y=.5,xend=-25,yend=7.75),linewidth=.1)+
  geom_segment(aes(x=-25,xend=-5,y=7.75,yend=7.75),linewidth=.1)+
  geom_segment(data=data.frame(
    x=rep(-25,8),xend=rep(2,8),y=(0:7),yend=(0:7)),
    aes(x=x,xend=xend,y=y+.5,yend=yend+.5),
    arrow=arrow(length=unit(0.15,"cm"),type="closed"),
    alpha=.25
  )+
  geom_text(data=data.frame(x=rep(-14,7),y=7:1,
                            label=ifelse(data.c1$Actual.Average==139.1,
                                         paste0("$",data.c1$Actual.Average,0),
                              paste0("$",data.c1$Actual.Average))),
            aes(x=x,y=y,label=label),size=4,hjust=0)+
  geom_text(data=data.frame(x=rep(-24,7),y=7:1,label=data.c1$Class),
            aes(x=x,y=y,label=label),size=4,hjust=0)+
  geom_text(data=data.frame(x=c(-20,-10),y=c(7.65,7.65),label=c("CLASS","AVERAGE")),
            aes(x=x,y=y,label=label),size=3)+
  
  ## create labels on right
  geom_segment(data=data.frame(
    x=c(101,101,101,101),
    xend=c(101,101,101,101),
    y=c(.7,1.7,3.7,5.7),
    yend=c(1.3,3.3,5.3,7.3)),
    aes(x=x,xend=xend,y=y,yend=yend),linewidth=.25
  )+
  geom_segment(data=data.frame(
    x=c(rep(99,8)),
    xend=c(rep(101,8)),
    y=c(.7,1.3,1.7,3.3,3.7,5.3,5.7,7.3),
    yend=c(.7,1.3,1.7,3.3,3.7,5.3,5.7,7.3)),
    aes(x=x,xend=xend,y=y,yend=yend),linewidth=.25
  )+
  geom_text(data=data.frame(
    x=c(103,103,103,103),
    y=c(1,2.5,4.5,6.5),
    label=c("WELL-TO-DO.","COMFORTABLE.","FAIR.","POOR.")),
    aes(x=x,y=y,label=label),angle=90,size=3
  )+
  
  ## lines to connect bars
  geom_segment(aes(x=29,xend=37,y=1.175,yend=1.825),linewidth=.01)+
  geom_segment(aes(x=45,xend=56,y=1.175,yend=1.825),linewidth=.01)+
  geom_segment(aes(x=49.5,xend=64,y=1.175,yend=1.825),linewidth=.01)+
  
  geom_segment(aes(x=0,xend=13,y=2.175,yend=2.825),linewidth=.01)+
  geom_segment(aes(x=37,xend=44,y=2.175,yend=2.825),linewidth=.01)+
  geom_segment(aes(x=56,xend=61,y=2.175,yend=2.825),linewidth=.01)+
  geom_segment(aes(x=64,xend=66,y=2.175,yend=2.825),linewidth=.01)+
  
  geom_segment(aes(x=13,xend=18,y=3.175,yend=3.825),linewidth=.01)+
  geom_segment(aes(x=44,xend=55,y=3.175,yend=3.825),linewidth=.01)+
  geom_segment(aes(x=61,xend=70,y=3.175,yend=3.825),linewidth=.01)+
  geom_segment(aes(x=66,xend=75.5,y=3.175,yend=3.825),linewidth=.01)+
  
  geom_segment(aes(x=18,xend=23,y=4.175,yend=4.825),linewidth=.01)+
  geom_segment(aes(x=55,xend=66,y=4.175,yend=4.825),linewidth=.01)+
  geom_segment(aes(x=70,xend=84,y=4.175,yend=4.825),linewidth=.01)+
  geom_segment(aes(x=75.5,xend=88.5,y=4.175,yend=4.825),linewidth=.01)+
  
  geom_segment(aes(x=23,xend=22,y=5.175,yend=5.825),linewidth=.01)+
  geom_segment(aes(x=66,xend=69,y=5.175,yend=5.825),linewidth=.01)+
  geom_segment(aes(x=84,xend=92,y=5.175,yend=5.825),linewidth=.01)+
  geom_segment(aes(x=88.5,xend=96,y=5.175,yend=5.825),linewidth=.01)+
  
  geom_segment(aes(x=22,xend=19,y=6.175,yend=6.825),linewidth=.01)+
  geom_segment(aes(x=69,xend=62,y=6.175,yend=6.825),linewidth=.01)+
  geom_segment(aes(x=92,xend=90,y=6.175,yend=6.825),linewidth=.01)+
  geom_segment(aes(x=96,xend=90.1,y=6.175,yend=6.825),linewidth=.01)+
  
  geom_segment(aes(x=19,xend=20,y=7.175,yend=7.825),linewidth=.01)+
  geom_segment(aes(x=62,xend=40,y=7.175,yend=7.825),linewidth=.01)+
  geom_segment(aes(x=90,xend=60,y=7.175,yend=7.825),linewidth=.01)+
  geom_segment(aes(x=90.1,xend=80,y=7.175,yend=7.825),linewidth=.01)+
  
  ## create table at top of viz
  annotate("rect",xmin=0,xmax=100,ymin=8.08,ymax=11.25,fill="#d7c1aa",col="black",size=.15)+
  geom_segment(aes(x=0,xend=100,y=10.65,yend=10.65),size=.15)+
  geom_segment(aes(x=0,xend=100,y=10.35,yend=10.35),size=.15)+
  geom_segment(data=data.frame(x=c(20,40,60,80),xend=c(20,40,60,80),
                               y=rep(8,4),yend=rep(10.65,4)),
               aes(x=x,xend=xend,y=y,yend=yend),size=.15)+
  ## insert images into top table and income graphic
  annotation_custom(img,xmin=-.015,xmax=100,ymin=8.5,ymax=9.9)+
  annotation_custom(img2,xmin=-30,xmax=0,ymin=8.3,ymax=10)+
  geom_textcurve(aes(x=-5,xend=-25,y=9.9,yend=9.9,label="ANNUAL INCOME"),
                 size=5,curvature=.5,linecolor="#d7c1aa")+
  ## income graphic label
  geom_text(data=data.frame(x=c(10,30,50,70,90,50),y=c(rep(10.5,5),11),
                            label=c("RENT.","FOOD.","CLOTHES.","DIRECT TAXES.","OTHER EXPENSES AND SAVINGS.","ANNUAL EXPENDITURE FOR")),
            aes(x=x,y=y,label=label),size=c(rep(4,4),3.1,5.5))+
  
  ## lower text
  geom_text(aes(x=40,y=.25,label="FOR FURTHER STATISTICS RAISE THIS FRAME"),size=5)+
  
  ## title and caption
  labs(title=c("INCOME AND EXPENDITURE OF 150 NEGRO FAMILIES IN ATLANTA, GA., U.S.A."),
       caption="#DuBoisChallenge2023 / Week 1 2023 / Data Source: Anthony Starks @ajstarks")+
  
  ## expand grid area
  scale_y_discrete(expand=expansion(mult=c(.25,.5)))+
  
  ## colors for bars
  scale_fill_manual(values=c("#ccbba8","#b1a69f","#d98776","#9b7b90","black"))+
  
  ## expand x limits
  xlim(-30,110)+
  
  ## theme and label modifications
  theme_minimal()+
  theme(panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        plot.title=element_text(hjust=.5,size=24,vjust=-2,face="bold"),
        plot.caption = element_text(hjust=0,size=12),
        legend.position = "none",
        panel.background=element_rect(fill="#d7c1aa",color="#d7c1aa"),
        plot.background=element_rect(fill="#d7c1aa"))

## save viz
ggsave("DuBoisChallenge2023_1.jpg",dpi=320,width=16,height=9)

