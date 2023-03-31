library(tidyverse)


color_pal <- c("#000000","#654321","#d2b48c","#ffd700","#ffc0cb",
               "#dc143c","#00aa00","#4682b4","#7e6583")
data7 <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2023/challenge07/data.csv")

data7 %>%
  add_row(Group="Negroes",Occupation="Blank",Percentage=60) %>%
  add_row(Group="Whites",Occupation="Blank",Percentage=60) %>%
  mutate(rnum=factor(paste(Group,Occupation)),order=c(1,3,2,5,4,7,9,8,11,10,6,12)) %>%
  mutate(rnum=fct_reorder(rnum,order)) %>%
  ggplot()+
  geom_col(aes(x=1,y=Percentage,fill=rnum),position = "stack",width=1)+
  
  ggfx::with_blur(
  geom_line(data=data.frame(
    x=runif(378,.5,1.5),
    y=rep(seq(225.5,319.5,.5),each=2),
    group=rep(1:189,each=2)
  ),
  aes(x=x,y=y,group=group),col="#E8D8CA",alpha=.25,linewidth=.5)
  ,sigma=unit(1,'mm'))+
  
  ggfx::with_blur(
    geom_line(data=data.frame(
      x=runif(398,.5,1.5),
      y=rep(seq(60.5,159.5,.5),each=2),
      group=rep(1:199,each=2)
    ),
    aes(x=x,y=y,group=group),col="#E8D8CA",alpha=.25,linewidth=.5)
    ,sigma=unit(1,'mm'))+
  
  geom_point(data=data.frame(
    x=c(1.40,1.355,1.387,1.43,1.43),y=c(177,192,207,26,41)),aes(x=x,y=y),size=15)+
  geom_point(data=data.frame(
    x=c(1.40,1.355,1.387,1.43,1.43),y=c(177,192,207,26,41),Occupation=unique(data7$Occupation)
  ),aes(x=x,y=y,col=Occupation),size=14)+
  
  geom_text(data=data.frame(
    x=c(1.28,1.37,1.43,1.45,1.47,
        1.28,1.45,1.41,1.4,1.45),
    y=c(288,245,227.5,222.5,220,
        124,93.5,84,70,62),
    label=c("62%","28%","5%","4.5%","0.5%",
            "64%","5.5%","13.5%","13%","4%"),
    size=c(6,6,4,3.5,2,
           6,3,4,4,4)
  ),
  aes(x=x,y=y,label=label,size=size))+

  geom_text(data=data.frame(
    x=c(1.17,1.14,1.15,1.12,1.12),y=c(171,191,211,22,45),
    Occupation=c(
      "TRADE AND\nTRANSPORTATION",
      "PROFESSIONS",
      "DOMESTIC AND\nPERSONAL SERVICE",
      "AGRICULTURE, FISHERIES\nAND MINING",
      "MANUFACTURING AND\nMECHANICAL INDUSTRIES"
    ),
    hjust=c(.5,1,1,0,0)
  ),aes(x=x,y=y,label=Occupation),hjust=.5,size=4)+
  
  geom_line(data=data.frame(
    x=c(.5,1.5,.5,1.5,1.5,1.5),
    y=c(320,320,220,220,320,220),
    group=c(1,1,2,2,3,3)),
    aes(x=x,y=y,group=group),linewidth=.1)+
  geom_line(data=data.frame(
    x=c(.5,1.5,.5,1.5,1.5,1.5),
    y=c(60,60,160,160,60,160),
    group=c(1,1,2,2,3,3)),
    aes(x=x,y=y,group=group),linewidth=.1)+
  
  annotate("text",x=1.55,y=275,label="NEGROES",size=6,alpha=.75)+
  annotate("text",x=1.55,y=111,label="WHITES",size=6,alpha=.75)+
  
  scale_y_reverse()+
  scale_color_manual(values=c("#e3d8cf","#ffd700","#d2b48c","#dc143c","#4682b4"))+
  scale_fill_manual(values=rep(c("#dc143c","#ECBB65","#4682b4","#e3d8cf","#d2b48c","#E8D8CA"),2))+
  coord_polar(theta="y",start=-pi/3.5)+
  labs(title="\nOCCUPATIONS OF NEGROES AND WHITES IN GEORGIA.")+
  theme_void()+
  theme(legend.position = "none",
        text=element_text(family="Optima",face="bold"),
        plot.title=element_text(hjust=.5,size=24),
        plot.background = element_rect(fill="#E8D8CA"),
        panel.background = element_rect(fill="#E8D8CA",color="#E8D8CA"))

ggsave("week7_challenge.jpg",dpi=320,height=12,width=12)




