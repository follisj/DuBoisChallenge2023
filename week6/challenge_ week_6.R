
##############################

### RECREATE VIZ

##############################

## PACKAGES
library(tidyverse)
library(ggtext)

## GET DATA
data <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2023/challenge06/data.csv")

## COLOR PALETTE
pal_color <-c("#BE0023","#ECBB65","#362114","#060905","#005736")

## CREATE DATA SET FOR THE VIZ
data2 <- data.frame(
  x=rep(c(0,0,40,40),5),
  y=c(0,10.6,10.6,0,
      10.6,58.8,58.8,10.6,
      58.8,86.4,86.4,58.8,
      86.4,99.8,99.8,86.4,
      99.8,100.3,100.3,99.8),
  category=c(
    rep("Miscellaneous",4),
    rep("Property",4),
    rep("Persons",4),
    rep("Society",4),
    rep("Government",4)
  )) %>% mutate(category=(fct_relevel(category,
                                      "Government",
                                      "Society",
                                      "Persons",
                                      "Property",
                                      "Miscellaneous"))
  )

## TRANSFORM DATA (ROTATE 45 DEGREES)
s2 <- 1/sqrt(2)
data3 <- data2 %>%
  mutate(x2=1.5*y*s2,y2=1.5*y*s2) %>%
  mutate(x2=x2+rep(c(40-s2-40/sqrt(2),40-s2-40/sqrt(2),40,40),5),
         y2=y2+rep(c(40/sqrt(2),40/sqrt(2),0,0),5))
data3.lab <- data.frame(
  x=c(rep(140,5)),
  y=seq(12,60,12),
  category=c("MISCELLANEOUS",
             "PROPERTY",
             "PERSONS",
             "SOCIETY",
             "GOVERNMENT")) %>% 
  mutate(#category2= category,
    category=(fct_relevel(category,
                          "GOVERNMENT",
                          "SOCIETY",
                          "PERSONS",
                          "PROPERTY",
                          "MISCELLANEOUS")))
ggplot()+
  #geom_polygon(data=data3,aes(x=x,y=y,fill=category))+
  geom_polygon(data=data3,aes(x=x2,y=y2+20,fill=category))+
  scale_fill_manual(values=pal_color)+
  geom_point(data=data3.lab,aes(x=x,y=y,col=category),size=8)+
  geom_text(data=data3.lab,aes(x=x-8,y=y,label=as.character(category)),
            size=4,hjust=1,family="Times New Roman")+
  geom_text(data=data3.lab[5,],aes(x=x-20,y=y+5,label="CRIMES AGAINST"),
            size=3,family="Times New Roman")+
  scale_color_manual(values=pal_color)+
  geom_hline(yintercept = 175,alpha=.1)+
  geom_richtext(aes(x=45,y=155,label=
                      "<span style='font-size:15pt'>**3250**</span>
                    <br>
                    <span style='font-size:10pt'>PRISONERS PER MILLION OF
                    <br>NEGRO POPULATION CRIMES
                    <br>DIVIDED AS FOLLOWS:</span>"),
                fill=NA,label.color=NA,family="Times New Roman")+
  labs(
    title="<br><br><span style='font-size:16pt'>Crime among American Negroes.<span>
    <br>
    <span style='font-size:6pt'>------------------------------------------</span>
    <br>
    <span style='font-size:11pt'>Criminalité parmi les Noirs Américains.</span>
    <br>
    <span style='font-size:6pt'>------------------------------------------</span>
    <br>
    <span style='font-size:10pt'>Done by Atlanta University.</span>"
  )+
  
  theme_void()+
  ylim(-10,180)+
  xlim(0,150)+
  theme(legend.position = "none",
        text=element_text(family="Optima"),
        plot.title=element_markdown(hjust=.5),
        plot.background = element_rect(fill="#E8D8CA"),
        panel.background = element_rect(fill="#E8D8CA",color="#E8D8CA"))

ggsave("week6_challenge.png",dpi=320,height=10,width=7.5)

##############################

### ALTERNATE VIZ

##############################
  
## PACKAGES
library(tidyverse)
library(showtext)

## GET FONT
font_add_google("Shadows Into Light","sil")
showtext_auto()

## COLOR PALETTE
pal_color <-c("#BE0023","#ECBB65","#362114","#060905","#005736")

## CREATE COLOR BLOCKS
data_g2 <- data.frame(
  x=runif(250,0,40),
  y=seq(0.12,10.5,.0416),
  group=rep(1:250,each=2)
) %>%
  mutate(x2=x*cos(pi/4)+y*sin(pi/4),
         y2=y*cos(pi/4)-x*sin(pi/4))

data_bl2 <- data.frame(
  x=runif(1000,0,40),
  y=rep(seq(10.7,58.7,.1925),each=2),
  group=rep(1:500,each=2)
) %>%
  mutate(x2=x*cos(pi/4)+y*sin(pi/4),
         y2=y*cos(pi/4)-x*sin(pi/4))

data_br2 <- data.frame(
  x=runif(500,0,40),
  y=rep(seq(59,86,.1083),each=2),
  group=rep(1:500,each=2)
) %>%
  mutate(x2=x*cos(pi/4)+y*sin(pi/4),
         y2=y*cos(pi/4)-x*sin(pi/4))

data_y2 <- data.frame(
  x=runif(250,0,40),
  y=seq(87,99,.0481),
  group=rep(1:250,each=2)
) %>%
  mutate(x2=x*cos(pi/4)+y*sin(pi/4),
         y2=y*cos(pi/4)-x*sin(pi/4))

data_r2 <- data.frame(
  x=runif(40,0,40),
  y=seq(99,100.95,.05),
  group=rep(1:20,each=2)
) %>%
  mutate(x2=x*cos(pi/4)+y*sin(pi/4),
         y2=y*cos(pi/4)-x*sin(pi/4))

## CREATE DATA FRAMES FOR 'LEGEND'
data3.lab <- data.frame(
  x=runif(5,98,102),
  y=c(4.25,14.25,24.25,34.25,44.25),
  category=c("misc",
             "property",
             "persons",
             "society",
             "govt")) %>% 
  mutate(#category2= category,
    category=(fct_relevel(category,
                          "govt",
                          "society",
                          "persons",
                          "property",
                          "misc")))

data4 <- data.frame(
  x=c(runif(190,105,111)),
  y=c(rep(seq(1.75,6.25,.25),each=2),
      rep(seq(11.75,16.25,.25),each=2),
      rep(seq(21.75,26.25,.25),each=2),
      rep(seq(31.75,36.25,.25),each=2),
      rep(seq(41.75,46.25,.25),each=2)
  ),
  group=rep(1:95,each=2),
  category=c(rep(as.factor(5:1),each=38))
)

## VIZ CODE

ggplot()+
  geom_line(data=data_g2,
            aes(x2+6,y2+21,group=group),col="#005736",alpha=.25,size=1)+
  geom_line(data=data_bl2,
            aes(x2+7,y2+22,group=group),col="#060905",alpha=.5,size=1)+
  geom_line(data=data_br2,
            aes(x2+8,y2+23,group=group),col="#362114",alpha=.25,size=1)+
  geom_line(data=data_y2,
            aes(x2+9,y2+24,group=group),col="#ECBB65",alpha=.25,size=1)+
  geom_line(data=data_r2,
            aes(x=x2+10,y=y2+25,group=group),col="#BE0023",alpha=.25,size=1)+
  
  geom_line(data=data4,aes(x=x,y=y,group=group,col=category),alpha=.75)+
  
  scale_fill_manual(values=pal_color)+
  
  geom_text(aes(x=98,y=48,label="crimes against"),size=4,family="sil")+
  geom_text(data=data3.lab,aes(x=x,y=y,label=as.character(category)),
            size=6,hjust=1,family="sil")+
  geom_text(aes(x=25,89,label="3250"),size=8,family="sil")+
  geom_text(aes(25,81,label="PRISONERS PER MILLION OF\nNEGRO POPULATION CRIMES\nDIVIDED AS FOLLOWS"),
            size=6,family="sil")+
  geom_text(aes(x=60,y=128,label="Crime among American Negroes"),size=12,family="sil")+
  geom_text(aes(x=60,y=120.5,label="Criminalité parmi les Noirs Américains"),size=7,family="sil")+
  geom_text(aes(x=60,y=113,label="Done by Atlanta University"),size=7,family="sil")+
  geom_hline(yintercept=100,linewidth=.1)+
  scale_color_manual(values=pal_color)+
  ylim(-10,130)+
  theme_void()+
  theme(legend.position = "none")

#ggsave("week6_challenge_alt2.jpg",dpi=320,height=14,width=8,bg="white")
