## Week 3
# https://redketchup.io/color-picker - for matching colors

## libraries used
library(tidyverse)
library(geomtextpath)

data.c3 <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2023/challenge03/data.csv")

# Set up for the spiral
a <- 2
b <- 3
theta <- seq(0,13.3*pi,0.1)
r <- a + b*theta
df <- data.frame(x=r*cos(theta), y=r*sin(theta)) # Cartesian coords

ggplot(df[128:nrow(df),], aes(x,-y)) +
  geom_path(col='#D72139',size=3.5)+
  geom_polygon(data=data.frame(
    x=c(-90,-81,149,140),
    y=c(94,94,310,310)),
    aes(x,y),fill="#D72139"
  )+
  geom_polygon(data=data.frame(
    x=c(140,149,31,23),
    y=c(310,310,410,410)
  ),aes(x,y),fill="#EEB12A")+
  geom_polygon(data=data.frame(
    x=c(23,31,52,44),
    y=c(410,410,430,430)
  ),aes(x,y),col="#6282BB")+
  geom_polygon(data=data.frame(
    x=c(52,52,-230,-230),
    y=c(430,435,435,430)
  ),aes(x,y),fill="#476D5C")+
  
  ylim(-230,450)+
  xlim(-300,300)+
  
  geom_text(aes(x=0,y=0,label="734952"),size=7,family="Roboto")+
  geom_text(aes(x=30,y=390,label="37699"),size=4,family="Roboto")+
  geom_text(aes(x=30,y=360,label="LIVING\nIN CITIES\nFROM\n2500 TO 5000"),
            family="Roboto",size=4)+
  geom_text(aes(x=57,y=420,label="8025"),size=4,family="Roboto")+
  geom_text(aes(x=124,y=420,label="LIVING IN CITIES\nFROM 5000 TO 10000"),size=4)+
  geom_text(aes(x=-150,y=417,label="78139 LIVING IN CITIES\nOF OVER 10000 INHABITANTS"),
            size=4)+
  geom_textcurve(aes(x=-129,xend=129,y=-57,yend=-50,
                     label="LIVING IN THE COUNTRY AND VILLAGES"),
                 curvature=.65,size=5,family="Roboto",text_only=T)+
  labs(title=
         "\nCITY AND RURAL POPULATION.\n1890.")+
  
  theme_void()+
  theme(plot.title=element_text(hjust=.5,size=24,family="Roboto"),
        plot.background = element_rect(fill="#E8D8CA"),
        panel.background = element_rect(fill="#E8D8CA",color="#E8D8CA")
        )

ggsave("DuBoisChallenge2023_3.jpg",dpi=320,width=11,height=14)


