## dubois challenge 2024 wk 6
library(tidyverse)
library(ggfx)
library(ggtext)


ggplot()+
  # tick marks at the top
  geom_segment(
    data=data.frame(
      x=seq(.76,.845,.001),
      xend=seq(.76,.845,.001),
      y=c(runif(86,.898,.9)),
      yend=c(runif(20,.901,.905),runif(66,.901,.908))+seq(0,0.015,.015/86)[1:86],
      group=seq(1:86)
    ),
    aes(x=x,xend=xend,y=y,yend=yend,group=group),
    linewidth=.5,col="black"
  )+
  
  # black polygonal section
  geom_polygon(
    data=data.frame(
      x=c(0,.35,.5,.75,.85,.77),
      y=c(0,.3,.6,.9,.92,0)
    ),
    aes(x=x, y=y),
    fill="gray20",
    col="gray20"
  )+
  
  # gold(ish) polygonal section
  geom_polygon(data=data.frame(
    x=c(.85,.85,.92),
    y=c(0,.9,0)
  ),
  aes(x=x, y=y),
  fill="#FFCC00"
  ) +
  
  # add black brush strokes (the parts outside the figure will be covered later)
  with_blur(geom_line(data=data.frame(
    x=runif(8000,0,.85),
    y=seq(0.01,.8949,.000110625),
    group=rep(1:4000,each=2)
  ),
  aes(x,y,group=group),col="black",alpha=.25,size=.5),sigma=unit(.5,"mm"))+
  
  # add lighter brown triangle
  geom_polygon(data=data.frame(
    x=c(.77,.85,.85),
    y=c(0,.92,0)
  ),
  aes(x=x, y=y),
  fill="#997950"
  ) +
  
  # add darker brown section (overlap with black section)
  geom_polygon(data=data.frame(
    x=c(.68,.845,.85,.77),
    y=c(0,.92,.92,0)
  ),
  aes(x=x, y=y),
  fill="#7C4700",alpha=.15
  ) +
  
  # add gold(ish) section brusk strokes
  with_blur(
    geom_line(data=data.frame(
      x=seq(.85,.9211,.0001),
      y=c(runif(712,0,.9)),
      group=rep(1:356,each=2)
    ) %>% mutate(
      y=ifelse(y<((-.9/.07)*x+(.92*.9/.07)),y,(-.9/.07)*x+(.92*.9/.07))
    ),
    aes(x,y,group=group),col="goldenrod",alpha=.25,size=.5),
    sigma=unit(.5,"mm")
  )+  
  
  # add lighter brown triangle brush strokes
  with_blur(
    geom_line(data=data.frame(
      x=c(seq(.77,.84999,.00016),seq(.79,.84999,.00012),seq(.81,.84999,.00016),seq(.83055,.8505,.00008)),
      y=c(runif(500,0,.27),runif(500,.2,.46),runif(250,.4,.65),runif(250,0,.855)),
      group=rep(1:750,each=2)
    ) %>%
      mutate(
        y=ifelse(y<((.92/.08)*x-(.77*.92/.08)),y,((.92/.08)*x-(.77*.92/.08)))
      ),
    aes(x,y,group=group),col="#1D0200",alpha=.5,size=.1),sigma=unit(.5,"mm"))+
  
  # add light yellowish triangle
  with_blur(
    geom_polygon(data=data.frame(
      x=c(.9,.85,1),
      y=c(0,.9,0)
    ),
    aes(x=x, y=y),
    fill="#D5B85A",alpha=.75),
    sigma=unit(3,"mm")
  )+
  
  # add line separating through brown section
  geom_segment(
    aes(x=.85,xend=.78,y=.9,yend=0),
    linewidth=.17
  )+
  
  # add line separating goldish/yellowish sections
  geom_segment(
    aes(x=.85,xend=.85,y=.9,yend=0),
    linewidth=.2
  )+
  
  # cover up black brush strokes
  geom_polygon(data=data.frame(
    x=c(0,0,.75,.5,.35),
    y=c(0,.9,.9,.6,.3)
  ),
  aes(x=x,y=y),
  fill="#E8D8CA")+
  
  # add black horizontal lines on right side
  geom_segment(data=data.frame(
    x=c(0,.8967,.87145,.85),
    xend=rep(1.25,4),
    y=c(0,.3,.6,.9),
    yend=c(0,.3,.6,.9),
    group=1:4
  ),
  aes(x=x,xend=xend,y=y,yend=yend,group=group),
  linewidth=.1)+
  
  # add white horizontal lines on figure
  geom_segment(data=data.frame(
    x=c(.35,.5,.75),
    xend=c(.8967,.87145,.85),
    y=c(.3,.6,.9),
    yend=c(.3,.6,.9),
    group=1:3
  ),
  aes(x=x,xend=xend,y=y,yend=yend,group=group),
  linewidth=.25,col="white")+
  
  # add squiggly line at top
  geom_line(data=data.frame(
    x=seq(.85,1.25,.002),
    y=c(runif(40,.915,.921),runif(140,.912,.918),rev(seq(.895,.915,.001)))
  ),
  aes(x=x,y=y),
  linewidth=.15
  )+
  
  # add thin line across the top
  geom_segment(
    aes(x=0,xend=1.25,y=.925,yend=.925),
    linewidth=.1,
    alpha=.25
  )+
  
  # year labels
  geom_text(data=data.frame(
    x=c(0.015,.285,.46,.69,.4,.85),
    y=c(-.015,.3,.6,.9,-.015,-.015),
    labs=c("1890","1860","1840","1800","85%","15%")
  ),
  aes(x=x,y=y,label=labs),
  )+
  
  # population labels - on black section
  geom_text(data=data.frame(
    x=c(.4,.6,.6,.6),
    y=c(.025,.2,.287,.315),
    labs=c("6.337.980","NEGROES","90%","3.542.147")
  ),
  aes(x=x,y=y,label=labs),
  col="white",size=c(5,4,3.5,4)
  )+
  
  # population labels on non-black sections
  geom_text(data=data.frame(
    x=c(.85,.85,.853,.853,.85,1.1),
    y=c(.21,.19,.287,.315,.01,.2),
    labels=c("MULATTOES","+\nMULATRES","10%","411.613","1.113.063","WHITES")
  ), aes(x=x,y=y,label=labels),size=c(3,2.5,3,3,3.5,4)
  )+
  
  # add light lines on right side
  geom_segment(data=data.frame(
    x=rep(.85,41),
    y=seq(.02,.92,.0225),
    yend=rep(0,41),
    group=1:41,
    alpha=runif(41,.4,.8)
  ) %>% mutate(xend=ifelse(y+.85<1.25,y+.85,1.25),
               yend=ifelse(y+.85<1.25,0,y-.4)),
  aes(x=x,xend=xend,y=y,yend=yend,group=group,alpha=alpha-.3),linewidth=.1,col="gray70"
  )+
  geom_segment(
    data=data.frame(
      x=seq(.87,1.23,.0225),
      y=rep(.92,17),
      xend=rep(1.25,17),
      group=1:17,
      alpha=runif(17,.4,.8)
    ) %>% mutate(yend=x-1.25+.92),
    aes(x=x,xend=xend,y=y,yend=yend,group=group,alpha=alpha-.3),linewidth=.1,col="gray70"
  )+
  
  # add title and subtitle
  labs(
    title="<span style='font-size:18pt'><br>The Amalgamation of the White and Black elements of the population<br>in the United States.<span>
    <br>
    <span style='font-size:6pt'>------------------------------------------------------------------</span>
    <br>
    <span style='font-size:10pt'>Amalgamation des elements blancs et noirs parmi la population Americaine.</span>
    <br>
    <span style='font-size:6pt'>------------------------------------------------------------------</span>
    <br>
    <span style='font-size:10pt'>Done by Atlanta University.</span>"
  )+

  # set theme elements
  theme_void()+
  theme(legend.position = "none",
        plot.title=element_markdown(hjust=.5),
        plot.background = element_rect(fill="#E8D8CA"),
        panel.background = element_rect(fill="#E8D8CA",color="#E8D8CA"))

# save it
ggsave("dubois_challenge_2024_wk6.png",dpi=320,height=11,width=8.5)
