require(ggplot2)
library(ggalt)
library(readxl)
library(zoo)
library(dplyr)
library(tidyverse)
library(cowplot)

library(readxl)
highwaydata <- read_excel("Documents/Highways/highwayplan.xlsx")


highwaydata[] <- t(na.locf(t(highwaydata)))

highwaydata[, c(2:16)] <- sapply(highwaydata[, c(2:16)], as.numeric)

highwaydata$old = 1000*highwaydata$`2007`/highwaydata$Area
highwaydata$current = 1000*highwaydata$`2018`/highwaydata$Area
highwaydata$future = 1000*highwaydata$Planned/highwaydata$Area

highwaydata$oldp = 100000*highwaydata$`2007`/highwaydata$Population
highwaydata$currentp = 100000*highwaydata$`2018`/highwaydata$Population
highwaydata$futurep = 100000*highwaydata$Planned/highwaydata$Population


pbreaks = 10*0:7

p <- ggplot(highwaydata, aes(x=old, xend=future, y=reorder(Year, current))) + 
  geom_dumbbell(color="light blue", 
                size_x=3.5, 
                size_xend = 3.5,
                #Note: there is no US:'color' for UK:'colour' 
                # in geom_dumbbel unlike standard geoms in ggplot()
                colour_x="#304c6d", 
                colour_xend = "#00ffff")+
  geom_point(aes(x=current,  y=reorder(Year, current)),size=3.5,color='#01a1c7')+
  ylab(element_blank())+
  xlab('Motorway Density (km per 1000 km^2)')+
  labs(title = 'Total Length of Motorways / Area')+
  scale_x_continuous(expand = c(0,0),limits = c(-1,70),breaks=pbreaks)+
  geom_rect(xmin = 60, xmax = 69.9, ymin = 'Finland', ymax = 'Sweden',colour='#999999',fill='white')+
  geom_point(aes(x=62,y='Turkey'),size=3.5,color='#304c6d')+
  geom_point(aes(x=62,y='Romania'),size=3.5,color='#01a1c7')+
  geom_point(aes(x=62,y='Estonia'),size=3.5,color='#00ffff')+
  annotate('text',x=63.5,y='Turkey',size=3,label='2007',hjust=0,alpha=0.7)+
  annotate('text',x=63.5,y='Romania',size=3,label='2018',hjust=0,alpha=0.7)+
  annotate('text',x=63.5,y='Estonia',size=3,label='Planned',hjust=0,alpha=0.7)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank())

qbreaks = 10*0:5

q <- ggplot(highwaydata, aes(x=oldp, xend=futurep, y=reorder(Year, currentp))) + 
  geom_dumbbell(color="light blue",
                size_x=3.5, 
                size_xend = 3.5,
                #Note: there is no US:'color' for UK:'colour' 
                # in geom_dumbbel unlike standard geoms in ggplot()
                colour_x="#304c6d", 
                colour_xend = "#00ffff")+
  geom_point(aes(x=currentp),size=3.5,color='#01a1c7')+
  ylab(element_blank())+
  xlab('Motorway Density (km per 100 000 population)')+
  labs(title = 'Total Length of Motorways / Population')+
  scale_x_continuous(expand = c(0.01,0),limits = c(-0.5,45),breaks=qbreaks)+
  geom_rect(xmin = 38, xmax = 44.9, ymin = 'Turkey', ymax = 'United Kingdom',colour='#999999',fill='white')+
  geom_point(aes(x=39.5,y='Kosovo'),size=3.5,color='#304c6d')+
  geom_point(aes(x=39.5,y='Poland'),size=3.5,color='#01a1c7')+
  geom_point(aes(x=39.5,y='Romania'),size=3.5,color='#00ffff')+
  annotate('text',x=40.5,y='Kosovo',size=3,label='2007',hjust=0,alpha=0.7)+
  annotate('text',x=40.5,y='Poland',size=3,label='2018',hjust=0,alpha=0.7)+
  annotate('text',x=40.5,y='Romania',size=3,label='Planned',hjust=0,alpha=0.7)+
  theme_minimal()+
  theme(panel.grid.minor=element_blank())

row <- plot_grid(p,q)

title <- ggdraw() + 
  draw_label(
    "The Growth of Motorways in Europe: 2007, 2018 and Planned Future Expansions",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    vjust = 0
  ) +
  draw_label(
    "Sources: EUROSTAT, ADAC, UN, Wikipedia",
    size=10,
    x=0,
    y = 0.1,
    hjust = 0,
    vjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 9)
  )

dumbs <- plot_grid(
  title, row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.06, 1)
)