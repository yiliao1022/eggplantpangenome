library(ggplot2)
library(sp)
library(tmap)

# world sampled  individuals x y coordinate
mydata <- read.table("./input/map.txt", header=TRUE, sep="\t")
visit.x <- mydata$x
visit.y <- mydata$y
mp<-NULL

# world map border object
mapworld <- borders("world", fill="white", xlim=c(-200,200), colour="black", size=0.2)
# world map
mp <- ggplot() + mapworld + ylim(-60,90)
# world map with sample dots
mp2<- mp + geom_point(size = 1, 
  aes(x=visit.x,y=visit.y,color=mydata$population1))+
          scale_color_manual(breaks = c("C1","C2","C3"),
                     values = c("#EF6F6F","#495D90","#37A95A"))
# world map with blue background
mp3<- mp2 +theme(panel.grid.major =element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill = '#AED8E6', colour = 'black'),
                     panel.border = element_blank(),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 legend.position = c(0.13, 0.10))
# output
mp3

ggsave(mp3, filename="map.pdf", width=10, height=6, dpi=300)
