install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")

library(dplyr)
library(ggplot2)
library(gridExtra)


Distance <- read.table("C:/Users/liao/Desktop/eggplantNC2026/Fig3f/All.dis.bed.bed",header = T)
p1<-ggplot(Distance)
head(Distance)
p1+geom_violin(aes(x=Types,y=Distance,fill=Types))+geom_boxplot(aes(x=Types,y=Distance),width=0.2)+ ylim(0, 18)

table(Distance$Types)


types_to_compare <- c("DNA","LTR")
sub <- subset(Distance, Types %in% types_to_compare)

# 双侧（默认）
wilcox.test(Distance ~ Types, data = sub, alternative = "two.sided", exact = FALSE)
