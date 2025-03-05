library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggsci)

data <- read.table("./input/Ka_Ks.txt", header=T)

data$Class <- factor(data$Class, levels = c("Core", "Softcore", "Dispensable", "Private"))

p <- ggplot(data, aes(Class, KaKs)) +
  stat_boxplot(geom = "errorbar", width=0.3)+
  geom_boxplot(aes(fill = Class), outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 1.3), breaks = seq(0, 1.3, by = 0.2)) +
  theme_minimal() +
  scale_color_lancet() +
  xlab('') +
  theme_cowplot()

p

ggsave(p, filename="KaKs.pdf", width=6, height=4, dpi=300)
