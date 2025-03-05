library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggsci)

data <- read.table("./input/TPM.txt", header=T)

data$Class <- factor(data$Class, levels = c("Core_root", "Core_stem", "Core_leaf", "Core_flower", "Core_fruit", "Softcore_root", "Softcore_stem", "Softcore_leaf", "Softcore_flower", "Softcore_fruit", "Dispensable_root", "Dispensable_stem", "Dispensable_leaf", "Dispensable_flower", "Dispensable_fruit", "Private_root", "Private_stem", "Private_leaf", "Private_flower", "Private_fruit"))

p <- ggplot(data, aes(x = Class, y = TPM, fill = tissue)) +
  stat_boxplot(geom = "errorbar", width = 0.3)+
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5)) +
  theme_cowplot()  

p

ggsave(p, filename="TPM.pdf", width=6, height=4, dpi=300)
