library(dplyr)
library(ggplot2)
library(ggrepel)

dataset <- read.table(file="./input/enrich.txt", header=TRUE, sep="\t")

dataset$Description <- factor(dataset$Description, levels=rev(dataset$Description))

mytheme <- theme(axis.title=element_text(face="plain", size=12, colour='black'),
                 axis.text=element_text(face="plain", size=10, colour='black'),
                 axis.line=element_line(size=0.5, colour='black'),
                 panel.background=element_rect(color='black'),
                 legend.key=element_blank()
)

p <- ggplot(dataset, aes(x=fold.Enrichment, y=Description, colour=p.value, size=Count)) +
  geom_point() +
  scale_size(range=c(2,8)) +
  scale_colour_gradient(low="#FF0000", high="#0000FF") +
  theme_bw() +
  ylab("Description") +
  xlab("Fold Enrichment") +
  labs(color=expression(p.value)) +
  theme(legend.title=element_text(size=10), legend.text=element_text(size=10)) +
  theme(axis.title.y=element_text(margin=margin(r=-10)), axis.title.x=element_text(margin=margin(t=10))) +
  theme(axis.text.x=element_text(face="plain", color="black", angle=0, vjust=1))

plot <- p + mytheme
plot

ggsave(plot,filename="enrich.pdf",width=8,height=6, dpi=300)
