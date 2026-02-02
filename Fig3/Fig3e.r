Rscript for Figure 3e
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
library(dplyr)
library(ggplot2)
library(gridExtra)

#Feature <- read.table("C:/Users/liao/Desktop/eggplantNC2026/Fig3e/FINAL.featureoverlap.bed.bed",header = F)
#head(Feature)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
infile  <- ifelse(length(args) >= 1, args[1], "C:/Users/liao/Desktop/eggplantNC2026/Fig3e/FINAL.featureoverlap.bed.bed")
outfile <- ifelse(length(args) >= 2, args[2], "C:/Users/liao/Desktop/eggplantNC2026/Fig3e/FINAL.featureoverlap.bed.bed.bed")

dat <- read.table(infile, header = FALSE, sep = "\t", stringsAsFactors = FALSE,
                  quote = "", comment.char = "", check.names = FALSE)

if (ncol(dat) < 3) stop("Input file must have at least 3 columns: Sample, Annotation, Count")

dat <- dat[, 1:3]
colnames(dat) <- c("Sample", "Anno", "Count")
dat$Count <- as.numeric(dat$Count)

res_wide_prop <- dat %>%
  mutate(
    Class = case_when(
      str_detect(Anno, "intergenic_region") ~ "intergenic_region",
      str_detect(Anno, "upstream_gene_variant") ~ "upstream_gene_variant",
      str_detect(Anno, "downstream_gene_variant") ~ "downstream_gene_variant",
      str_detect(Anno, "intron_variant|Bintron_variant") ~ "intron_variant", # 兼容 Bintron_variant
      TRUE ~ "Exon"
    )
  ) %>%
  group_by(Sample, Class) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  complete(
    Sample,
    Class = c("intergenic_region","upstream_gene_variant",
              "downstream_gene_variant","intron_variant","Exon"),
    fill = list(Count = 0)
  ) %>%
  group_by(Sample) %>%
  mutate(Total = sum(Count), Prop = ifelse(Total == 0, NA_real_, Count / Total)) %>%
  ungroup() %>%
  select(Sample, Class, Prop) %>%
  pivot_wider(names_from = Class, values_from = Prop)

write.table(res_wide_prop, file = outfile, sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = TRUE)



#####Prepare Figure
library(tidyverse)
library(scales)
install.packages("ggbreak")
library(ggbreak)

# 
df <- read.table("C:/Users/liao/Desktop/eggplantNC2026/Fig3e/FINAL.featureoverlap.bed.bed.bed",
                 header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)

type_order <- c("intergenic_region",
                "upstream_gene_variant",
                "downstream_gene_variant",
                "intron_variant",
                "Exon")

df_long <- df %>%
  pivot_longer(cols = all_of(type_order),
               names_to = "Type",
               values_to = "Prop") %>%
  mutate(Type = factor(Type, levels = type_order))

# 
# 
break_low  <- 0.15
break_high <- 0.65

p <- ggplot(df_long, aes(x = Type, y = Prop)) +
  # 
  geom_jitter(
    aes(color = Type),
    width = 0.15, height = 0,
    shape = 21, fill = NA, stroke = 0.9,  # 空心关键：fill=NA
    size = 0.5, alpha = 0.9
  ) +
  # 
  geom_boxplot(
    aes(color = Type),
    width = 0.5, outlier.shape = NA,
    fill = NA, linewidth = 0.8
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.8)) +
  scale_y_break(c(break_low, break_high), space = 0.05) +  # 
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  ) +
  labs(x = "Type", y = "Proportion")

print(p)

ggsave("FINAL.featureoverlap.5types.box_jitter.ybreak.pdf", p, width = 8, height = 5)
ggsave("FINAL.featureoverlap.5types.box_jitter.ybreak.png", p, width = 8, height = 5, dpi = 300)
