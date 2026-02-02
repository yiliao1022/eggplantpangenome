install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")

library(ggplot2)
library(dplyr)
library(gridExtra)

Expression <- read.table("E:/备份20250503/原C盘/备份/桌面.bak/Eggplant_ms/SV.annotatioin.vcf.report.bed.reformated.bed.new.bed.bed.bed",sep="\t",header = T)

S1260 <- Expression %>% rowwise() %>%  filter (S126==0 ) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S126_flower > 2 | S126_fruit > 2 | S126_leaf >2 | S126_root > 2 | S126_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S126_flower,S126_fruit,S126_leaf,S126_root,S126_stem),method ="pearson")$estimate)

S1261 <- Expression %>% rowwise() %>%  filter (S126==1) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S126_flower > 2 | S126_fruit > 2 | S126_leaf >2 | S126_root > 2 | S126_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S126_flower,S126_fruit,S126_leaf,S126_root,S126_stem),method ="pearson")$estimate)

n0 <- nrow(S1260)
n1 <- nrow(S1261)
c(S126_0 = n0, S126_1 = n1)



tmp1 <- as.vector(1-S1260$corE)
tmp2 <- as.vector(1-S1261$corE)


S1850 <- Expression %>% rowwise() %>%  filter (S185==0 ) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S185_flower > 2 | S185_fruit > 2 | S185_leaf >2 | S185_root > 2 | S185_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S185_flower,S185_fruit,S185_leaf,S185_root,S185_stem),method ="pearson")$estimate)

S1851 <- Expression %>% rowwise() %>%  filter (S185==1) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S185_flower > 2 | S185_fruit > 2 | S185_leaf >2 | S185_root > 2 | S185_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S185_flower,S185_fruit,S185_leaf,S185_root,S185_stem),method ="pearson")$estimate)


n0 <- nrow(S1850)
n1 <- nrow(S1851)
c(S185_0 = n0, S185_1 = n1)

S0920 <- Expression %>% rowwise() %>%  filter (S092==0 ) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S92_flower > 2 | S92_fruit > 2 | S92_leaf >2 | S92_root > 2 | S92_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S92_flower,S92_fruit,S92_leaf,S92_root,S92_stem),method ="pearson")$estimate)

S0921 <- Expression %>% rowwise() %>%  filter (S092==1) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S92_flower > 2 | S92_fruit > 2 | S92_leaf >2 | S92_root > 2 | S92_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S92_flower,S92_fruit,S92_leaf,S92_root,S92_stem),method ="pearson")$estimate)


n0 <- nrow(S0920)
n1 <- nrow(S0921)
c(S092_0 = n0, S092_1 = n1)


S0270 <- Expression %>% rowwise() %>%  filter (S027==0 ) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S27_flower > 2 | S27_fruit > 2 | S27_leaf >2 | S27_root > 2 | S27_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S27_flower,S27_fruit,S27_leaf,S27_root,S27_stem),method ="pearson")$estimate)

S0271 <- Expression %>% rowwise() %>%  filter (S027==1) %>% filter (GeneStart-End<= 2000 | Begin-GeneEnd <= 2000) %>% filter (S76_flower > 2 | S76_fruit >2 | S76_leaf >2 | S76_root> 2| S76_stem >2 |  S27_flower > 2 | S27_fruit > 2 | S27_leaf >2 | S27_root > 2 | S27_stem > 2) %>%
  mutate(corE=cor.test(c(S76_flower,S76_fruit,S76_leaf,S76_root,S76_stem),c(S27_flower,S27_fruit,S27_leaf,S27_root,S27_stem),method ="pearson")$estimate)


n0 <- nrow(S0270)
n1 <- nrow(S0271)
c(S027_0 = n0, S027_1 = n1)



# Function to perform bootstrap resampling

bootstrap_diff_mean <- function(data1, data2, n_bootstrap = 10000) {
  # Vector to store the bootstrapped differences of means
  boot_diff <- numeric(n_bootstrap)
  
  # Perform bootstrap sampling
  for (i in 1:n_bootstrap) {
    # Resample with replacement from both datasets
    sample1 <- sample(data1, length(data1), replace = TRUE)
    sample2 <- sample(data2, length(data2), replace = TRUE)
    
    # Compute the difference in means between the resampled datasets
    boot_diff[i] <- mean(sample1) - mean(sample2)
    
  }
  return(boot_diff)
}


bootstrap_mean <- function(Data, n_bootstrap = 10000) {
  # Vector to store the bootstrapped differences of means
  boot_mean <- numeric(n_bootstrap)
  
  # Perform bootstrap sampling
  for (i in 1:n_bootstrap) {
    # Resample with replacement from both datasets
    sample <- sample(Data, length(Data), replace = TRUE)
    
    # Compute the difference in means between the resampled datasets
    boot_mean[i] <- mean(sample)
  }
  
  return(boot_mean)
}


tmp11<-na.omit(tmp1)
tmp22<-na.omit(tmp2)

boot_mean1 <- bootstrap_mean(tmp11)
boot_mean2 <- bootstrap_mean(tmp22)

boot_mean1

w <- wilcox.test(boot_mean1, boot_mean2, alternative = "two.sided", exact = FALSE)
cat("\nWilcoxon test:\n")
print(w)

ci_lower1 <- quantile(boot_mean1, 0.025)
ci_lower1
ci_upper1 <- quantile(boot_mean1, 0.975)
ci_upper1


ci_lower2 <- quantile(boot_mean2, 0.025)
ci_lower2
ci_upper2 <- quantile(boot_mean2, 0.975)
ci_upper2

install.packages("ggpubr")
library(ggpubr)
library(dplyr)



merged_data <- c(tmp1, tmp2)
labels <- c(rep("tmp1", length(tmp1)), rep("tmp2", length(tmp2)))
data <- data.frame(Value = merged_data, Label = labels)

# 
calc_97.5_ci <- function(x) {
  m <- mean(x)
  se <- sd(x) / sqrt(length(x))  # 
  ci_upper <- m + qnorm(0.975) * se  # 
  ci_lower <- m - qnorm(0.975) * se  # 
  return(c(y = m, ymin = ci_lower, ymax = ci_upper))
}

# 
ggboxplot(data, x = "Label", y = "Value", bxp.errorbar=T, bxp.errorbar.width = 0.2,
          color = "Label", palette = "jco",  
          width = 0.3,  # 
          main = "Boxplot comparing tmp1 and tmp2") +
  stat_summary(fun.data = calc_97.5_ci, 
               geom = "errorbar", 
               width = 0.3,  # 
               size = 0.7) +  # 
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.5, color = "red", size = 1) +  # 
  theme(axis.text.x = element_text(size = 12))
