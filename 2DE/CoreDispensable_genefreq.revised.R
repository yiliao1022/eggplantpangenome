#!/usr/bin/env Rscript

library(argparser, quietly=TRUE)
p <- arg_parser("Classify gene/famliy base on gene/family PAV table.
                Draw  barplot, piechart and heatmap. ",
                hide.opts = T )

# Add command line arguments
p <- add_argument(p, "pav", help="input: genePAV.table", type="character" )
p <- add_argument(p, "outpre", help="output prefix", type="character" )

argv <- parse_args(p)

input <- argv$pav
pre <- argv$outpre

#input <- "./Orthogroups.GeneCount.tsv"
#pre <- "test4"


library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tibble)
#library(ggrepel)
#library(ggsci)


## 数据格式
#ID  ind1 ind2 ind3 ind4
#gene1 0 1 0 0
#gene2 0 4 0 0
#gene3 1 0 0 1
#gene4 1 1 1 1

pav <- read.delim(input, header = T, row.names = 1)
pav <- pav[rowSums(pav)>0,] ## 去除0�? 
samples <- colnames(pav)
indNum <- ncol(pav)

## 0 1 矩阵
pav[pav > 0] = 1


## gene分类, 4�?(a, b]
Ftype <- c("Private" , "Dispensable"  ,   "Softcore"  ,"Core")
Fcut <- c(0, 1 , 15, 16, 17)

## gene����, 4��(a, b]
#Ftype <- c("Private" , "Dispensable"  ,   "Softcore"  ,"Core")
#Fcut <- c(0, 1 , 0.9*indNum, indNum -1 , indNum)

## gene/family分类�?3�? (a, b]
# Ftype <- c("Private" , "Dispensable"  ,"Core")
# Fcut <- c(0, 1 , indNum - 1, indNum)

Fcolor <- brewer.pal(length(Ftype), 'Set1')


psum <- tibble( ID=rownames(pav), Freq = rowSums(pav)) %>%
  mutate( Class  = cut(Freq, breaks = Fcut , labels = Ftype))

psum$Class <- factor(psum$Class, levels = rev(Ftype))
## 输出基因分类�?
write.table(psum, 
            file=paste(pre, "freq_class.txt", sep = "."),
            row.names = F,
            sep = "\t",
            quote = F)

## 绘制柱状�?
p1 <- ggplot(psum,aes( x=Freq) ) + 
  geom_bar(aes(fill = Class),width = 0.6) +
  ylab("Number of Gene/Gene family") +
  xlab("Frequency") +  
  scale_fill_manual(values = Fcolor,  # 设置颜色 
                    name = NULL,  # 不显示图例标�?
  ) +
  theme_classic() 

ggsave(paste(pre, "freq_Barplot.pdf", sep = "."), 
       plot = p1 , device = "pdf", width = 7,height = 7 )


## 生成频率�?  
pfreq <- as.data.frame(table(psum$Class)) %>%
  mutate(percent = Freq/sum(Freq)) %>%
  dplyr::rename(Class=Var1)
  
write.table(pfreq, 
            file=paste(pre, "freq_class.summary.txt", sep = "."),
            row.names = F,
            sep = "\t",
            quote = F)

## 绘制饼图
p2 <- ggplot(pfreq, aes(x=1 ,y=percent,fill=Class )) +
  geom_col(color = "white",position = "stack",width = 1) +
  geom_text( aes(x = 1.3,
                label = paste0(Class, "\n", round(percent*100,1), "%")) ,
             position = position_stack(vjust = 0.5)
             ) +
  scale_fill_manual(values = Fcolor,  # 设置颜色 
                    name = NULL,  # 不显示图例标�?
                    ) +
  coord_polar(theta = "y",clip = "off") + #极坐�?
  theme(legend.position = "none") +  #不显示图�?
  theme_void()   # 清空主题

ggsave(paste(pre, "freq_class.Piechart.pdf", sep = "."), 
       plot = p2 , device = "pdf", width = 7,height = 7 )

## 绘制热图
library(ComplexHeatmap)

# 按freq进行基因排序
psum <- column_to_rownames(psum, var="ID")
## 小数据测�?
#pav <- pav[1:100,]
#psum <- psum[rownames(pav),]

psum <- psum[order(psum$Freq,decreasing=T),]
pav_sort <- t(pav[rownames(psum), ])

drow = T
#if(indNum >30){
#  drow = F
#}
names(Fcolor) <- Ftype
h <- Heatmap(pav_sort,
        cluster_rows = F,
        cluster_columns = F, use_raster=F,
        col = c("grey","darkorange" ),  # 热图颜色
        row_names_side = "left",
        show_column_names = F ,
        show_row_names = T,
        ## 图例设置
        heatmap_legend_param = list(
          title = " ", at = c(1, 0),
          labels = c( "Present","Absent"),
          ncol = 2
        ),
        
        ## 列注释设�?
        top_annotation = HeatmapAnnotation(
          Type = psum$Class,
          col = list( Type =  Fcolor)
        ))

pdf(paste(pre, "freq_class.Heatmap.pdf", sep = "."),width = 10,height = 5  )
draw(h,   heatmap_legend_side = 'bottom', annotation_legend_side = "right")
dev.off()
