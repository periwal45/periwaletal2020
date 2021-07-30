library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr) #for count
library(ggsci)
library(Cairo)

## Figure S1

################################### 1. Drug structural classification and ATC distribution (Fig S1a) ###################################################

data<-data.frame(read.table("1410_Drugs_Stat", header = TRUE, sep = '\t'))
head(data)
nrow(data)
count<-count(data, c("ATC","Superclass"))

head(count)
nrow(count)

#FigS1a
CairoSVG(file="FigS1a.svg", width = 9, height = 6, bg = "white")
ggplot(data.frame(count), aes(x = Superclass, y = freq, group=ATC, fill=ATC)) + geom_point(size =2.5, shape=23, position = position_dodge(width = 0.4))  + scale_fill_d3(palette = "category20") + scale_y_continuous(trans='log10') + labs(x = "Structural Classification (Superclass)", y = "Freq (log10)")+theme_bw()+theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + coord_flip()
dev.off()

############################### 2. FooDB structural classification and food distribution (Fig S1b) ###########################################################

data<-read.table("11kFood", header = TRUE, sep = '\t')
head(data)
fcount<-count(data, c('Superclass','Type'))
head(fcount)

#FigS1b
CairoSVG(file="FigS1b.svg", width = 9, height = 6, bg = "white")
ggplot(data.frame(fcount), aes(x = Superclass, y = freq, group=Type, fill=Type)) + geom_point(size =2.5, shape=23, position = position_dodge(width = 0.4))  + scale_fill_d3(palette = "category20") + scale_y_continuous(trans='log10') + labs(x = "Structural Classification (Superclass)", y = "Freq (log10)")+theme_bw()+theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + coord_flip()
dev.off()
