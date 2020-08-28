setwd("/Users/vinitaperiwal/DrugvsDrug/Paper_Data/Figures/FigureS1/")
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr) #for count

## Figure S1
########################################################## Color Pallette ###################################################################

set.seed(1)
n <- 20
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

################################### 1. Drug structural classification and ATC distribution (Fig S1a) ###################################################

data<-data.frame(read.table("1410_Drugs_Stat", header = TRUE, sep = '\t'))
head(data)
nrow(data)
count<-count(data, c("ATC","Superclass"))

head(count)
nrow(count)

color_sample<-sample(col_vector,18)

ggplot(data.frame(count), aes(x = Superclass, y = freq, group=ATC, fill=ATC)) + geom_point(size =2.5, shape=23, position = position_dodge(width = 0.4))  + scale_fill_manual(values = color_sample) + scale_y_continuous(trans='log2') + labs(x = "Structural Classification (Superclass)", y = "Freq (log2)")+theme_bw()+theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + coord_flip()

############################### 2. FooDB structural classification and food distribution (Fig S1b) ###########################################################

data<-read.table("11kFood", header = TRUE, sep = '\t')
head(data)
fcount<-count(data, c('Superclass','Type'))
head(fcount)

color_sample<-sample(col_vector,15)
ggplot(data.frame(fcount), aes(x = Superclass, y = freq, group=Type, fill=Type)) + geom_point(size =2.5, shape=23, position = position_dodge(width = 0.4))  + scale_fill_manual(values = color_sample) + scale_y_continuous(trans='log2') + labs(x = "Structural Classification (Superclass)", y = "Freq (log2)")+theme_bw()+theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size = 12), legend.title = element_text(size = 14)) + coord_flip()
