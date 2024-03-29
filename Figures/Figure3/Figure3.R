library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(ggpubr)
library(Cairo)

th<-theme(plot.title = element_text(size = 12, face = "bold"),axis.title=element_text(size=12,color = "black"),
          axis.text.x = element_text(size=10, color = "black"),axis.text.y = element_text(size=10, color = "black"))

inhib<-data.frame(read.table(file="Cox_results_NA", header = TRUE, sep = '\t'))
inhib

inhib.reshape<-melt(inhib, id.vars = c("Conc"))
inhib.reshape

CairoSVG(file="cox_inhib_na.svg", width = 5, height = 4, bg = "white")
ggplot(inhib.reshape, aes(x=Conc,y=value,color=variable)) + geom_point() + geom_line(size=0.8) + scale_color_jama() +
  xlab(label = "log[Conc. (µM)]") + scale_y_continuous(name = " % Relative Inhibition", limits = c(-1,60)) + theme_minimal()
dev.off()

#RFU plot

rfu<-data.frame(read.table(file="RFU_readings", header = TRUE, sep = '\t'))
head(rfu)

rfu.reshape<-melt(rfu, id.vars = c("Time","Concentration")) %>% na.omit() %>% mutate(Conc_var = paste0(Concentration, "_", variable))
head(rfu.reshape)

CairoSVG(file="cox_rfu.svg", width = 8, height = 4, bg = "white")
rfu.reshape %>% dplyr::group_by(Concentration, variable) %>%
  ggplot(., aes(x=Time,y=value, color=variable)) + geom_point(size = 0.6) + theme_bw() + scale_color_jama() +
  geom_line(aes(group=Conc_var), size=0.5) + 
  ylab(label = "RFU/min") + xlab(label="Time (min)") +
  facet_wrap("Concentration", scales = "free")
dev.off()

