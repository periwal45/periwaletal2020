library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(Cairo)

# plot of hyperparameter results
hyp<-data.frame(read.table(file = "rf_hyper_results", sep = '\t', header = TRUE))
head(hyp)

CairoSVG(file="FigS4.svg", width = 7, height = 4, bg = "white")
hyp %>% melt(id.vars = c("Iter","ntree","nodesize","mtry","classwt")) %>% filter(variable != "exec.time") %>% 
  ggplot(aes(y=variable,x=value,fill=variable)) +
  geom_boxplot(lwd = 0.3) + geom_jitter(size=0.2) + scale_fill_jco() + theme_minimal() + th
dev.off()