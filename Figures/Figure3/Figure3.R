library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(igraph)

nodes<-read.csv("matrix_drug_prot_food_nodes", header = TRUE, sep = '\t', as.is = T)
edges<-as.matrix(read.csv("matrix_drug_prot_food_edges", header = TRUE, sep = '\t', as.is = T, row.names=1))
head(edges)
edges_melt<-melt(edges)
subset<-subset(edges_melt, edges_melt$value >= 1)
nrow(subset)

net <- graph_from_data_frame(d=subset, vertices=nodes, directed=T)

colrs<-c("#98B900","#F67500", "#1065E2", "#d7191c", "#00B2D5", "#7F130D", "#009390", "#FA007E","#003251","#33A02C","#8434C1", "#565E83", "#7B452E", "#814F00", "#D95F0E", "#D1A400","#BDBDBD", "#f1b6da")
V(net)$color<-colrs[V(net)$media.type]
V(net)$frame.color<-colrs[V(net)$media.type]
V(net)$vertex.size<-V(net)$size
shpe<-c("circle","circle", "circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","square")
V(net)$shape<-shpe[V(net)$media.type]

par(mar=c(0.9, 0.9, 0.9, 0.9))
plot(net, vertex.label.cex=0.5, vertex.label.family="Helvetica",edge.color="#525252", edge.width=0.9, edge.arrow.size=0.01, margins=c(10,10))

legend(x=1.1, y=0.7, c("Alimentary","Blood","Cardiovascular","Dermatological","Genitourinary","Hormonal","Antiinfectives","Antineoplastics","Musculoskeletal","Nervous","Respiratory","Sensory","Various","Neutraceutical","Multiple-ATC","Unclassified","Target","FoodCmpd"), pch=21,
       col="white", pt.bg=colrs, pt.cex=2, cex=1, bty="n", ncol=1)
