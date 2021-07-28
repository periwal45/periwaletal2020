# The script is used to select drugs for training and testing. Testing pairs
# were randomly selected from 1410 drugs, comprising of 20% of the most
# represented Superclass of drugs (i.e. at least > 100 drugs in that Superclass)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(Cairo)
library(readr)

# set global theme for all plots

th<-theme(plot.title = element_text(size = 12, face = "bold"),axis.title=element_text(size=12,color = "black"),
          axis.text.x = element_text(size=10, color = "black"),axis.text.y = element_text(size=10, color = "black"))

#read files
data<-read.csv("drugs", header = TRUE, sep = '\t')
head(data)
dim(data)

# superclass
sc<-data %>% group_by(Superclass) %>% summarise(count = length(Superclass))
head(sc)

CairoSVG(file="drugs_superclass.svg", width = 5, height = 7, bg = "white")
ggplot(sc, aes(Superclass,count)) + geom_bar(stat = "identity", fill = "#bdbdbd") +
  xlab("Superclass") + ylab("#count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.line = element_line(size=0.3, colour = "black"), panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 14, colour = "black"))
dev.off()

# filter top superclasses with count > 100

sc100<-data %>% group_by(Superclass) %>% summarise(count = length(Superclass)) %>%
  filter(count > 100)
head(sc100)

## randomly pick 20% drugs from Superclass having greater than 100 drugs
test_drugs<-data.frame(data %>% dplyr::group_by(Drug.ID, ATC, Name, Superclass, Class) %>%
  filter(Superclass %in% as.factor(sc100$Superclass)))
head(test_drugs)

# this will be independent set
test_index<-sample(nrow(test_drugs), size = 0.2 * nrow(test_drugs))
test.set<-data.frame(test_drugs[test_index,])
dim(test.set)
head(test.set)

#View(test.set %>% group_by(Superclass) %>% summarise(count = length(Superclass)))

write.csv(test.set, "test.set.csv", row.names = F)

# this will be used for training and validation set
training.set<-data.frame(data[-test_index,])
head(training.set)
dim(training.set)

write.csv(training.set, "training.set.csv", row.names = F)
