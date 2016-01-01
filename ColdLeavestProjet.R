
library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
setwd("C:\\Users\\Ed\\Documents\\Subclovergit")
getwd()

dev.off()
#analysis of Experiment 1 - 14 cultivars GH - Number Leaves 

file <- read.table("ColdPercLeaves.txt",header=TRUE)
file
head(file)
summary (file)

#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#666666", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")


#Here is analysing Subspecies 
file %>%
  group_by(Subspecie) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Subspecie, y=X.RdLeaves_mean, fill=Subspecie)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=X.RdLeaves_mean-X.RdLeaves_sd/2,
                    ymax=X.RdLeaves_mean+X.RdLeaves_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))

ggsave(file="GHPercentageLeaves1.tiff", dpi=300)

#Here Analysis of cultivar 
file %>%
  #filter(Cultivar == "Antas") %>%
group_by(Cv) %>%
  summarise_each(funs(mean, sd)) %>%
  mutate(Cv = factor(Cv,levels = 
                             Cv[order(X.RdLeaves_mean)])) %>%
  ggplot(aes(x=Cv, y=X.RdLeaves_mean, fill=Cv)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=X.RdLeaves_mean-X.RdLeaves_sd/2,
                    ymax=X.RdLeaves_mean+X.RdLeaves_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 22)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))+
  ylab("RedLeaves")


ggsave(file="PlotsPercentage_Cultivar3.tiff", dpi=300)

#data transformation



file$SQR<- sqrt(file$X.RdLeaves)
head(file)

#histogram
ggplot(data=file, aes(SQR)) + 
  geom_histogram()

#normality test
shapiro.test(file$SQR)

#QQplot
var<-file$SQR
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))


#AnovaLog analysis of transformed sqr
file$Block <- as.factor(file$Block )
anovaSQR<-aov(file$SQR~file$Cv+file$Block)
summary(anovaSQR)
TukeyHSD(anovaSQR)

#Means separation of SQR transformed data by Cultivar
(LSD.test(anovaSQR, "file$Cv", alpha= 0.05, p.adj="none"))             

#Anova and Means separation of log transformed data by Subspecies
file$Block <- as.factor(file$Block )
anovaSQR <- aov(file$SQR ~file$Subspecie+file$Block)
summary(anovaSQR)

TukeyHSD(anovaSQR)
(LSD.test(anovaSQR, "file$Subspecie", alpha= 0.05, p.adj="none"))             
summary (anovaSQR)

#Analysing variable Red Colour intensity 
#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")
#Here Analysis of cultivar 
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Cv) %>%
  summarise_each(funs(mean, sd)) %>%
  mutate(Cv = factor(Cv,levels = 
                       Cv[order(RedIntensity_mean)])) %>%
  ggplot(aes(x=Cv, y=RedIntensity_mean, fill=Cv)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=RedIntensity_mean-RedIntensity_sd/2,
                    ymax=RedIntensity_mean+RedIntensity_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 22)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))+
  ylab("RedIntensity")
head(file)


 