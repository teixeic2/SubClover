library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
setwd("C:\\Users\\Ed\\Documents\\Subclover\\ColdSens")
getwd()

#Analysis of Experiment 3 - Field Iversen 2 - 6 cultivars 
#create file 
file <- read.table("ColdPercPlant.txt",header=TRUE)
file
head(file)
summary (file)

#AnovaOut pure numbers
file$Block <- as.factor(file$Block )
anovaOut<-aov(file$PercentageofColddamagedPlants~file$Cultivar+file$Block)
summary(anovaOut)
TukeyHSD(anovaOut)



#barplot 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2")

#Here is analysis of Origin
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Origin) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Origin, y=PercentageofColddamagedPlants_mean, fill=Origin)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercentageofColddamagedPlants_mean-PercentageofColddamagedPlants_sd/2,
                    ymax=PercentageofColddamagedPlants_mean+PercentageofColddamagedPlants_sd/2),
                    width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))

  ggsave(file="PercentagePlants1.tiff", dpi=300)
  
#Here is analysing Subspecies 
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Subspecies) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Subspecies, y=PercentageofColddamagedPlants_mean, fill=Subspecies)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercentageofColddamagedPlants_mean-PercentageofColddamagedPlants_sd/2,
                    ymax=PercentageofColddamagedPlants_mean+PercentageofColddamagedPlants_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))

ggsave(file="PercentagePlants2.tiff", dpi=300)

#Here is analysing Cultivar 
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Cultivar) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Cultivar, y=PercentageofColddamagedPlants_mean, fill=Cultivar)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercentageofColddamagedPlants_mean-PercentageofColddamagedPlants_sd/2,
                    ymax=PercentageofColddamagedPlants_mean+PercentageofColddamagedPlants_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))

ggsave(file="PercentagePlants3.tiff", dpi=300)

#histogram
ggplot(data=file, aes(PercentageofColddamagedPlants)) + 
  geom_histogram()

#normality test
shapiro.test(file$PercentageofColddamagedPlants)

#QQplot
var<-file$PercentageofColddamagedPlants
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))

ggplot(file, aes(sample = var)) + stat_qq()


#transform SQR
file$sQR <- sqrt(file$PercentageofColddamagedPlants)
ggplot(data=file, aes(file$sQR)) + 
  geom_histogram()

#transform log
file$log <- log (file$PercentageofColddamagedPlants)
ggplot(data=file, aes(file$log)) + 
  geom_histogram()


head(file)

#AnovaLog analysis of transformed log
file$Block <- as.factor(file$Block )
anovaLog<-aov(file$log~file$Cultivar+file$Block)
summary(anovaLog)
TukeyHSD(anovaLog)

#Alternative way of doing ANoVA with agricolae 
model<-lm(file$log~file$Cultivar+file$Block)
anova(model)

#Means separation of log transformed data by Cultivar
(LSD.test(anovaLog, "file$Cultivar", alpha= 0.05, p.adj="none"))             
summary (anovaLog)

#Anova and Means separation of log transformed data by Subspecies
file$Block <- as.factor(file$Block )
anovaLogsub<-aov(file$log~file$Subspecies+file$Block)
summary(anovaLogsub)

TukeyHSD(anovaLogsub)
(LSD.test(anovaLogsub, "file$Subspecies", alpha= 0.05, p.adj="none"))             
summary (anovaLogsub)


#Anova and Means separation of log transformed data by Origin 
file$Block <- as.factor(file$Block )
anovaLogOrigin<-aov(file$log~file$Origin+file$Block)
summary(anovaLogOrigin)

TukeyHSD(anovaLogOrigin)
(LSD.test(anovaLogOrigin, "file$Origin", alpha= 0.05, p.adj="none"))             
summary (anovaLogOrigin)


#Means separation of log transformed data
(HSD.test(anovaLogOrigin, "file$Cultivar"))             
summary (anovaLogOrigin)




