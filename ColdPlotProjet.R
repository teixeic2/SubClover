
library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
setwd("C:\\Users\\Ed\\Documents\\Subclovergit")
getwd()
dev.off()
#analysis of Experiment 1 - 14 cultivars Iversen 2 - plot scoring 
file <- read.table("ColdPercPlots.txt",header=TRUE)
file
head(file)
summary (file)

#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")


#Here is analysing Subspecies 
file %>%
  group_by(Subspecies) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Subspecies, y=PercentageRedPlants50DAS_mean, fill=Subspecies)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercentageRedPlants50DAS_mean-PercentageRedPlants50DAS_sd/2,
                    ymax=PercentageRedPlants50DAS_mean+PercentageRedPlants50DAS_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))+
  

ggsave(file="PlotsPercentagePlants2.tiff", dpi=300)

#Here Analysis of cultivar 
file %>%
  #filter(Cultivar == "Antas") %>%
group_by(Cultivar) %>%
  summarise_each(funs(mean, sd) %>%
  mutate(Cultivar = factor(Cultivar,levels = 
                             Cultivar[order(PercentageRedPlants50DAS_mean)])) %>%
  ggplot(aes(x=Cultivar, y=PercentageRedPlants50DAS_mean, fill=Cultivar)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercentageRedPlants50DAS_mean-PercentageRedPlants50DAS_sd/2,
                    ymax=PercentageRedPlants50DAS_mean+PercentageRedPlants50DAS_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 22)+
  theme(axis.text.x=element_text(angle = +60, hjust = 0)) +
  theme(axis.text.y = element_text(face="bold", colour="#990000", size=12)+
  xlab("Cultivar")+
  ylab("PercentDamage")




ggsave(file="PlotsPercentage_Cultivar3.tiff", dpi=300)

#histogram
ggplot(data=file, aes(NaturalLog)) + 
  geom_histogram()

#normality test
shapiro.test(file$NaturalLog)

#QQplot
var<-file$NaturalLog
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))

#AnovaLog analysis of transformed log
file$Block <- as.factor(file$Block )
anovaLog<-aov(file$NaturalLog~file$Cultivar+file$Block)
summary(anovaLog)
TukeyHSD(anovaLog)

#Means separation of log transformed data by Cultivar
(LSD.test(anovaLog, "file$Cultivar", alpha= 0.05, p.adj="none"))             
summary (anovaLog)

#Anova and Means separation of log transformed data by Subspecies
file$Block <- as.factor(file$Block )
anovaLogsub <- aov(file$NaturalLog ~file$Subspecies+file$Block)
summary(anovaLogsub)

TukeyHSD(anovaLogsub)
(LSD.test(anovaLogsub, "file$Subspecies", alpha= 0.05, p.adj="none"))             
summary (anovaLogsub)