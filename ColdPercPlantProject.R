library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
setwd("C:\\Users\\Ed\\Documents\\Subclover\\ColdSens")
getwd()

#create file
file <- read.table("ColdPercPlant.txt",header=TRUE)
file
head(file)
summary (file)

#Anova
file$Block <- as.factor(file$Block )
anova<-aov(file$PercentageofColddamagedPlants~file$Cultivar+file$Block)
summary(anova)
TukeyHSD(anova)



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
