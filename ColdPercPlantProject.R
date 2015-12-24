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


