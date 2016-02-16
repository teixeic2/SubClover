library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)


#dev.off()
#analysis of Round 1 - 6 cultivars  

setwd("C:\\Users\\Ed\\Documents\\Subclovergit")
getwd()
file <- read.table("Softseed1.txt",header=TRUE)
head(file)
summary(file)

#define factors
file <- file %>%
  mutate(
    Plot = as.factor(Plot),
    Block = as.factor(Block),
    round = as.factor(round),
    percentageHard = (100-percentagesoft)
    )


#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#666666", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")

str(file)

# define position of bar and errorbar
dodge_x <- position_dodge(width = 0.9)

#Here is analysing Subspecies 
file %>%
  group_by(Cultivar, depth, SowTreat) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Cultivar, y=(percentageHard_mean), fill=depth)) +
  geom_bar(stat="identity",position = dodge_x) +
  geom_errorbar(aes(ymin=percentageHard_mean-percentageHard_sd/2,
                    ymax=percentageHard_mean+percentageHard_sd/2),
                width=0.25, size=0.3,position=dodge_x)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16) #+
 # theme(axis.text.x=element_text(angle = +90, hjust = 0))

ggsave(file="seedhard1.tiff", dpi=300)



#data transformation


file$SQR<- sqrt(file$Red)
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
                       Cv[order(Intensity_mean)])) %>%
  ggplot(aes(x=Cv, y=Intensity_mean, fill=Cv)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Intensity_mean-Intensity_sd/2,
                    ymax=Intensity_mean+Intensity_sd/2),
                width=0.25)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 22)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))+
  ylab("Intensity")
head(file)


 