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
file$SQR <- file$percentageHard
#file$SQR <- sqrt(file$percentageHard)
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
anovaSQR<-aov(file$SQR ~ file$Cultivar*file$depth+file$Block)
summary(anovaSQR)
TukeyHSD(anovaSQR)

#Means separation of SQR transformed data by Cultivar
(LSD.test(anovaSQR, "file$Cultivar", alpha= 0.05, p.adj="none"))             

#Anova and Means separation of log transformed data by Subspecies
file$Block <- as.factor(file$Block )
anovaSQR<-aov(file$SQR ~ file$Cultivar*file$depth+file$Block)
summary(anovaSQR)

TukeyHSD(anovaSQR)
(LSD.test(anovaSQR, "file$depth", alpha= 0.05, p.adj="none"))             
summary (anovaSQR)

#Analysing variable Red Colour intensity 
#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")
#Here Analysis of cultivar 
file %>%
  filter(depth == "S") %>%
  group_by(Cultivar) %>%
  summarise_each(funs(mean, sd)) %>%
  mutate(Cultivar = factor(Cultivar,levels = 
                       Cultivar[order(percentageHard_mean)])) %>%
  ggplot(aes(x=Cultivar, y=percentageHard_mean, fill=Cultivar)) +
  geom_bar(position = dodge_x, stat="identity") +
  geom_errorbar(aes(ymin=percentageHard_mean-percentageHard_sd/2,
                    ymax=percentageHard_mean+percentageHard_sd/2),
                width=0.25,position = dodge_x)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 22) +
  #facet_grid(depth~.) +
  theme(axis.text.x=element_text(angle = +45,hjust = 1 ))+
  ylab("Seed hardness")
head(file)


 