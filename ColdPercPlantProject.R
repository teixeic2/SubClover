library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
setwd("C:\\Users\\Ed\\Documents\\Subclovergit")
getwd()
dev.off()
#Analysis of Experiment 3 - Field Iversen 2 - 6 cultivars 
#create file 
file <- read.table("ColdPercPlant.txt",header=TRUE)
file
head(file)
summary (file)

#AnovaOut pure numbers
file$Block <- as.factor(file$Block )
anovaOut<-aov(file$PercDamag~file$Cultivar+file$Block)
summary(anovaOut)
TukeyHSD(anovaOut)



#barplot 
# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2")

#Here is analysis of Origin
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Origin) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Origin, y=PercDamag_mean, fill=Origin)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercDamag_mean-PercDamag_sd/2,
                    ymax=PercDamag_mean+PercDamag_sd/2),
                    width=0.25)   +
 # scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))

  ggsave(file="PercentagePlants1.tiff", dpi=300)
  
#Here is analysing Subspecies 
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Subspecies) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Subspecies, y=PercDamag_mean, fill=Subspecies)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercDamag_mean-PercDamag_sd/2,
                    ymax=PercDamag_mean+PercDamag_sd/2),
                width=0.25)   +
 # scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0))

ggsave(file="PercentagePlants2.tiff", dpi=300)

#structure check
str()

x <- 1
x <- c(1,2,3,4,5)
x <- x*2


#Here is analysing Cultivar 
file %>%
  #filter(Cultivar == "Antas") %>%
  group_by(Cultivar) %>%
  summarise_each(funs(mean, sd)) %>%
  mutate(Cultivar = factor(Cultivar,levels = 
                                Cultivar[order(PercDamag_mean)])) %>%
  ggplot(aes(x=Cultivar, y=PercDamag_mean, fill=Cultivar)) +
  geom_bar(position = position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=PercDamag_mean-PercDamag_sd/2,
                    ymax=PercDamag_mean+PercDamag_sd/2),
                width=0.25)   +
 # scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16)+
  theme(axis.text.x=element_text(angle = +90, hjust = 0)) +
  xlab("Cultivar")+
  ylab("% Damage")

ggsave(file="PercentagePlants3.tiff", dpi=300)

#histogram
ggplot(data=file, aes(PercDamag)) + 
  geom_histogram()

#normality test
shapiro.test(file$PercDamag)

#QQplot
var<-file$PercDamag
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))

ggplot(file, aes(sample = var)) + stat_qq()


#transform SQR
file$sQR <- sqrt(file$PercDamag)
ggplot(data=file, aes(file$sQR)) + 
  geom_histogram()

#transform log
file$log <- log (file$PercDamag)
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

