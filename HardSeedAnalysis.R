library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
library(lubridate)


#dev.off()
#analysis of Round 1 and 2 - 6 cultivars  

setwd("C:\\Users\\Ed\\Documents\\Subclovergit")
getwd()
file <- read.table("HardseedsRawData.txt",header=TRUE)
head(file)
summary(file)

str(file)

plots <- unique(file$Plot)
rounds <- unique(file$round)
depths <- unique(file$depth)
  

# calculate updated seed number for rounds 2 to 5
for (r in 2:length(rounds)) {
  for(p in 1:length(plots)) {
    for (d in length(depths)) {
    
  file$SeedNumber[file$round == rounds[r] & 
                    file$Plot == plots[p] & 
                    file$depth == depths[d]] <- 
    
    file$SeedNumber[file$round == rounds[r]-1 & 
                      file$Plot == plots[p] & 
                      file$depth == depths[d]
                                ] - 
    file$Imbibed[file$round == rounds[r]-1 & 
                   file$Plot == plots[p] & 
                   file$depth == depths[d]                                           
                 ]
  
    }
  
  }
  
}


# define factors and formats
file <- file %>%
  mutate(
    Plot = as.factor(Plot),
    Block = as.factor(Block),
    round = as.factor(round),
    StartDate = dmy(StartDate),
    HardSeedAssessDate = dmy(HardSeedAssessDate),
    GermAssessDate = dmy(GermAssessDate),
    SowingD = dmy(SowingD),
    hardSeedPerc = round(100-(Imbibed/SeedNumber)*100,1)
  )


file %>%
  filter(round == 2 & SowTreat == "S1" & is.na(SeedNumber))

file %>%
  filter(round == 1 & SowTreat == "S1" & depth == "down")


# graph
head(file)


#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#666666", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")

str(file)

# define position of bar and errorbar
dodge_x <- position_dodge(width = 0.9)

#Here is analysing Subspecies 
file %>%
  group_by(Cultivar, depth, SowTreat, round) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Cultivar, y=(hardSeedPerc_mean), fill=depth)) +
  geom_bar(stat="identity",position = dodge_x) +
  geom_errorbar(aes(ymin=hardSeedPerc_mean-hardSeedPerc_sd/2,
                    ymax=hardSeedPerc_mean+hardSeedPerc_sd/2),
                width=0.25, size=0.3,position=dodge_x)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16) +
  facet_grid(round~SowTreat)

#+
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

# transformations

#data transformation
file$transf_sqr <- sqrt(file$percentageHard)
file$transf_log <- log(file$percentageHard)
file$transf_acos <- acos(file$percentageHard/100)
file %>%
  tidyr::gather("transformation","value",10:12) %>%
  ggplot( aes(value)) + 
  geom_histogram()+
  facet_grid(.~transformation, scales="free")




head(file)

#histogram
ggplot(data=file, aes(transf)) + 
  geom_histogram()

#normality test
shapiro.test(file$SQR)
shapiro.test(file$transf_log)

#QQplot
var<-file$SQR
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))

var<-file$transf_log
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))

#AnovaLog analysis of transformed sqr
file$Block <- as.factor(file$Block )
anovaSQR<-aov(file$SQR ~ file$Cultivar*file$depth+file$Block)
summary(anovaSQR)
TukeyHSD(anovaSQR)

#AnovaLog analysis of transformed log
file$Block <- as.factor(file$Block )
anovatransf_log<-aov(file$transf_log ~ file$Cultivar*file$depth+file$Block)
summary(anovatransf_log)
TukeyHSD(anovatransf_log)

#Means separation of SQR transformed data by Cultivar
(LSD.test(anovaSQR, "file$Cultivar", alpha= 0.05, p.adj="none"))

#Means separation of log transformed data by Cultivar
(LSD.test(anovatransf_log, "file$Cultivar", alpha= 0.05, p.adj="none"))

#Anova and Means separation of log transformed data by Subspecies
file$Block <- as.factor(file$Block )
anovaSQR<-aov(file$SQR ~ file$Cultivar*file$depth+file$Block)
summary(anovaSQR)

#log transformed 

file$Block <- as.factor(file$Block )
anovaSQR<-aov(file$transf_log ~ file$Cultivar*file$depth+file$Block)
summary(anovatransf_log)


TukeyHSD(anovaSQR)
(LSD.test(anovaSQR, "file$depth", alpha= 0.05, p.adj="none"))             
summary (anovaSQR)



#Analysing variable Red Colour intensity 
#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")

#Here Analysis of cultivar 

# For S
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
  #facet_grid(depth ~.) +
  theme(axis.text.x=element_text(angle = +45,hjust = 1 ))+
  ylab("% Seed hardness")


# For B
# For S
file %>%
  filter(depth == "B") %>%
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
  #facet_grid(depth ~.) +
  theme(axis.text.x=element_text(angle = +45,hjust = 1 ))+
  ylab("% Seed hardness")

head(file)


