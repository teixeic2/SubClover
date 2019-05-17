library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
library(lubridate)



#analysis of Round 1 and 2 - 6 cultivars  

setwd("C:\\Users\\Ed\\Documents\\Subclovergit")
getwd()
file <- read.table("HardseedsRawData.txt",header=TRUE)
head(file)
summary(file)

str(file)
#dev.off()
plots <- unique(file$Plot)
rounds <- unique(file$round)
depths <- unique(file$depth)
  

# calculate updated seed number for rounds 2 to 5 FIXME: down not yet working
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
  
    file$SeedNoRound_1 <- file$SeedNumber[file$round == rounds[1] & 
                                            file$Plot == plots[p] & 
                                            file$depth == depths[d]
                                          ]
  
    }
  
  }
  
}

#calculates cummulative imbibed seeds at that round

file <- file %>%
  group_by(Plot, depth) %>%
  arrange(round) %>%
  mutate(Imbibed_cum = cumsum(Imbibed)) %>%
  dplyr::ungroup()
 # dplyr::select(Plot, round, depth, Imbibed, Imbibed_cum)
  

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
    hardSeedPerc = round(100-(Imbibed/SeedNumber)*100,1),
    hardSeedPercCum = round(100-(Imbibed_cum/SeedNoRound_1)*100,1)
  )


head(file)
summary(file)

#barplot 
#The palette with grey:
cbPalette <- c("#999999", "#666666", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2","#56B4E9", "#009E73")

str(file)

# define position of bar and errorbar
dodge_x <- position_dodge(width = 0.9)

#Here is analysing hardseeds per cultivar, depth sowtreat and round  
file %>%
  filter(round == 1)%>%
    group_by(Cultivar, depth, SowTreat,round) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=Cultivar, y=(hardSeedPerc_mean), fill=depth)) +
  geom_bar(stat="identity",position = dodge_x) +
  geom_errorbar(aes(ymin=hardSeedPerc_mean-hardSeedPerc_sd/2,
                    ymax=hardSeedPerc_mean+hardSeedPerc_sd/2),
                width=0.25, size=0.3,position=dodge_x)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16) +
  facet_grid(round~SowTreat) +
theme(axis.text.x=element_text(angle = +90, hjust = 0))

ggsave(file="seedhard1.tiff", dpi=300)


#data transformation
file$transf_sqr <- sqrt(file$hardSeedPerc)
file$transf_log <- log(file$hardSeedPerc)
file$transf_acos <- acos(file$hardSeedPerc/100)

head(file)

x <- c("transf_sqr", "transf_log", "transf_acos")
colsSelec <- match(x,colnames(file))

file %>%
  tidyr::gather("transformation","value",colsSelec) %>%
  ggplot(aes(value)) + 
  geom_histogram() +
  facet_grid(.~transformation, scales="free")

head(file)

#normality test
shapiro.test(file$transf_acos)

#QQplot
var<-file$transf_acos
qqnorm(var)
qqline(var, col = 2)
qqplot(var, rt(300, df = 5))

#-------------------------------------------------
# ANOVA (selecing by hand FIXME: do loop)
file.subset <- file %>% subset(SowTreat == "S2" & round==1)
file.subset <- file %>% subset( round==1)

head(file.subset, 50)
summary(file.subset)
my.anova <- aov(transf_acos ~ Cultivar*depth + Block, data = file.subset)
my.anova <- aov(transf_acos ~ Cultivar*depth*SowTreat + Block, data = file.subset)

summary(my.anova)
TukeyHSD(my.anova)

#Means separation 
(LSD.test(my.anova, "SowTreat", alpha= 0.05, p.adj="none")) 


# Plot as scatter (round in X axes)

#Here is analysing hardseeds per cultivar, depth sowtreat and round  
file %>%
  group_by(Cultivar, depth, SowTreat,round) %>%
  summarise_each(funs(mean, sd)) %>%
  ggplot(aes(x=round, y=(hardSeedPercCum_mean))) +
  geom_point(aes(colour = Cultivar, size = 3, shape=Cultivar)) +
  geom_line(aes(colour = Cultivar))+ 
  geom_errorbar(aes(ymin=hardSeedPercCum_mean-hardSeedPercCum_sd/2,
                    ymax=hardSeedPercCum_mean+hardSeedPercCum_sd/2),
                width=0.25, size=0.3)   +
  scale_fill_manual(values=cbPalette) + 
  theme_grey(base_size = 16) +
  facet_grid(SowTreat~depth)


