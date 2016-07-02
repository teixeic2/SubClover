

```{r loadLibraries}
library(tidyr)
library (dplyr)
library (ggplot2)
library(agricolae)
library(knitr)
library (lubridate)

setwd("C:\\Users\\EdCarmen\\Documents\\CarmenProjects2016\\GitSubclover")
getwd()

#create file
df_BB <- read.table("BudBur3.txt",header=TRUE)

head(df_BB)

summary(df_BB)



df_BB %>%
  group_by(Cultivar,SowTreat) %>%
  dplyr::select(Difference) %>%
  ggplot(aes(x=Cultivar, y=(Difference))) +
  geom_bar(stat="identity",position = dodge_x) +
  geom_errorbar(aes(ymin=PropBurrBuried_mean-PropBurrBuried_sd/2, ymax=PropBurrBuried_mean+PropBurrBuried_sd/2),
                width=0.25, size=0.3,position=dodge_x)   +
  theme_grey(base_size = 16) +
  facet_grid(.~SowTreat) +
  labs(y="Percentage Buried burrs") +
  theme(axis.text.x=element_text(angle = +89, hjust = 0.95))
  
  ggplot(aes(x=Cultivar, y=mean, colour=factor(Cultivar), shape=factor(Cultivar))) +
  geom_point() +
  geom_line() +
  facet_grid(.~SowTreat) +
  labs(x="Sowing Date",y="Maximum Percentage of Hardseeds") +
  


df_BB %>%
  ggplot(aes(x=Cultivar, y=Difference, colour=factor(SowTreat)))+
  geom_boxplot() +
  geom_jitter() +
  labs(y="Thermal time from bud to bur3") +
  theme(axis.text.x=element_text(angle = +89, hjust = 0.95))
  facet_grid(.~SowTreat)