
library (lubridate)

#Calculate Thermal Time for the day of Sowing 

df_SowingDate <- read.table("SowingDate.txt",header=TRUE)
df_TTSum<- read.table("ThermalSum.txt", header=TRUE)

df_SowingDate$Date <- dmy(df_SowingDate$Date)
df_TTSum$Date <- ymd(df_TTSum$Date)
df_merged2 <- merge(df_SowingDate,df_TTSum,by="Date")
head(df_merged2)
write.table(df_merged2, "TTSowDate.txt")

