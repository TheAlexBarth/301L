###
# Litter Free Journal DataPrep
###
library(tidyr)
library(ggplot2)

trashDf = read.csv("litter_free_digital_journal_20211004.csv", as.is = T)
trashDf$date = unlist(lapply(strsplit(trashDf$observed, split = " "), "[[",1))
trashDf$date = as.Date(trashDf$date, format = "%Y-%m-%d")


trimTrash = trashDf[,c(12,17,18,53,54,55,56)]
trimTrash = trimTrash[which(trimTrash$Duration_of_Sweep != ""),]
trimTrash = trimTrash[which(!(is.na(trimTrash$Number_of_Participants))),]

keyVals = trimTrash[which(trimTrash$Debris_Type %in% c("Platsic Bottles",
                                                       "Styrofoam", "Plastic Bags - Retail",
                                                       "Beverage Cans (aluminum)")),]

keyVals$Duration_of_Sweep[which(keyVals$Duration_of_Sweep == "1-2 hours")] = 2
keyVals$Duration_of_Sweep[which(keyVals$Duration_of_Sweep == "30 minutes - 1 hour")]=1
keyVals$Duration_of_Sweep[which(keyVals$Duration_of_Sweep == "10 - 30 Minutes")] = .5
keyVals$Duration_of_Sweep[which(keyVals$Duration_of_Sweep == "More than 2 hours")] = 3
keyVals$Duration_of_Sweep[which(keyVals$Duration_of_Sweep == "Under 10 Minutes")] = .25

keyVals$Duration_of_Sweep = as.numeric(keyVals$Duration_of_Sweep)
keyVals$Manhours = keyVals$Number_of_Participants * keyVals$Duration_of_Sweep
keyVals$CPUE = keyVals$Count / keyVals$Manhours
keyVals$Year = unlist(lapply(strsplit(as.character(keyVals$date),split = "-"),
                             "[[",1))

popData = read.csv("popDensity.csv",as.is = T,header = F)
popData[1,1] = "County"
names(popData) = popData[1,]
popData = popData[-1,]

popData$Area = as.numeric(popData$Area)
popDen = popData
for(c in 2:13){
  popData[,c] = as.numeric(popData[,c])
  popDen[,c] = popData[,c] / popData$Area
}
popDen$county = unlist(lapply(strsplit(popDen$County, split = ","), "[[", 1))
popDen = popDen[,-1]

popDen_pivot = pivot_longer(popDen,cols = c(1:12), names_to = "Year", values_to = "Den")

popDen_21 = popDen_pivot[which(popDen_pivot$Year >=2021),]
popDenFilter = popDen_21[which(popDen_21$county %in% unique(keyVals$county)),]

finalDf = merge(keyVals,popDenFilter, by = "county")
model = lm(CPUE ~ Den, data = finalDf)
summary(model)
mod2 = lm(CPUE~Den, finalDf[which(finalDf$Debris_Type == "Beverage Cans (aluminum)"),])
summary(mod2)

write.csv(finalDf, "LitterData.csv")


#plotting
bc=ggplot(finalDf[which(finalDf$Debris_Type == "Beverage Cans (aluminum)"),],
       aes(x = Den, y = CPUE))+
  geom_point(alpha = 0.5,col = "grey50")+
  stat_smooth(method = "lm",se = F, col = "black")+
  labs(x = "Density [People per square km]", y = "Trash Collected [Count per manhour]")+
  theme_bw()

pb=ggplot(finalDf[which(finalDf$Debris_Type == "Plastic Bags - Retail"),],
       aes(x = Den, y = CPUE))+
  geom_point(alpha = 0.5,col = "grey50")+
  stat_smooth(method = "lm",se = F, col = "black")+
  labs(x = "Density [People per square km]", y = "Trash Collected [Count per manhour]")+
  theme_bw()

sf=ggplot(finalDf[which(finalDf$Debris_Type == "Styrofoam"),],
       aes(x = Den, y = CPUE))+
  geom_point(alpha = 0.5,col = "grey50")+
  stat_smooth(method = "lm",se = F, col = "black")+
  labs(x = "Density [People per square km]", y = "Trash Collected [Count per manhour]")+
  theme_bw()

library(cowplot)
plot_grid(bc,pb,sf,nrow = 1)
