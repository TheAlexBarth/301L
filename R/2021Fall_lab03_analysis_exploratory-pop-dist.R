## Initial investigation into the data
plantDf <- readRDS('./Data/2021Fall_item_ac-moore-data.rds')

# how do the environmental parameters correlate with transect position?
cor(cbind(plantDf$Can_Cov,plantDf$Surf_Temp,plantDf$Tran_Pos))
plot(plantDf$Can_Cov,plantDf$Surf_Temp)
plot(plantDf$Tran_Pos, plantDf$Can_Cov)
plot(plantDf$Tran_Pos, plantDf$Surf_Temp)

# it doesn't look like there are any strong relationships between the transect position and the environmental params
# Before I do a bulk regression, I would like to look at the shape of the abudance data

plantList = split(plantDf,f = plantDf$Spp) #break into individual dataframes
for(l in 1:length(plantList)){
  windows()
  qqnorm(plantList[[l]]$Abundance, main = names(plantList)[l])
  qqline(plantList[[l]]$Abundance)
}

#obviously gripeweed is most heavily skewed towards the tails
# Let's see if it works with a log

for(l in 1:length(plantList)){
  windows()
  qqnorm(log(plantList[[l]]$Abundance+1), main = names(plantList)[l])
  qqline(log(plantList[[l]]$Abundance+1))
}

# log transform does seem to fix normality a tiny bit but it is not clear
# Overall let's just run the regressions with non-transformed data and see what we get.

# first let's run the entire abundance data and see what's up
plot(plantDf$Abundance ~ plantDf$Tran_Pos, col = as.factor(plantDf$Spp))
plot(plantDf$Abundance ~ plantDf$Can_Cov, col = as.factor(plantDf$Spp))
plot(plantDf$Abundance ~ plantDf$Surf_Temp, col = as.factor(plantDf$Spp))


predVect = c("Can_Cov")
for(l in 1:length(plantList)){
  for(i in 1:length(predVect)){
    print(paste(names(plantList)[l],"RegOutput: Abundance ~", predVect[i]))
    predCol = which(names(plantList[[l]]) == predVect[i])
    modTemp = lm(plantList[[l]]$Abundance ~ plantList[[l]][,predCol])
    print(summary(modTemp))
    windows()
    plot(plantList[[l]]$Abundance ~ plantList[[l]][,predCol], 
         main = paste(names(plantList)[l],"RegOutput: Abundance ~", predVect[i]))
    abline(lm(modTemp))
  }
}

#let's just look at it once more for how it works with log transformed data:

for(l in 1:length(plantList)){
  for(i in 1:length(predVects)){
    print(paste(names(plantList)[l],"RegOutput: Abundance ~", predVect[i]))
    predCol = which(names(plantList[[l]]) == predVect[i])
    modTemp = lm(log(plantList[[l]]$Abundance+1) ~ plantList[[l]][,predCol])
    print(summary(modTemp))
    windows()
    plot(log(plantList[[l]]$Abundance+1) ~ plantList[[l]][,predCol], 
         main = paste(names(plantList)[l],"RegOutput: Abundance ~", predVect[i]))
    abline(lm(modTemp))
  }
}
