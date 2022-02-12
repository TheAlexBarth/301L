##
# Fall Plant Data query
##
library(readr)
file_location <- 'C:/Users/abart/OneDrive/Documents/UofSC/Classes/301 TA/ExampleDataSet/'
file_name <- '/2021Fall_item_L03_ac-moore-data.csv'
read_in <- paste(file_location,file_name,sep ="")

# initial datamanagement
plantDf <- read_csv(read_in) # read in the data
names(plantDf)[1] = "Section"
plantDf$GroupID = paste(plantDf$Section, plantDf$Group,sep = "")

saveRDS(plantDf,'./Data/2021Fall_item_ac-moore-data.rds')
