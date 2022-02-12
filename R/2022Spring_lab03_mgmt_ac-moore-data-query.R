###
# Data import for fall class data
###

library(readxl)

file_loc <- 'C:/Users/abart/OneDrive/Documents/UofSC/Classes/Spring 2022/'
file_name <- '2022Spring_item_ac-moore-data.xlsx'

plant_df <- read_xlsx(paste0(file_loc,file_name))
saveRDS(plant_df,file='./Data/2022Spring_lab03_item_ac-moore-data.rds')
