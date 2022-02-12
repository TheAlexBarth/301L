###
# initial read-in of course data
###

library(ggplot2)
library(dplyr)

raw <- readxl::read_xlsx("C:/Users/abart/OneDrive/Documents/UofSC/Classes/Spring 2022/ClassRoster.xlsx")
raw$EQC <- ifelse(is.na(raw$Major),"No","Yes")

private <- raw %>%
  select(-'Last Name',-'First',-'Pref') %>%
  select(names(private)[is.na(as.numeric(names(private)))])
  
saveRDS(raw,file = "./Data/spring22_admin01_initial-class-surv.rds")
