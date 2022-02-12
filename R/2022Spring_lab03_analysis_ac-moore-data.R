###
# Analysis of AC-Moore Population Distributions
###

rm(list = ls())
library(dplyr)
library(ggplot2)
library(gridExtra)

# Read in data
pop_df <- readRDS("./Data/2022Spring_lab03_item_ac-moore-data.rds")

###
# Analyze the data for different species and different predictors
###

pred_vect <- c("Transect_Pos","Can_cov","Surf_Temp")

spp_sep <- split(pop_df,f = pop_df$Spp) # split to new lists
stat_output <- vector('list',length(spp_sep)) # make output holder
names(stat_output) <- names(spp_sep)

make_models <- function(df) {
  pred_out <- vector('list',length(pred_vect))
  for(i in 1:length(pred_out)) {
    mod <- summary(lm(df[['Abundance']]~df[[pred_vect[i]]]))
    pred_out[[i]] <- mod
  }
  names(pred_out) <- pred_vect
  return(pred_out)
}


for(i in 1:length(spp_sep)) {
  stat_output[[i]] <- make_models(spp_sep[[i]])
}

sink('./Output/2022Spring_lab03_item_ac-moore-stats.txt')
for(i in 1:length(stat_output)) {
  for(j in 1:length(stat_output[[i]])){
    cat('\n')
    cat('====================')
    cat('====================')
    cat('\n')
    cat(paste(names(stat_output)[i],'~',names(stat_output[[i]])[j]))
    cat('\n')
    cat('====================')
    cat('\n')
    cat(names(stat_output[[i]][[j]]$coefficients[2,]))
    cat('\n')
    cat(stat_output[[i]][[j]]$coefficients[2,])
    cat('\n')
    cat(paste0('Rsq: ',stat_output[[i]][[j]]$r.squared))
    cat('\n')
  }
}
sink()

pdf('./Output/2022Spring_lab03_figures-ac-moore-pop-dist.pdf')
for(i in 1:length(spp_sep)){
  tp_plot <- spp_sep[[i]] %>%
    ggplot(aes(x = Transect_Pos,y = Abundance))+
    geom_point(color = 'Green4')+
    geom_smooth(method = 'lm',se = F, color = 'Green4')+
    labs(x = 'Transect Position',y = 'Individuals [sqm]',
         subtitle = names(spp_sep)[i])+
    theme_bw()
  
  cc_plot <- spp_sep[[i]] %>%
    ggplot(aes(x = Can_cov,y = Abundance))+
    geom_point(color = 'Blue4')+
    geom_smooth(method = 'lm',se = F, color = 'Blue4')+
    labs(x = 'Canopy Cover [%]',y = 'Individuals [sqm]')+
    theme_bw()
  
  st_plot <- spp_sep[[i]] %>%
    ggplot(aes(x = Surf_Temp,y = Abundance))+
    geom_point(color = 'red4')+
    geom_smooth(method = 'lm',se = F, color = 'red4')+
    labs(x = 'Surface Temperature [*C]',y = 'Individuals [sqm]')+
    theme_bw()
  
  print(ggarrange(tp_plot,cc_plot,st_plot,ncol = 1))
}
dev.off()
