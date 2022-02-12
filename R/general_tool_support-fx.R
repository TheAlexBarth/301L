###
# General support functions
###

#' gg_color_hue - a function to generate 
#' 
#' @param n number of colors to return
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}