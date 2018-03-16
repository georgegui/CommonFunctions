## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 4, fig.width = 6)

## ------------------------------------------------------------------------
if(!require(CommonFunctions)){
  devtools::install_github('georgegui/CommonFunctions')
  library(CommonFunctions)
}
library(data.table)
library(gridExtra)

## ------------------------------------------------------------------------
iris <- data.table(iris) 
ggplot_list <- list()

## ------------------------------------------------------------------------
ggplot_list[[1]] <- PrettyPlot(iris, 'Sepal.Length', include_0 = F)
ggplot_list[[1]]

## ------------------------------------------------------------------------
ggplot_list[[2]] <- PrettyPlot(iris, 'Sepal.Length', include_0 = F, 
                               weight = 'Sepal.Width')
ggplot_list[[2]]

## ------------------------------------------------------------------------
my_axis <- GenerateAxisX(iris$Sepal.Length, min_x = 4, max_x = 8, xtitle = 'Sepal Length')
ggplot_list[[3]] <- PrettyPlot(iris, 'Sepal.Length', scalex = my_axis)
ggplot_list[[3]]

## ------------------------------------------------------------------------
iris[, is_setosa := Species == 'setosa']

ggplot_list[[4]] <- PrettyPlot(
  iris, 'Sepal.Length', include_0 = F, x_full_range = T, 
  color_legend = 'is_setosa')
ggplot_list[[4]]

## ------------------------------------------------------------------------
grid.arrange(grobs = ggplot_list)

