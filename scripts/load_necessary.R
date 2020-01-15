library(ggplot2)
library(dplyr)
library(Matrix)
library(PST)
library(TraMineR)
library(reshape2)
library(gridExtra)
library(grid)
library(maps)
library(fields)
library(maptools)
library(viridis)
library(cowplot)
library(scico)
library(parallel)
library(PrecipitationDataEval)

# A function for everything one might need when working with this
# ----------------------------------------------------------------------------------------------------------------------------------------------------------
colsAndNames <- function(){
  type_names <<- c('EOBS', 'NGCD_type1', 'NGCD_type2')
  type_plot_names <<- c("EOBS", "NGCD 1", "NGCD 2")
  seasons <<- c('summer', 'winter')
  model_names <<- c("CCLM4-8-17(CNRM-CERFACS-CM5)","RACMO22E(ICHEC-EC-EARTH)","CCLM4-8-17(ICHEC-EC-EARTH)","CCLM4-8-17(MPI-ESM-LR)","REMO2009(MPI-ESM-LR_r1i1p1)")
  model_plot_names <<- c("CCLM(CM5)", "RACMO(EC-EARTH)", "CCLM(EC-EARTH)", "CCLM(MPI)", "REMO(MPI)", "data_sets")
  method_names <<- c("raw", "LSCE-IPSL-CDFt-EOBS10-1971-2005","METNO-QMAP-MESAN-1989-2010","SMHI-DBS45-MESAN-1989-2010")
  method_plot_names <<- c("raw", "IPSL", "METNO", "SMHI", "EOBS", "NGCD 1", "NGCD 2")
  colors_vec <<- c('magenta','blue1','red','deepskyblue1','gray30')
  colors_list <<- as.list(colors_vec)
  names(colors_list) <<- model_names
  load(paste("~/postclim/PreparedData/2018/NGCD_type1/precipitation_19802005.dat", sep = ""))
  d <- matrix(NGCD_prec_summer, ncol = length(NGCD_prec_summer[1,1,]))
  keepIndex <<- rowSums(is.na(d)) != ncol(d)
  threshold <<- 0.1
}
# --------------------------------------------------------------------------------------------------------------------------------------------------