#!/usr/bin/Rscript
library(dplyr)

setwd("/srv/shiny-server/HBV3h_meps")

source(paste(getwd(),"/R/read_flood_forecasting_meps_data.R",sep=""))

load_flood_data ()

closeAllConnections()