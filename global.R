setwd("/srv/shiny-server/HBV3h_meps")

#source(paste(getwd(),"/R/read_flood_forecasting_meps_data.R",sep=""))
library(lubridate)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(grid)

#load("data/update_time.RData")
load("data/group1.RData")
load("data/group2.RData")
load("data/group3.RData")
load("data/group4.RData")
load("data/group5.RData")
load("data/group6.RData")
UpdateInfo<-file.info("data/HBVmeps.RData")$mtime
