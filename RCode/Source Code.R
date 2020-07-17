rm(list = ls())

# Load functions and packages ---------------------------------------------
source("./RFunctions/LoadLibrary.R")
load.library(c(
  
))

# Read in the data --------------------------------------------------------
kiah.data <- read.csv("./Data/MarineTrainingAnalysis07172020.csv")
start.date <- as.Date("2020-06-02")
end.date <- as.Date("2020-07-16")

# Cleaning up data --------------------------------------------------------
names(kiah.data)[8:dim(kiah.data)[2]] <- as.character(rev(seq(start.date, end.date,1)))
kiah.data <- kiah.data[,na.omit(match(c(names(kiah.data)[1:7], as.character(seq(start.date, end.date,1))), names(kiah.data)))]
use.data <- kiah.data[,c(1:7, (dim(kiah.data)[2] - 14):dim(kiah.data)[2])]

# Creating % increase/decrease by county ----------------------------------






# Creating analysis -------------------------------------------------------









