rm(list = ls())

# Loading Needed Functions and packages -----------------------------------
source("./RFunctions/LoadLibrary.R")
load.library(c(
  "stringr"
))

# Loading Data ------------------------------------------------------------
kiah.files <- list.files("./Data")
kiah.files <- kiah.files[order(kiah.files, decreasing = TRUE)]
kiah.data <- read.csv(paste0("./Data/", kiah.files[1]))

# Cleaning data -----------------------------------------------------------
names(kiah.data)[8:dim(kiah.data)[2]] <- as.character(as.Date(gsub(".", "/", as.character(substr(names(kiah.data)[8:dim(kiah.data)[2]], 2, nchar(names(kiah.data)[8:dim(kiah.data)[2]]))), fixed = TRUE),"%m/%d/%y"))
kiah.data <- kiah.data[,c(1:7,dim(kiah.data)[2]:8)]

# Creating piecewise regressions ------------------------------------------
x <- 1:7
old.reg <- list()
new.reg <- list()
for(i in 1:dim(kiah.data)[1]){
  if(kiah.data$PositiveTestedActiveCasesLast45Days[i] != 0){
    old.y <- t(kiah.data[i,c((dim(kiah.data)[2] - 13):(dim(kiah.data)[2] - 7))])
    old.reg[[i]] <- coef(summary(lm(old.y ~ x)))
    names(old.reg)[i] <- paste0(kiah.data$State[i], " - ", kiah.data$County[i])
    new.y <- t(kiah.data[i,c((dim(kiah.data)[2] - 6):dim(kiah.data)[2])])
    new.reg[[i]] <- coef(summary(lm(new.y ~ x)))
    names(new.reg)[i] <- paste0(kiah.data$State[i], " - ", kiah.data$County[i])
  }
  print(paste0(100*round(i/dim(kiah.data)[1], 4), "% - Regressions"))
}

















