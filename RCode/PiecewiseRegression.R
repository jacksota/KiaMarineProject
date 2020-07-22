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
reg.data <- data.frame()
for(i in 1:dim(kiah.data)[1]){
  if(kiah.data$PositiveTestedActiveCasesLast45Days[i] != 0){
    old.y <- t(kiah.data[i,c((dim(kiah.data)[2] - 13):(dim(kiah.data)[2] - 7))])
    old.reg <- coef(summary(lm(old.y ~ x)))
    new.y <- t(kiah.data[i,c((dim(kiah.data)[2] - 6):dim(kiah.data)[2])])
    new.reg <- coef(summary(lm(new.y ~ x)))
    test.stat <- (new.reg[2,1] - old.reg[2,1])/sqrt(new.reg[2,2]^2 + old.reg[2,2]^2)
    for.out <- data.frame(
      kiah.data$State[i],
      kiah.data$County[i],
      kiah.data$RtByState[i],
      old.reg[2,1],
      old.reg[2,2],
      old.reg[2,4],
      new.reg[2,1],
      new.reg[2,2],
      new.reg[2,4],
      test.stat,
      pt(abs(test.stat), (7 + 7 - 4), lower.tail = FALSE)
    )
    names(for.out) <- c("State", "County", "StateRt", "OldSlope", "OldError", "OldPValue", "NewSlope", "NewError", "NewPValue", "TestStat", "PValue")
    reg.data <- rbind(reg.data, for.out)
  }
  print(paste0(100*round(i/dim(kiah.data)[1], 4), "% - Regressions"))
}

# Trouble Counties --------------------------------------------------------
reg.data[reg.data$PValue <= .05 & reg.data$TestStat > 0,]

# Safe Counties -----------------------------------------------------------
reg.data[reg.data$PValue <= .05 & reg.data$TestStat < 0,]
