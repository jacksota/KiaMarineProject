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
  # Are there cases to analyze?
  if(kiah.data$PositiveTestedActiveCasesLast45Days[i] != 0){
    # Regression of prior s weeks
    old.y <- t(kiah.data[i,c((dim(kiah.data)[2] - 13):(dim(kiah.data)[2] - 7))])
    old.reg <- coef(summary(lm(old.y ~ x)))
    
    # Regression of prior week
    new.y <- t(kiah.data[i,c((dim(kiah.data)[2] - 6):dim(kiah.data)[2])])
    new.reg <- coef(summary(lm(new.y ~ x)))
    
    # Calculation of the t-test (2 slope comparison)
    test.stat <- (new.reg[2,1] - old.reg[2,1])/sqrt(new.reg[2,2]^2 + old.reg[2,2]^2)
    
    # Creating out file
    for.out <- data.frame(
      kiah.data$State[i],
      kiah.data$County[i],
      kiah.data$RtByState[i],
      kiah.data$MarineCnts[i],
      old.reg[2,1],
      old.reg[2,2],
      old.reg[2,4],
      new.reg[2,1],
      new.reg[2,2],
      new.reg[2,4],
      test.stat,
      
      # P-value calculation
      pt(abs(test.stat), (7 + 7 - 4), lower.tail = FALSE)
    )
    names(for.out) <- c("State", "County", "StateRt", "MarineCount", "OldSlope", "OldError", "OldPValue", "NewSlope", "NewError", "NewPValue", "TestStat", "PValue")
    reg.data <- rbind(reg.data, for.out)
  }
  print(paste0(100*round(i/dim(kiah.data)[1], 4), "% - Regressions"))
}
reg.data$SlopeDiff <- reg.data$NewSlope - reg.data$OldSlope

# Trouble Counties --------------------------------------------------------
reg.data[reg.data$PValue <= .05 & reg.data$TestStat > 0,]

# Safe Counties -----------------------------------------------------------
reg.data[reg.data$PValue <= .05 & reg.data$TestStat < 0,]
kiah.data[kiah.data$PositiveTestedActiveCasesLast45Days == 0,]

# Creating plot -----------------------------------------------------------
x <- reg.data$SlopeDiff
y <- reg.data$StateRt
point.size <- 1

plot(1,1); dev.off()
setwd("./Graphs")
pdf("Slope and State Rt.pdf")
plot(
  y ~ x,
  main = "Slope Differences and State Rt",
  xlab = "Slope Difference",
  ylab = "State Rt",
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  col = "blue",
  pch = 16,
  cex = point.size
  # type = "n"
)
axis(1, seq(-500, 3500, 20), seq(-500, 3500, 20))
axis(2, seq(-1, 2, .05), seq(-1, 2, .05))
abline(h = 1)
abline(v = 0)
points(
  y[reg.data$StateRt > 1 & reg.data$SlopeDiff > 0] ~ x[reg.data$StateRt > 1 & reg.data$SlopeDiff > 0],
  col = 'red',
  pch = 16,
  cex = point.size
)
points(
  y[reg.data$StateRt < 1 & reg.data$SlopeDiff > 0] ~ x[reg.data$StateRt < 1 & reg.data$SlopeDiff > 0],
  col = 'yellow',
  pch = 16,
  cex = point.size
)
points(
  y[reg.data$StateRt < 1 & reg.data$SlopeDiff < 0] ~ x[reg.data$StateRt < 1 & reg.data$SlopeDiff < 0],
  col = 'green',
  pch = 16,
  cex = point.size
)
# text(
#   y ~ x,
#   labels = reg.data$MarineCount[reg.data$PValue <= .05 & reg.data$TestStat > 0],
#   cex = reg.data$MarineCount[reg.data$PValue <= .05 & reg.data$TestStat > 0]/5
# )
legend("topleft",
  legend = c(
    "State Bad, Local Bad", 
    "State Good, Local Bad", 
    "State Bad, Local Good",
    "State Good, Local Good",
    paste0(dim(kiah.data[kiah.data$PositiveTestedActiveCasesLast45Days == 0,])[1], " Counties With No Cases")
  ),
  pch = c(16,16,16,16, NA),
  col = c("red", "yellow", "blue", "green", NA),
  bty = "n"
)
# text(x = 60, y = 1.125, labels = dim(reg.data[reg.data$SlopeDiff > 0 & reg.data$StateRt > 1,])[1])
# text(x = -140, y = 1.125, labels = dim(reg.data[reg.data$SlopeDiff < 0 & reg.data$StateRt > 1,])[1])
# text(x = 60, y = .98, labels = dim(reg.data[reg.data$SlopeDiff > 0 & reg.data$StateRt < 1,])[1])
# text(x = -140, y = .98, labels = dim(reg.data[reg.data$SlopeDiff < 0 & reg.data$StateRt < 1,])[1])

text(x = 60, y = 1.125, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff > 0 & reg.data$StateRt > 1])[1])
text(x = -140, y = 1.125, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff < 0 & reg.data$StateRt > 1])[1])
text(x = 60, y = .98, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff > 0 & reg.data$StateRt < 1])[1])
text(x = -140, y = .98, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff < 0 & reg.data$StateRt < 1])[1])
dev.off()