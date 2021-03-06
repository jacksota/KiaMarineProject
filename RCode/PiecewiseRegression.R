rm(list = ls())

# Loading Needed Functions and packages -----------------------------------
source("./RFunctions/LoadLibrary.R")
load.library(c(
  "lubridate",
  "stringr",
  "xlsx"
))

# Loading Data ------------------------------------------------------------
kiah.files <- list.files("./Data")
kiah.files <- kiah.files[order(kiah.files, decreasing = TRUE)]
kiah.data <- read.csv(paste0("./Data/", kiah.files[1]))

# Cleaning data -----------------------------------------------------------
names(kiah.data)[8:dim(kiah.data)[2]] <- as.character(as.Date(gsub(".", "/", as.character(substr(names(kiah.data)[8:dim(kiah.data)[2]], 2, nchar(names(kiah.data)[8:dim(kiah.data)[2]]))), fixed = TRUE),"%m/%d/%y"))
kiah.data <- kiah.data[,c(1:7,dim(kiah.data)[2]:8)]
kiah.data[is.na(kiah.data)] <- 0

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

# Creating plot -----------------------------------------------------------
x <- reg.data$SlopeDiff
y <- reg.data$StateRt
point.size <- 1

plot(1,1); dev.off()
setwd("./Graphs")
pdf("Slope and State Rt.pdf")

# All Points
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

# Axes
axis(1, seq(-500, 3500, 20), seq(-500, 3500, 20))
axis(2, seq(-1, 2, .05), seq(-1, 2, .05))
abline(h = 1)
abline(v = 0)

# Changing point colors
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
  y[reg.data$StateRt < 1 & reg.data$SlopeDiff < 0] ~ x[reg.data$StateRt < 1 & reg.data$SlopeDiff <= 0],
  col = 'green',
  pch = 16,
  cex = point.size
)

# Adding Legend
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

# Adding in Marine Counts
text(x = 60, y = 1.125, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff > 0 & reg.data$StateRt > 1])[1])
text(x = -140, y = 1.125, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff <= 0 & reg.data$StateRt > 1])[1])
text(x = 60, y = .98, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff > 0 & reg.data$StateRt < 1])[1])
text(x = -140, y = .98, labels = sum(reg.data$MarineCount[reg.data$SlopeDiff <= 0 & reg.data$StateRt < 1])[1])
dev.off()
setwd("../")

# Graph demonstrating Piecewise regressions -------------------------------
place.use <- "St. Charles, MO"
tmp.data <- kiah.data[paste0(kiah.data$County, ", ", kiah.data$State) == place.use,]
tmp.x <- 1:14
tmp.y <- t(tmp.data[1,c((dim(tmp.data)[2] - 13):(dim(tmp.data)[2]))])
plot(1,1); dev.off()

setwd("./Graphs")
pdf("Piecewise Regression.pdf")
# Creating plot
plot(
  tmp.y ~ tmp.x,
  main = paste0(
    place.use,
    " (",
    as.character(month(as.Date(names(kiah.data)[dim(kiah.data)[2] - 13]), label = TRUE, abbr = FALSE)),
    " ",
    day(as.Date(names(kiah.data)[dim(kiah.data)[2] - 13])),
    " - ",
    as.character(month(as.Date(names(kiah.data)[dim(kiah.data)[2]]), label = TRUE, abbr = FALSE)),
    " ",
    day(as.Date(names(kiah.data)[dim(kiah.data)[2]])),
    ")"
  ),
  xlab = "Days",
  ylab = "Confirmed Cases",
  bty = "n",
  xaxt = "n",
  yaxt = "n",
  type = "n"
)

# Axes
axis(1, seq(-4, 16, 4), c(NA, as.character(seq(as.Date(names(kiah.data)[dim(kiah.data)[2] - 13]), as.Date(names(kiah.data)[dim(kiah.data)[2]]), by = 4)), NA))
axis(2, seq(1000, 2200, 200), seq(1000, 2200, 200))

# Adding points
points(tmp.y[1:7] ~ tmp.x[1:7], col = "red", pch = 16)
points(tmp.y[8:14] ~ tmp.x[8:14], col = "blue", pch = 16)

# Adding lines
old.reg <- coef(summary(lm(tmp.y[1:7] ~ tmp.x[1:7])))
x <- seq(.5, 7.5, .5)
y <- x*old.reg[2,1] + old.reg[1,1]
points(y ~ x, col = "red", type = "l", lwd = 2)

new.reg <- coef(summary(lm(tmp.y[8:14] ~ tmp.x[8:14])))
x <- seq(7.5, 14.5, .5)
y <- x*new.reg[2,1] + new.reg[1,1]
points(y ~ x, col = "blue", type = "l", lwd = 2)

# Adding legend
legend(
  "topleft",
  legend = c(
    paste0("Prior Week (Slope = ", round(old.reg[2,1], 2), ")"),
    paste("Current Week (Slope = ", round(new.reg[2,1], 2), ")"),
    paste0("Marine Count: ", tmp.data$MarineCnts[1])
  ),
  col = c("red", "blue", NA),
  pch = c(16, 16, NA),
  lty = c(1, 1, NA),
  lwd = c(2, 2, NA),
  bty = "n"
)
dev.off()
setwd("../")

# Creating xlsx File ------------------------------------------------------
# Create workbook
wb <- createWorkbook()

# Get Q1 data
tmp.data <- kiah.data[paste0(kiah.data$State, " - ", kiah.data$County)%in%paste0(reg.data$State[reg.data$SlopeDiff > 0 & reg.data$StateRt > 1], " - ", reg.data$County[reg.data$SlopeDiff > 0 & reg.data$StateRt > 1]),]

# Create sheet and add data
wb.sheet <- createSheet(wb, sheetName = "State Bad Local Bad")
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(kiah.data)[2])

# Get Q2 data
tmp.data <- kiah.data[paste0(kiah.data$State, " - ", kiah.data$County)%in%paste0(reg.data$State[reg.data$SlopeDiff <= 0 & reg.data$StateRt > 1], " - ", reg.data$County[reg.data$SlopeDiff <= 0 & reg.data$StateRt > 1]),]
# Create sheet and add data
wb.sheet <- createSheet(wb, sheetName = "State Bad Local Good")
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(kiah.data)[2])

# Get Q3 data
tmp.data <- kiah.data[paste0(kiah.data$State, " - ", kiah.data$County)%in%paste0(reg.data$State[reg.data$SlopeDiff > 0 & reg.data$StateRt < 1], " - ", reg.data$County[reg.data$SlopeDiff > 0 & reg.data$StateRt < 1]),]

# Create sheet and add data
wb.sheet <- createSheet(wb, sheetName = "State Good Local Bad")
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(kiah.data)[2])

# Get Q4 data
tmp.data <- kiah.data[paste0(kiah.data$State, " - ", kiah.data$County)%in%paste0(reg.data$State[reg.data$SlopeDiff <= 0 & reg.data$StateRt < 1], " - ", reg.data$County[reg.data$SlopeDiff <= 0 & reg.data$StateRt < 1]),]

# Create sheet and add data
wb.sheet <- createSheet(wb, sheetName = "State Good Local Good")
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(kiah.data)[2])

# Get Other data
tmp.data <- kiah.data[!(paste0(kiah.data$State, " - ", kiah.data$County)%in%paste0(reg.data$State, " - ", reg.data$County)),]

# Create sheet and add data
wb.sheet <- createSheet(wb, sheetName = "No New Cases")
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(kiah.data)[2])

# Get Other data
tmp.data <- data.frame(
  c(
    "State Bad Local Bad",
    "State Bad Local Good",
    "State Good Local Bad",
    "State Good Local Good",
    "No New Cases"
  ),
  c(
    "Corresponds to Q1 in graph.  Increasing Rt for the State and average number of cases per day increasing for the county.",
    "Corresponds to Q2 in the graph.  Increasing Rt for the State, but average number of cases per day decreasing for the county.  Includes counties with no difference in Slope but increasing Rt.",
    "Corresponds to Q4 in the graph.  Decreasing Rt for the State, but average number of cases per day increasing for the county.",
    "Correpsonds to Q3 in the graph. Decreasing Rt for the State and average number of cases per day decreasing for the county.  Includes counties with no difference in Slope and decreasing Rt.",
    "No new confirmed cases in the last 45 days.  Does not appear on the graph."
  )
)
names(tmp.data) <- c("Tab", "Explanation")

# Create sheet and add data
wb.sheet <- createSheet(wb, sheetName = "Data Dictionary")
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)

# Saving workbook
saveWorkbook(wb, paste0("./Data/Infection by County Break Down from ", as.Date(names(kiah.data)[dim(kiah.data)[2]]) + 1, ".xlsx"))

# Writing out regression data ---------------------------------------------
write.csv(reg.data, paste0("./Data/Regression Results for ", as.Date(names(kiah.data)[dim(kiah.data)[2]]) + 1, ".csv"), row.names = FALSE)

