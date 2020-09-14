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

# Change in Rates are lower counties pre training
# Training from July 26th to August 8th (7/26-8/8)
# 3 2 week period comparisons

# Creating comparative regressions ----------------------------------------
x <- 1:21
out.data <- data.frame()
for(i in 1:dim(kiah.data)[1]){
  pre.y <- as.numeric(kiah.data[i,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-07-25"), "days"))), names(kiah.data)))])
  meet.y <- as.numeric(kiah.data[i,na.omit(match(c(as.character(seq(as.Date("2020-07-26"), as.Date("2020-08-15"), "days"))), names(kiah.data)))])
  post.y <- as.numeric(kiah.data[i,na.omit(match(c(as.character(seq(as.Date("2020-08-16"), as.Date("2020-09-05"), "days"))), names(kiah.data)))])
  pre.fit <- coef(summary(lm(pre.y ~ x)))
  meet.fit <- coef(summary(lm(meet.y ~ x)))
  post.fit <- coef(summary(lm(post.y ~ x)))
  rm.test <- (meet.fit[2,1] - pre.fit[2,1])/(meet.fit[2,2]^2 + pre.fit[2,2]^2)
  rp.test <- (post.fit[2,1] - pre.fit[2,1])/(post.fit[2,2]^2 + pre.fit[2,2]^2)
  mp.test <- (post.fit[2,1] - meet.fit[2,1])/(post.fit[2,2]^2 + meet.fit[2,2]^2)
  for.out <- data.frame(
    kiah.data$State[i],
    kiah.data$County[i],
    kiah.data$RtByState[i],
    kiah.data$MarineCnts[i],
    round(pre.fit[2,1], 4), round(pre.fit[2,2], 4), round(pre.fit[2,4], 4),
    round(meet.fit[2,1], 4), round(meet.fit[2,2], 4), round(meet.fit[2,4], 4),
    round(post.fit[2,1], 4), round(post.fit[2,2], 4), round(post.fit[2,4], 4),
    round(rm.test, 4), round(2*pt(abs(rm.test), df = (21 + 21 - 4), lower.tail = FALSE), 4),
    round(rp.test, 4), round(2*pt(abs(rp.test), df = (21 + 21 - 4), lower.tail = FALSE), 4),
    round(mp.test, 4), round(2*pt(abs(mp.test), df = (21 + 21 - 4), lower.tail = FALSE), 4)
  )
  names(for.out) <- c(
    "State", "County", "StateRt", "MarineCount",
    "PreSlope", "PreError", "PrePValue",
    "MeetSlope", "MeetError", "MeetPValue",
    "PostSlope", "PostError", "PostPValue",
    "PreMeetTestStat", "PreMeetPValue",
    "PrePostTestStat", "PrePostPValue",
    "MeetPostTestStat", "MeetPostPValue"
  )
  out.data <- rbind(out.data, for.out)
  print(paste0(100*round(i/dim(kiah.data)[1], 4), "%"))
}

pre.data <- out.data[,c(1:7, 14:15)]
pre.data$Type <- "pre"
names(pre.data)[5:9] <- c(
  "Slope",
  "StdError",
  "SPValue",
  "TestStat",
  "TestPValue"
)
meet.data <- out.data[,c(1:4, 8:10, 18:19)]
meet.data$Type <- "meet"
names(meet.data) <- names(pre.data)
post.data <- out.data[,c(1:4, 11:13, 16:17)]
post.data$Type <- "post"
names(post.data) <- names(pre.data)
bh.data <- rbind(pre.data, meet.data, post.data)
bh.data <- bh.data[order(bh.data$TestPValue, decreasing = TRUE),]
bh.data$bhpvalue <- c(1:dim(bh.data)[1])/dim(bh.data)[1]*.05
bh.data$flag <- 0
bh.data$flag[bh.data$TestPValue < bh.data$bhpvalue] <- 1

pre.atad <- bh.data[bh.data$Type == "pre",]
names(pre.atad)[c(5:dim(pre.atad)[2])] <- c(
  "PreSlope",
  "PreStdError",
  "PreSPValue",
  "PreMeetTestValue",
  "PreMeetPValue",
  "Type",
  "PreBHPValue",
  "PreMeetFlag"
)
meet.atad <- bh.data[bh.data$Type == "meet",]
names(meet.atad)[c(5:dim(meet.atad)[2])] <- c(
  "MeetSlope",
  "MeetStdError",
  "MeetSPValue",
  "MeetPostTestValue",
  "MeetPostPValue",
  "Type",
  "MeetPostBHPValue",
  "MeetPostFlag"
)
post.atad <- bh.data[bh.data$Type == "post",]
names(post.atad)[c(5:dim(post.atad)[2])] <- c(
  "PostSlope",
  "PostStdError",
  "PostSPValue",
  "PrePostTestValue",
  "PrePostPValue",
  "Type",
  "PrePostBHPValue",
  "PrePostFlag"
)

merge.1 <- merge(pre.atad[,-10], meet.atad[,-10], by = c("State", "County", "StateRt", "MarineCount"))
merge.2 <- merge(merge.1, post.atad[,-10], by = c("State", "County", "StateRt", "MarineCount"))
final.data <- merge.2[,c(1:7, 12:14, 19:21, 8:11, 15:18, 22:25)]

# Pre and Meet no Diff, Post Greater than Meet
dim(final.data[final.data$PreMeetFlag == 0 & final.data$MeetPostFlag == 1 & final.data$MeetSlope < final.data$PostSlope,c(1:5, 8, 11)])

# Meet greater than Pre, Post Greater than Meet
dim(final.data[final.data$PreMeetFlag == 1 & final.data$PreSlope < final.data$MeetSlope & final.data$MeetPostFlag == 1 & final.data$MeetSlope < final.data$PostSlope,c(1:5, 8, 11)])

# Meet less than Pre, Post Greater than Meet
dim(final.data[final.data$PreMeetFlag == 1 & final.data$PreSlope > final.data$MeetSlope & final.data$MeetPostFlag == 1 & final.data$MeetSlope < final.data$PostSlope,c(1:5, 8, 11)])

# Outputting data
wb <- createWorkbook()

# No Change Pre/Meet
wb.sheet <- createSheet(wb, sheetName = "No Change Pre to Meet")
tmp.data <- final.data[final.data$PreMeetFlag == 0 & final.data$MeetPostFlag == 1 & final.data$MeetSlope < final.data$PostSlope,c(1:5, 8, 11)]
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(tmp.data)[2])

# Increase Pre/Meet
wb.sheet <- createSheet(wb, sheetName = "Increase Pre to Meet")
tmp.data <- final.data[final.data$PreMeetFlag == 1 & final.data$PreSlope < final.data$MeetSlope & final.data$MeetPostFlag == 1 & final.data$MeetSlope < final.data$PostSlope,c(1:5, 8, 11)]
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(tmp.data)[2])

# Decrease Pre/Meet
wb.sheet <- createSheet(wb, sheetName = "Decrease Pre to Meet")
tmp.data <- final.data[final.data$PreMeetFlag == 1 & final.data$PreSlope > final.data$MeetSlope & final.data$MeetPostFlag == 1 & final.data$MeetSlope < final.data$PostSlope,c(1:5, 8, 11)]
addDataFrame(tmp.data, sheet = wb.sheet, row.names = FALSE, startRow = 1, startColumn = 1)
autoSizeColumn(wb.sheet, colIndex = 1:dim(tmp.data)[2])

# Saving workbook
saveWorkbook(wb, paste0("./Data/After Meeting Slopes.xlsx"))

# Create 3 graphs (1 per group)
tmp.data <- kiah.data[kiah.data$County == "Oktibbeha",]
plot(1,1); dev.off()
setwd("./Graphs")
pdf("Oktibbeha County MS.pdf")
plot(
  1,1,
  xlim = c(1,63),
  ylim = c(540, 1563),
  main = "Oktibbeha County MS Infection Count",
  xlab = "Date",
  ylab = "Infected Count",
  bty = "n",
  type = "n",
  col = "blue",
  xaxt = "n",
  yaxt = "n",
  pch = 16
)
rect(-5, 450, 70, 1700, col = "grey85")
axis(1, seq(-5, 75, 10), c(NA, as.character(seq(as.Date("2020-07-05"), as.Date("2020-09-13"), by = 10, units = "days"))))
axis(2, seq(330, 1730, 200), seq(330, 1730, 200))
x.seq <- seq(-5, 75, 10)
for(i in 1:length(x.seq)){abline(v = x.seq[i], col = "white")}
y.seq <- seq(330, 1730, 200)
for(i in 1:length(y.seq)){abline(h = y.seq[i], col = "white")}
x <- 1:63
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-09-05"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "black", type = "b", pch = 16)
x <- 1:21
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-07-25"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "blue", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "blue", type = "l")
x <- 22:42
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-26"), as.Date("2020-08-15"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "green", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "green", type = "l")
x <- 43:63
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-08-16"), as.Date("2020-09-05"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "red", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "red", type = "l")
legend(
  "topleft", 
  bty = "n", 
  legend = c(
    "Prior to Training (11.77 per day)", 
    "During Training (10.59 per day)", 
    "Post Training (19.58 per day)"), 
  pch = c(16,16,16), 
  col = c("blue","green","red"), 
  lty = c(1,1,1)
)
dev.off()

tmp.data <- kiah.data[kiah.data$County == "Callaway",]
plot(1,1); dev.off()
pdf("Callaway County MO.pdf")
plot(
  1,1,
  xlim = c(1,63),
  ylim = c(56, 408),
  main = "Callaway County MO Infection Count",
  xlab = "Date",
  ylab = "Infected Count",
  bty = "n",
  type = "n",
  col = "blue",
  xaxt = "n",
  yaxt = "n",
  pch = 16
)
rect(-5, 40, 70, 1700, col = "grey85")
axis(1, seq(-5, 75, 10), c(NA, as.character(seq(as.Date("2020-07-05"), as.Date("2020-09-13"), by = 10, units = "days"))))
axis(2, seq(20, 440, 60), seq(20,440,60))
x.seq <- seq(-5, 75, 10)
for(i in 1:length(x.seq)){abline(v = x.seq[i], col = "white")}
y.seq <- seq(20, 440, 60)
for(i in 1:length(y.seq)){abline(h = y.seq[i], col = "white")}
x <- 1:63
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-09-05"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "black", type = "b", pch = 16)
x <- 1:21
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-07-25"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "blue", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "blue", type = "l")
x <- 22:42
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-26"), as.Date("2020-08-15"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "green", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "green", type = "l")
x <- 43:63
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-08-16"), as.Date("2020-09-05"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "red", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "red", type = "l")
legend(
  "topleft", 
  bty = "n", 
  legend = c(
    "Prior to Training (1.74 per day)", 
    "During Training (4.22 per day)", 
    "Post Training (10.01 per day)"), 
  pch = c(16,16,16), 
  col = c("blue","green","red"), 
  lty = c(1,1,1)
)
dev.off()

tmp.data <- kiah.data[kiah.data$County == "Plymouth",]
plot(1,1); dev.off()
pdf("Plymouth County IA.pdf")
plot(
  1,1,
  xlim = c(1,63),
  ylim = c(321, 838),
  main = "Plymouth County IA Infection Count",
  xlab = "Date",
  ylab = "Infected Count",
  bty = "n",
  type = "n",
  col = "blue",
  xaxt = "n",
  yaxt = "n",
  pch = 16
)
rect(-5, 100, 70, 1700, col = "grey85")
x.seq <- seq(-5, 75, 10)
for(i in 1:length(x.seq)){abline(v = x.seq[i], col = "white")}
y.seq <- seq(300, 900, 100)
for(i in 1:length(y.seq)){abline(h = y.seq[i], col = "white")}
axis(1, seq(-5, 75, 10), c(NA, as.character(seq(as.Date("2020-07-05"), as.Date("2020-09-13"), by = 10, units = "days"))))
axis(2, seq(300, 900, 100), seq(300, 900, 100))
x <- 1:63
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-09-05"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "black", type = "b", pch = 16)
x <- 1:21
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-05"), as.Date("2020-07-25"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "blue", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "blue", type = "l")
x <- 22:42
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-07-26"), as.Date("2020-08-15"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "green", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "green", type = "l")
x <- 43:63
y <- as.numeric(t(tmp.data[,na.omit(match(c(as.character(seq(as.Date("2020-08-16"), as.Date("2020-09-05"), "days"))), names(kiah.data)))]))
points(y ~ x, col = "red", type = "b", pch = 16)
fit <- coef(summary(lm(y~x)))
z <- fit[1,1] + fit[2,1]*x
points(z~x, col = "red", type = "l")
legend(
  "topleft", 
  bty = "n", 
  legend = c(
    "Prior to Training (4.36 per day)", 
    "During Training (3.92 per day)", 
    "Post Training (16.26 per day)"), 
  pch = c(16,16,16), 
  col = c("blue","green","red"), 
  lty = c(1,1,1)
)
dev.off()


