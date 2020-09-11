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
for(i in 9:22){
  new.data <- use.data[,i]
  old.data <- use.data[,i-1]
  use.data[,dim(use.data)[2]+1] <- (new.data - old.data)/old.data
  names(use.data)[dim(use.data)[2]] <- paste0("PChange", names(use.data)[i])
}
use.data[is.na(use.data)] <- 0

# Visualization -----------------------------------------------------------
x <- as.Date(names(use.data)[9:22])
y <- data.frame(t(use.data[1,23:dim(use.data)[2]]))
plot(
  y$X1~x,
  type = "b",
  col = "blue",
  pch = 16
)
abline(lm(y$X1~x), col = "red")

# Creating analysis -------------------------------------------------------
tmp.x <- as.Date(names(use.data)[16:22])
lm.data <- data.frame()
for(i in 1:dim(use.data)[1]){
  if(use.data$PositiveTestedActiveCasesLast45Days[i] != 0){
    tmp.y <- as.numeric(use.data[i, 16:22])
    tmp.lm <- coef(summary(lm(tmp.y ~ tmp.x)))
    for.out <- data.frame(
      use.data$State[i],
      use.data$County[i],
      tmp.lm[2,1],
      tmp.lm[2,4],
      use.data$`PChange2020-07-16`[i],
      use.data$`2020-07-16`[i]
    )
    names(for.out) <- c("State", "County", "slope", "p.value", "last.change", "last.count")
    lm.data <- rbind(lm.data, for.out)
  }
  print(paste0(100*round(i/dim(use.data)[1], 4), "% - regressions"))
}

dim(lm.data[lm.data$p.value <= .05,])[1]/dim(lm.data)[1]
lm.use <- lm.data[lm.data$p.value <= .05,]

new.data <- data.frame()
for(i in 1:dim(lm.use)){
  tmp.data <- as.numeric(use.data[use.data$State == lm.use$State[i] & use.data$County == lm.use$County[i], 23:dim(use.data)[2]])
  for.out <- data.frame(
    
  )
  names(for.out) <- c()
}


# Piecewise regression with post hoc slope comparisons



