rm(list = ls())

# Reading in data ---------------------------------------------------------
kiah.data <- read.csv("./Data/MarineJulyTrainingAnalysis.csv")
names(kiah.data)[8:dim(kiah.data)[2]] <- as.character(rev(seq(as.Date('2020-05-29'),as.Date('2020-07-12'),1)))
kiah.data <- kiah.data[,na.omit(match(c(names(kiah.data)[1:7], as.character(seq(as.Date('2020-05-29'),as.Date('2020-07-12'),1))), names(kiah.data)))]

# Examine by State --------------------------------------------------------
unq.states <- unique(kiah.data$State[order(kiah.data$State)])
state.data <- data.frame()
for(i in 1:length(unq.states)){
  tmp.data <- kiah.data[kiah.data$State == unq.states[i],]
  tmp.data[is.na(tmp.data)] <- 0
  for.out <- data.frame(
    as.character(unq.states[i]),
    as.Date(names(tmp.data)[c(8:dim(tmp.data)[2])]),
    sum(tmp.data$MarineCnts),
    apply(tmp.data[,c(8:dim(tmp.data)[2])], 2, sum)
  )
  names(for.out) <- c("state", "day", "marine.count", "virus.count")
  state.data <- rbind(state.data, for.out)
  print(paste0(100*round(i/length(unq.states), 4), "% - Combining States"))
}

plot(1,1); dev.off()
par(mfrow = c(1, 2))
plot(
  state.data$virus.count ~ as.Date(state.data$day),
  col = 'blue',
  type = 'n',
  bty = 'n',
  # xaxt = 'n',
  yaxt = 'n',
  xlab = "Days",
  ylab = "Confirmed Infections",
  main = "Corona By State"
)
rect(18400, -20000,184400, 1600000, col = "grey90")
x.lines <- seq(as.Date("2020-05-29"), as.Date("2020-07-12"), 7)
for(i in 1:length(x.lines)){abline(v = x.lines[i], col = "white")}
y.lines <- seq(-20000, 1600000, 20000)
for(i in 1:length(y.lines)){abline(h = y.lines[i], col = "white")}
axis(1, seq(18400, 184400, 40), rep(NA, 2))
axis(2, seq(-20000, 1600000, 20000))
for(i in 1:length(unq.states)){
  points(
    state.data$virus.count[state.data$state == unq.states[i]] ~ state.data$day[state.data$state == unq.states[i]],
    col = i,
    type = 'l'
  )
  print(paste0(100*round(i/length(unq.states), 4), "% - Plotting States"))
}


plot(
  state.data$virus.count ~ as.Date(state.data$day),
  col = 'blue',
  type = 'n',
  bty = 'n',
  # xaxt = 'n',
  yaxt = 'n',
  xlab = "Days",
  ylab = "Confirmed Infections",
  main = "Corona By State",
  ylim = c(0, 40000)
)
rect(18400, -20000,184400, 160000, col = "grey90")
x.lines <- seq(as.Date("2020-05-25"), as.Date("2020-06-22"), 7)
for(i in 1:length(x.lines)){abline(v = x.lines[i], col = "white")}
y.lines <- seq(-20000, 160000, 20000)
for(i in 1:length(y.lines)){abline(h = y.lines[i], col = "white")}
axis(1, seq(18400, 18440, 40), rep(NA, 2))
axis(2, seq(-20000, 160000, 20000))
for(i in 1:length(unq.states)){
  points(
    state.data$virus.count[state.data$state == unq.states[i]] ~ state.data$day[state.data$state == unq.states[i]],
    col = i,
    type = 'l'
  )
  print(paste0(100*round(i/length(unq.states), 4), "% - Plotting Sub States"))
}

# What do we think will be the most useful? -------------------------------
# R0?  Population density?
# Growth rate values?
# RT - tested vs untested positives
# How many people will they give it to?  Projected growth rates
# New cases per ??? per day
# What's the likelihood that any individual marine will show up with an active case of COVID and be infectious?

# What are they looking for? ----------------------------------------------
# What can give us insight into what to do moving forward?

# Report outline ----------------------------------------------------------
# Problem statement
# Data
# Limitations
# Methods
# Conclusions





