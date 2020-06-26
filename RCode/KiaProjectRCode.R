rm(list = ls())

# Reading in data ---------------------------------------------------------
kiah.data <- read.csv("./Data/MarineJulyTrainingAnalysis.csv")
names(kiah.data)[8:dim(kiah.data)[2]] <- as.character(rev(seq(as.Date('2020-05-25'),as.Date('2020-06-24'),1)))

# Examine by State --------------------------------------------------------



# Examine by County -------------------------------------------------------



