### Necessities

# Data set Link: https://www.kaggle.com/datasets/faa/wildlife-strikes

data <- read.csv("airstrike.csv")

data[data == ""] <- NA


head(data)
summary(data)
