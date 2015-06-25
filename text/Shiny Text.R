
##! INPUT
dataset = "rock"  # Choose the dataset:, c("rock", "pressure", "cars")
obs = 10          # Number of observations to view:


##! MAIN COMPUTATIONS
# return the requested dataset
library(datasets)
data = switch(dataset, "rock" = rock, "pressure" = pressure, "cars" = cars)


##! MAIN PAGE
# tabulate the first rows of the data set
head(data, n = obs)

