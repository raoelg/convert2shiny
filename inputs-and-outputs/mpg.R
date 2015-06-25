
##! INPUT

variable = "cyl"	# Variable:, c(Cylinders = 'cyl', Transmission = 'am', Gears = 'gear')
outliers = FALSE	# Show outliers?


##! MAIN COMPUTATIONS

# get the data frame; redefine am for nicer output
library(datasets)
mpgData <- transform(mtcars, am = factor(am, labels = c('Automatic', 'Manual')))

# compute formula text
formulaText <- paste("mpg ~", variable)


##! MAIN PAGE

# show Caption
formulaText

# plot boxplot of mpgData
boxplot(as.formula(formulaText), data=mpgData, outline = outliers)

