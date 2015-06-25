

##! INPUT

datafile = "data.csv"                  # Choose a CSV file to upload, c("text/csv", "text/plain")
depvar = "area"	                       # Specify the dependent variable, names(data)
idepvar = c("peri","shape")            # Select independent variables, setdiff(names(data), depvar)

##! MAIN COMPUTATIONS

# read the data file
data = read.csv(datafile$datapath)


# fit the model
model = as.formula(paste(depvar, "~", paste(idepvar, collapse = "+")))
fit = lm(model, data)



##! MAIN PAGE

# tabulate Listing of the data read
head(data)

# print Model
print(model)

# dump Summary of the linear (multiple) regression result
summary(fit)

# plot Diagnostics
plot(fitted(fit), data[, depvar])
abline(lm(data[, depvar] ~ fitted(fit)), col="red")

