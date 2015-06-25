

##! INPUT

integer = 500            # Integer:, 0:1000                                   					     #  --> sliderInput min=0, max=1000, value=500, step=1
decimal = 0.5            # Decimal:, seq(0, 1, by=0.1)                        					     #  --> sliderInput min=0, max=1, value=0.5, step=0.1
range = c(200, 500)      # Range:, 1:1000                                       						 #  --> sliderInput min=1, max=1000, value=c(200,500), step=1


dataset = "rock"         # Choose a dataset, c("rock", "pressure", "cars", "iris")             # --> selectInput
varsa = "peri"           # Choose dependent, names(data)                                       # --> selectInput choices=names(data)
vars = c("area","peri")  # Choose predictors,  setdiff(names(data),varsa)                      # --> selectInput limit choices to names(data) not selected in varsa, multiple = TRUE
nobs = 10                # Number of observations to view                   							     # --> numericInput
                         # Note: while the data view will                        					     # --> helpText
                         # show only the specified number of observation,
                         # the summary is based on all data.

file1 = "input.txt"      # Choose CSV file, c('text/csv', 'text/comma-separated-values', 'text/plain') # --> fileInput
header = TRUE		         # Header 			                                                     # --> checkboxInput
sep = 'Comma'            # Separator, c(Comma = ',', Semicolon = ';', Tab = '\t')						 # --> radioButtons
quote = 'Double Quote'   # Quote, c(None='', `Double Quote`='"', `Single Quote`="'")         # --> radioButtons



##! MAIN COMPUTATIONS

# compute which data set
require(datasets)
data = switch(dataset, "rock" = rock, "pressure" = pressure, "cars" = cars, "iris" = iris)


# compute slider values data frame
sliderValues = data.frame(
	Name = c("Integer", "Decimal", "Range", "Dataset", "Number of observation", "Header", "Separator", "Quote"),
	Value = as.character(c(integer, decimal, paste(range,collapse="-"), dataset, nobs, header, sep, quote)),
	stringsAsFactors = FALSE
)

# read data from the uploaded file
if(is.data.frame(file1))
  updata =  read.csv(file1$datapath)


##! MAIN PAGE
# tabulate slider values
sliderValues

# dump chosen dependent variable
varsa

# dump selected predictor variables
vars

# dump a summary of data
if (!is.null(vars) && all(vars %in% names(data)))
  summary(data[, vars, drop = FALSE])
else
  summary(data)


# tabulate the first few rows of data
if (!is.null(vars) && all(vars %in% names(data)))
  head(data[, vars, drop = FALSE], nobs)
else
  head(data, nobs)


# plot histogram of normal sample
hist(rnorm(integer), main=paste("Histogram of", integer, "observations from N(0,1)"))

# tabulate the content data frame associated with file1
file1

# dump first line of the file in file1
if (is.data.frame(file1))
  readLines(file1$datapath, 6)


# data table from the file
updata

