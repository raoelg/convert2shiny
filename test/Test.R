
source("../exgauss/exgauss_routines.R")

##! INPUT

mu = 210      # Value of μ, seq(10,2000, by=10)
sigma = 20    # Value of σ, seq(0.1, 200, by=4.4)
tau = 40      # Value of τ (= 1/λ), seq(1,80, len=30)


numbr = 6   # Number of RTs, 2:12
              # number of trials = 2^n 
              # 4, 8, 16, 32, 64, 128, 256, 512, 1024  etc
nbins = 20    # Number of histogram bins, seq(10, 100, by=10)
color = "red" # Histogram color, c("red", "green", "blue", "orange", "yellow", "pink", "cyan", "magenta", "purple","gray","transparent")



##! MAIN COMPUTATIONS
# These are computations that need to be done whenever input values change but don't produce visible output
# Each computation has to start with a comment, followed by statements which include assignments to new variable names

# generate RTs
RTs = rexgauss(2^numbr, mu, sigma, 1/tau)


##! MAIN PAGE 

# plot Histogram of Response Times
hist(RTs, nbins, freq=FALSE, col=color, main="", xlab="time")
curve(dexgauss(x, mu, sigma, 1/tau), par("usr")[1], par("usr")[2], lwd=3, col="darkgreen", add=TRUE)
fit = fitExgauss(RTs)
curve(dexgauss(x, fit$par[1], fit$par[2], fit$par[3]), par("usr")[1], par("usr")[2], lwd=3, col=3, lty=2, add=TRUE)

# Note
cat("This plot is a histogram of the simulated response times, with the true and estimated exgauss pdf superimposed. The dark green continuous line is true pdf, light green dashed line is estimated pdf.")

# Parameter estimation
fit = fitExgauss(RTs)
fit

