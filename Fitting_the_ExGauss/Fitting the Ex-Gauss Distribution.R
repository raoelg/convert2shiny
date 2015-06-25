#expression({
#  rts = rexgauss(2^numbr, mu, sigma, 1/tau); 
#  fit = try(fitExgauss(rts)); 
#  if(class(fit)!="try-error") c(fit$a_par, fit$a_s.e.) else c(NA,NA,NA,NA,NA,NA)
#})


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

simulate = FALSE # Simulate draw and fit 100 times?, 


##! MAIN COMPUTATIONS

# generate RTs
RTs = rexgauss(2^numbr, mu, sigma, 1/tau)


# simulate fitting
sims = rnorm(100)


##! MAIN PAGE 

# plot 
hist(RTs, nbins, freq=FALSE, col=color, main="Histogram of Response Times", xlab="time")
curve(dexgauss(x, mu, sigma, 1/tau), par("usr")[1], par("usr")[2], lwd=3, col="darkgreen", add=TRUE)
fit = fitExgauss(RTs)
curve(dexgauss(x, fit$par[1], fit$par[2], fit$par[3]), par("usr")[1], par("usr")[2], lwd=3, col=3, lty=2, add=TRUE)

# Note
cat("This plot is a histogram of the simulated response times, with the true and estimated exgauss pdf superimposed. The dark green continuous line is true pdf, light green dashed line is estimated pdf.")

# Parameter estimation
fit = fitExgauss(RTs)
fit

# Simulation results
y = sims
if (simulate) {
  ests = do.call(rbind, y['a_par',])
  my = colMeans(ests, na.rm = TRUE)
  sy = apply(ests, 2, sd, na.rm = TRUE)
  ny = apply(ests, 2, function(x) sum(!is.na(x)))
  ses = do.call(rbind, y['a_s.e.',])
  mse = colMeans(ses, na.rm = TRUE)
  df = data.frame(`Ave Est` = my, `Ave Est SE` = mse, `Std Dev` = sy, `N` = ny)
  rownames(df) = c("mu", "sigma","lambda")
  df
}
else {cat("No simulations done.")}

# plot Simulation results
  y = sims
if (simulate) {
  ests = do.call(rbind, y['a_par',])
  boxplot(ests,horizontal = TRUE); 
  abline(v=mu, lty=2, col=2)
  abline(v=sigma, lty=2, col=3)
  abline(v=tau, lty=2, col=4)
}  
