#################################
### FRE7241 Homework #6 Solution Tuesday, October 22, 2024
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.

############## Part I
# Summary: Backtest a rolling minimum variance 
# portfolio strategy.

## Run the setup code below

library(rutils)
# Load the stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

## End of setup code


# 1. (10pts)
# Calculate the dates of retstock$GOOGL.
# You can use the functions na.omit() 
# and zoo::index().

datev <- zoo::index(na.omit(retstock$GOOGL))

# You should get the following outputs:
head(datev)
# [1] "2004-08-20" "2004-08-23" "2004-08-24" "2004-08-25" "2004-08-26" "2004-08-27"
tail(datev)
# [1] "2024-08-23" "2024-08-26" "2024-08-27" "2024-08-28" "2024-08-29" "2024-08-30"

# Subset the stock returns to GOOGL (run this):
retp <- retstock[datev]

# You should get the following output:
dim(retp)
# [1] 5042  702

# Remove the stocks with any NA values.
# You can use the functions sapply(), sum(), 
# and is.na().

nonas <- sapply(retp, function(x) sum(is.na(x)))
nonas <- !(nonas > 0)
retp <- retp[, nonas]

# You should get the following output:
dim(retp)
# [1] 5042  457

# Select the in-sample returns (run this):
nstocks <- NCOL(retp)
colv <- colnames(retp)
retis <- retp["/2014"] # In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) # Excess returns


# 2. (10pts)
# Calculate the maximum Sharpe portfolio weights 
# for the in-sample interval.
# Hint: Copy the code from the lecture slides.
# You can use the functions cov(), MASS::ginv(), 
# colMeans(), and names().

covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
colmeanv <- colMeans(retx, na.rm=TRUE)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- colv

# You should get the following output:
head(sort(weightv))
#     AEE       IBM       NSC       EXC       CLX       REG 
# -5.120923 -3.582718 -3.504680 -3.276873 -2.955923 -2.949660 
tail(sort(weightv))
#     DTE      WEC      DUK      CVX       MO      UNP 
# 4.025864 4.029182 4.063581 4.803495 5.207508 5.434806 

# Calculate the maximum Sharpe portfolio returns.
# Hint: Copy the code from the lecture slides.
# Scale the returns by their in-sample volatility:
# pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
# 
# You can use the functions  HighFreq::mult_mat(), 
# rowMeans(), xts(), and cbind().

pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("Equal Weight", "MaxSharpe")

# You should get the following output:
tail(wealthv)
#            Equal Weight    MaxSharpe
# 2024-08-23  0.013936012 -0.021810125
# 2024-08-26 -0.001514155 -0.018024064
# 2024-08-27 -0.001756260  0.032553877
# 2024-08-28 -0.003803954  0.039028198
# 2024-08-29  0.003099209  0.036458435
# 2024-08-30  0.006967333 -0.004785367

# Calculate the in-sample and out-of-sample Sharpe 
# and Sortino ratios.

sqrt(252)*sapply(wealthv["/2014"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv["2015/"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get the following outputs:
#         Equal Weight MaxSharpe
# Sharpe     0.4888424  4.953952
# Sortino    0.5819925  8.685674
# 
#         Equal Weight MaxSharpe
# Sharpe     0.4178739 0.2333733
# Sortino    0.5006801 0.2614252


# 3. (10pts)
# Calculate the minimum variance portfolio returns.
# Scale the returns by their in-sample volatility.
# Hint: Multiply the inverse of the covariance matrix
# by a unit vector.

# Minimum variance weights.
weightv <- drop(invreg %*% rep(1/nstocks, nstocks))
pnlm <- HighFreq::mult_mat(weightv, retp)
pnlm <- rowMeans(pnlm, na.rm=TRUE)
pnlm <- xts::xts(pnlm, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnlm <- pnlm*sd(retew["/2014"])/sd(pnlm["/2014"])
wealthv <- cbind(retew, pnlm)
colnames(wealthv) <- c("Equal Weight", "MinVar")

# You should get the following output:
tail(wealthv)
#            Equal Weight      MinVar
# 2024-08-23  0.013936012 0.004162200
# 2024-08-26 -0.001514155 0.005727193
# 2024-08-27 -0.001756260 0.015947862
# 2024-08-28 -0.003803954 0.024970237
# 2024-08-29  0.003099209 0.039219544
# 2024-08-30  0.006967333 0.019287638


# Calculate the in-sample and out-of-sample Sharpe 
# and Sortino ratios.

sqrt(252)*sapply(wealthv["/2014"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv["2015/"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get the following outputs:
#         Equal Weight   MinVar
# Sharpe     0.4888424 1.441032
# Sortino    0.5819925 2.257272
# 
#         Equal Weight    MinVar
# Sharpe     0.4178739 0.8816314
# Sortino    0.5006801 1.0744102

# Note that the minimum variance portfolio has a worse
# in-sample Sharpe ratio than the maximum Sharpe portfolio,
# but it has a much better out-of-sample Sharpe ratio.

# Note that the minimum variance portfolio is equivalent 
# to the maximum Sharpe portfolio with a return shrinkage 
# intensity equal to 1.
# Because then the individual returns are all equal to 
# the same mean value, and the weights of both 
# portfolios are proportional to each other.


# Plot a dygraph of the cumulative returns of the minimum 
# variance and the maximum Sharpe portfolios.

# Your plot should be similar to portf_minvar_maxsharpe.png

wealthv <- cbind(pnls, pnlm)
colnames(wealthv) <- c("MaxSharpe", "MinVar")
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], 
  main="Maximum Sharpe And Minimum Variance Portfolios") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)


# 4. (20pts)
# Backtest the rolling minimum variance portfolio 
# strategy using HighFreq::back_test().

# Define monthly end points, and shift them to 
# C++ convention.

endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd > (nstocks+1)]
endd <- (endd - 1)
endd[endd < 0] <- 0
npts <- NROW(endd)

# You should get the following outputs:
head(endd)
# [1] 470 490 513 533 555 576
tail(endd)
# [1] 4935 4957 4979 4998 5020 5042
npts
# [1] 219


# Specify the strategy parameters, for the minimum 
# variance weights with a linear constraint that 
# the sum of the weights is equal to 1 (run this):

controlv <- HighFreq::param_portf(method="minvarlin", scalew="sumone")


# Specify the excess returns and a vector of 
# look-back intervals for the rolling strategy.
# Run this:

# Excess returns
raterf <- 0.03/252
retx <- (retp - raterf)
# Look-back intervals for the rolling strategy.
lookbv <- seq(from=16, to=24, by=1)


# Perform a parallel loop over the vector lookbv.
# For each value of the look-back interval lookb, 
# define the start points, and run the function
# HighFreq::back_test() to backtest the rolling
# portfolio strategy for the look-back interval 
# lookb.
# Hint: Copy the code from the lecture slides.
# Use the parallel code for your machine.
# Takes a long time to run!!!

library(parallel)  # Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(lookbv, function(lookb) {
  startp <- c(rep_len(0, lookb), endd[1:(npts-lookb)])
  startp <- (startp - 1)
  startp[startp < 0] <- 0
  HighFreq::back_test(retx=retx, retp=retp,
                      startp=startp, endd=endd, controlv=controlv)
})  # end lapply
profilev <- sapply(pnls, sum)

# Plot the rolling strategy PnLs as a function 
# of the look-back interval.

plot(x=lookbv, y=profilev, t="l", 
     main="MinVar Linear Constraint PnL Versus Look-back Interval",
     xlab="Look-back Interval", ylab="pnl")

# Your plot should be similar to: 
# portf_rolling_minvarlinsumone_lookb_profile.png


# Calculate the optimal look-back interval.

whichmax <- which.max(profilev)
lookb <- lookbv[whichmax]

# You should get the following output:
lookb
# [1] 19


# Select the PnLs for the optimal strategy.
# Calculate its Sharpe and Sortino ratios.

pnls <- pnls[[whichmax]]
pnls <- pnls*sd(retew)/sd(pnls)
wealthv <- xts(cbind(retew, pnls), datev)
colnames(wealthv) <- c("Equal Weight", "minvarlin")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))


# You should get the following output:
#         Equal Weight minvarlin
# Sharpe     0.4560313 0.7471829
# Sortino    0.5440954 0.9560710

# This demonstrates that the rolling minimum 
# variance strategy can outperform the equal 
# weight strategy.
# Portfolios with the minimum variance can
# outperform other portfolios out-of-sample.


# Plot a dygraph of the cumulative wealth.
# Your plot should be similar to: 
# portf_rolling_minvarlinsumone.png

dygraphs::dygraph(cumsum(wealthv)[endd], 
  main="Rolling Minimum Variance Portfolio With Linear Constraint") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



############## Part II
# Summary: Create a shiny app for the efficient 
# frontier of a portfolio of two stocks.
# Solution file is: app_stop_start_strat.R

# 1. (50pts)
# Hint: Adapt the code from the lecture slides.
# Hint: If you want the plot to show up in a new 
# window, you will need to call dev.new() on the 
# Mac, or x11() on Windows.
# Mac users: First install XQuartz then call 
# dev.new(), not x11().
dev.new(widthp <- 6, heightp <- 5, noRStudioGD=TRUE)
# Then a plot will show up as a new window.
# This isn't required.

# This video on the share drive demonstrates 
# the shiny app:
# https://drive.google.com/file/d/1plL1K70vtVQN_k7w7z8sHrkVz2NwHhNi/view?usp=sharing

# The screenshots below demonstrate the shiny app.
# Note the following about the max Sharpe portfolio:

# If the risk-free rate is zero, then the max Sharpe 
# portfolio is close to the MinVar portfolio, and the 
# weight of the riskier stock is negative.
# eff_front_riskfreezero.jpeg

# If the risk-free rate is large, then the weight 
# of the safer stock is negative.
# eff_front_riskfreelarge.jpeg

# If the correlation is positive and the risk-free 
# rate is small, then the weight of the safer stock 
# is larger than the riskier stock.
# eff_front_corrmod.jpeg

# If the correlation is high and the risk-free 
# rate is small, then the MinVar and max Sharpe 
# portfolio returns are less than the returns of 
# the stocks, and weight of the riskier stock is 
# negative.
# eff_front_corrhigh.jpeg

# If the correlation is negative and the risk-free 
# rate is small, then the max Sharpe portfolio is 
# close to the MinVar portfolio, and its return 
# between the returns of the stocks.
# eff_front_corrneg.jpeg

# If the correlation is very negative, then the 
# volatility of the MinVar portfolio is close to 
# zero, and the efficient frontier is two straight 
# lines.
# eff_front_corrvneg.jpeg

# If the stocks have equal returns, then the 
# efficient frontier becomes a single line.
# eff_front_retequal.jpeg


# You must upload screenshots of the shiny app,
# similar to the ones above.
# You must upload your shiny app to Brightspace.
# You must submit a complete shiny app, so that 
# the user can hit "run" and run it, without any 
# modifications.



