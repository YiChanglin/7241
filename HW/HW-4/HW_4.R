#################################
### FRE7241 Homework #4 due at 6AM Tuesday October 1, 2024
#################################
# Max score 110pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the VTI returns in months 
# with high volatilities and in months with 
# low volatilities.

## Run the setup code below

library(rutils)
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
nrows <- NROW(retp)

## End of setup code


# 1. (20pts) 
# Calculate a vector of monthly end points, 
# using rutils::calc_endpoints().

endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)

# You should get the following outputs:
NROW(endd)
# [1] 280
head(endd)
# [1] 0  21  42  65  80 103
tail(endd)
# [1] 5742 5764 5786 5805 5827 5849


# Perform a loop over the end points and calculate 
# the volatilities of returns in monthly intervals.
# If there are less than 4 days in a given monthly 
# interval, then set the volatility to zero.
# Make sure to calculate the volatilities over 
# non-overlapping monthly intervals.
# You can use the functions sapply(), sd(), and NROW(). 

volv <- sapply(2:npts, function(tday) {
  retis <- retp[(endd[tday-1]+1):endd[tday]]
  if (NROW(retis) > 3)
    return(sd(retis))
  else
    return(0)
})  # end sapply

# You should get the following outputs:
NROW(volv)
# [1] 279
head(volv)
# [1] 0.009162844 0.012093958 0.009591909 0.021846856 0.012257105 0.010195163
tail(volv)
# [1] 0.006204082 0.008535622 0.006089795 0.004154266 0.008847694 0.012563515


# 2. (20pts) 
# Calculate the 80% quantile of the volatilities.
# You can use the function quantile().

quantv <- quantile(volv, 0.8)

# You should get the following output:
quantv
#     80%
# 0.01264623

# Calculate a time series the VTI returns in months 
# with low volatilities.
# The low volatility returns are in the months with 
# volatility below the quantile quantv.
# The high volatility returns are in the months with 
# volatility above the quantile quantv.
# Select the low volatility returns and set the high 
# volatility returns to zero.
# Take note that volv has only 279 elements, while 
# there are 280 npts.  So volv is shifted by 1 element.
# You can use the functions lapply(), do.call(), and 
# rbind(). 

pnls <- lapply(2:npts, function(tday) {
  retis <- retp[(endd[tday-1]+1):endd[tday]]
  if (volv[tday-1] <= quantv)
    return(retis)
  else
    return(0*retis)
})  # end lapply
pnls <- do.call(rbind, pnls)

# You should get the following outputs:
NROW(pnls)
# [1] 5849
head(pnls)
#                     VTI
# 2001-06-01  0.006944472
# 2001-06-04  0.004315932
# 2001-06-05  0.014536383
# 2001-06-06 -0.008525201
# 2001-06-07  0.005123837
# 2001-06-08 -0.008554372
tail(pnls)
#                      VTI
# 2024-08-23  0.0125792382
# 2024-08-26 -0.0025248895
# 2024-08-27  0.0009024783
# 2024-08-28 -0.0061166698
# 2024-08-29  0.0009797341
# 2024-08-30  0.0096012288


# 3. (20pts) 
# Combine the VTI returns with the pnls, and
# calculate their Sharpe and Sortino ratios.

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "LowVol")
sharper <- sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sharper <- round(sharper, 3)


# You should get the following outputs:
sharper
#         VTI   LowVol
# Sharpe  0.447  1.287
# Sortino 0.549  1.616


# Plot the dygraph of the VTI returns with the pnls, 
# with shading.

colnamev <- colnames(wealthv)
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="LowVol Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Create colors for background shading
hivol <- (wealthv$LowVol == 0) # Is it high vol?
indic <- (rutils::diffit(hivol) != 0) # Indices of crosses
crossd <- c(datev[indic], datev[nrows]) # Dates of crosses
shadev <- ifelse(hivol[indic] == 1, "antiquewhite", "lightgreen")

# Plot dygraph with shading
captiont <- paste("Low Volatility Strategy / Sharpe",
                  paste(paste(colnames(sharper), round(sharper[1, ], 3), sep="="), collapse=", "))

# Create dygraph object without plotting it
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot

# Your plot should be similar to volat_low_plot.png


# Perform the Treynor-Mazuy test for market timing skill.
predm <- cbind(VTI=retp, retp^2)
colnames(predm)[2] <- c("Treynor")
regmod <- lm(wealthv$LowVol ~ VTI + Treynor, data=predm); summary(regmod)
# Plot residual scatterplot
resids <- regmod$residuals
plot.default(x=retp, y=resids, xlab="VTI", ylab="Low Volatility")
title(main="Treynor-Mazuy Market Timing Test\n for Low Volatility vs VTI", line=0.5)
# Plot fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["VTI", "Estimate"]*retp
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=retp, y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))


# Your plot should be similar to volat_low_timing.png


# Conclusion:
# The VTI returns in periods of low volatility are 
# higher than in periods of high volatility.
# So knowing future volatility could allow investors 
# to time the market.
# If an investor were able to forecast the spikes in 
# volatility, then they could sell their stocks before 
# periods of high volatility, and they would significantly 
# outperform the stock market.
# But the current example is not a trading strategy 
# because the volatilities are calculated in-sample.
# They are not forecasts.



############## Part I
# Summary: Create a shiny app which backtests 
# a dual EMA moving average crossover strategy.

# 1. (50pts)
# Create a shiny app which backtests (simulates) 
# an EMA moving average crossover strategy with 
# two EMAs.

# Modify the attached file app_dual_crossover_hw.R.
# It contains the necessary code, but it has been 
# corrupted with bugs.
# You need to fix the code to make it work.

# The shiny app should simulate a dual EMA moving 
# average strategy with two EMAs.
# The user should be able to select any of the 
# ETFs in rutils::etfenv$symbolv.
# The user should be able to add annotations 
# for buys and sells.
# The shiny app output should be similar to: 
# shiny_ema_strat1.png
# shiny_ema_strat2.png
# shiny_ema_strat3.png
# shiny_ema_strat4.png

# Rename the corrected shiny app to: 
#   your_name_app_ema_strat.R
# You must upload your shiny app to Brightspace.
# You must submit a complete shiny app, so that the 
# user can hit "run" and run it, without any 
# modifications.

# Hint: 
# Read the slide Dual VWAP Crossover Strategy 
# and the slide MA Crossover Strategy With Lag.
# Hint: 
# Before running the app, delete everything from 
# your workspace, so that the app doesn't use 
# objects from your workspace:
rm(list=ls())


