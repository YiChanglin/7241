#################################
### FRE7241 Homework #3 due 6AM Tuesday September 24 2024
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the optimal weights in the Proportional 
# Wealth Allocations strategy for stocks and bonds.

## Run the setup code below:

# Calculate VTI and IEF percentage returns
pricev <- rutils::etfenv$prices[, c("VTI", "IEF")]
pricev <- na.omit(pricev)
nrows <- NROW(pricev)
retp <- rutils::diffit(pricev)/rutils::lagit(pricev, lagg=1, pad_zeros=FALSE)
datev <- zoo::index(pricev)
# Weights equal to 50% of wealth each
weightv <- c(0.5, 0.5)

## End of setup code

# 1. (30pts)
# Calculate the wealth of the proportional allocation 
# strategy, with equal wealth allocation to VTI and IEF.

wealthv <- cumprod(1 + retp %*% weightv)
wealthv <- xts::xts(wealthv, zoo::index(pricev))
colnames(wealthv) <- "PWA"

# You should get the following outputs:
dim(wealthv)
# [1] 5563  1
head(wealthv)
#                 PWA
# 2002-07-26 1.000000
# 2002-07-29 1.021892
# 2002-07-30 1.024121
# 2002-07-31 1.030024
# 2002-08-01 1.018947
# 2002-08-02 1.011018
tail(wealthv)
#                 PWA
# 2024-08-23 5.421190
# 2024-08-26 5.411029
# 2024-08-27 5.412363
# 2024-08-28 5.394475
# 2024-08-29 5.392420
# 2024-08-30 5.409021

# Calculate the maximum drawdown of the wealth, 
# and the date of the maximum drawdown.
# You can use the functions cummax(), min(), and 
# which.min().

drawdns <- (wealthv - cummax(wealthv))
maxdd <- min(drawdns)
indexmin <- which.min(drawdns)
datemin <- datev[indexmin]

# You should get the following outputs:
datemin
# [1] "2022-10-14"
maxdd
# -1.075687


# Calculate the maximum drawdown of the log wealth, 
# and the date of the maximum drawdown.
# You can use the functions log(), cummax(), min(), 
# and which.min().

wealthlog <- log(wealthv)
drawdns <- (wealthlog - cummax(wealthlog))
maxdd <- min(drawdns)
indexmin <- which.min(drawdns)
datemin <- datev[indexmin]

# You should get the following outputs:
datemin
# [1] "2009-03-09"
maxdd
# [1] -0.2900086


# Calculate the Calmar ratio as the ratio of the annualized 
# average of the log returns of wealth divided by the maximum 
# drawdown of the log wealth.  
# Hint: Annualize the log returns of wealth by multiplying 
# them by 252.
# Assume that 1 year has 252 business days.
# You can use the functions mean() and rutils::diffit().

# The log returns of wealth.
retw <- rutils::diffit(wealthlog)
# The Calmar ratio.
calmarr <- -252*mean(retw)/maxdd

# You should get the following output:
calmarr
# [1] 0.263676

# Calculate the Sharpe and Sortino ratios.
# Assume a target return of 0%.
# Hint: Annualize by multiplying by sqrt(252).

sharper <- sqrt(252)*c(Sharpe=mean(retw)/sd(retw), Sortino=mean(retw)/sd(retw[retw<0]))

# You should get the following output:
sharper
#   Sharpe  Sortino 
# 0.8411121 1.0756585


# 2. (30pts)
# Calculate the Sharpe, Sortino, and Calmar ratios for 
# a vector of IEF weights.  You can collect all the formulas 
# from p.1 and perform an sapply() loop.
# The IEF and VTI weights must add up to 1.

weightv <- (1:9)/10

ratiov <- sapply(weightv, function(wv) {
  wealthv <- cumprod(1 + retp %*% c(1-wv, wv))
  wealthlog <- log(wealthv)
  wealthlog <- log(wealthv)
  retw <- rutils::diffit(wealthlog)
  maxdd <- min(wealthlog - cummax(wealthlog))
  calmarr <- -252*mean(retw)/maxdd
  sharper <- sqrt(252)*c(Sharpe=mean(retw)/sd(retw), Sortino=mean(retw)/sd(retw[retw<0]))
  c(sharper, Calmar=calmarr)
})  # end sapply
ratiov <- t(ratiov)

ratiov <- cbind(weightv=weightv, ratiov)

# You should get the following output:
ratiov
#      weightv    Sharpe   Sortino    Calmar
# [1,]     0.1 0.5940257 0.7264511 0.1449775
# [2,]     0.2 0.6408844 0.7878391 0.1624060
# [3,]     0.3 0.6964113 0.8640472 0.1856353
# [4,]     0.4 0.7628478 0.9579416 0.2190129
# [5,]     0.5 0.8411121 1.0756585 0.2636760
# [6,]     0.6 0.9245939 1.2104335 0.3081770
# [7,]     0.7 0.9809987 1.3155802 0.2802140
# [8,]     0.8 0.9345910 1.2840865 0.2460907
# [9,]     0.9 0.7436124 1.0694782 0.2002328

# Calculate the IEF weights corresponding to the 
# largest Sharpe, Sortino, and Calmar ratios.
# You can use the functions sapply() and which.max().

apply(ratiov[, -1], 2, function(x) weightv[which.max(x)])

# You should get the following output:
# Sharpe Sortino  Calmar 
#   0.7     0.7     0.6 

# Plot the ratios.
# You must use the function plot.zoo().
# Your plot should be similar to strat_pwa_ratios.png

plot.zoo(ratiov[, -1], xlab="IEF weight", 
         main="Ratios for Proportional Wealth Allocations")



############## Part II
# Summary: Simulate a calendar strategy that holds 
# VTI only over the weekends.

## Run the setup code below

# Calculate the daily open and close prices,
# and the daily close-to-close returns.

library(rutils)
ohlc <- log(rutils::etfenv$VTI)
nrows <- NROW(ohlc)
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)

## End of setup code


# 1. (20pts)
# Calculate the close prices on Fridays (closef) 
# and the open prices on the following business 
# day (openn).
# Note that the business day following some Fridays 
# is not always a Monday.
# Hint: You can use the function weekdays() to 
# determine the day of the week for a given date.
# You can use the functions zoo::index(), 
# weekdays(), which(), NROW(), and min(). 

# Get the weekdays of the dates.
datev <- zoo::index(ohlc)
weekdayv <- weekdays(datev)
endd <- which(weekdayv == "Friday")
endn <- endd + 1
# Handle the case when the next day is the last.
nend <- NROW(endn)
endn[nend] <- min(endn[nend], nrows)
# Get the close prices on Fridays and the open 
# prices on the following business day.
closef <- closep[endd]
openn <- openp[endn]

# You should get the following outputs:
tail(closef)
#             VTI.Close
# 2024-07-26  5.597273
# 2024-08-02  5.571774
# 2024-08-09  5.571850
# 2024-08-16  5.610094
# 2024-08-23  5.626145
# 2024-08-30  5.628987

tail(openn)
#             VTI.Open
# 2024-07-29 5.601122
# 2024-08-05 5.526289
# 2024-08-12 5.574547
# 2024-08-19 5.611740
# 2024-08-26 5.628089
# 2024-08-30 5.623006


# 2. (20pts)
# Calculate the strategy pnls as the difference 
# between the open prices on the following business 
# day (openn), minus the close prices on Fridays 
# (closef). 
# Combine (cbind) the strategy pnls with the daily 
# VTI returns (retp).  Set the NA values to zero.
# Hint: You should apply the function zoo::coredata()
# before subtracting closef from openn.
# You can use the functions zoo::coredata(), 
# xts::xts(), zoo::index(), cbind(), colnames(), 
# is.na(), cumsum(),and dygraphs::dygraph().

pnls <- zoo::coredata(openn) - zoo::coredata(closef)
pnls <- xts::xts(pnls, zoo::index(openn))
wealthv <- cbind(retp, pnls)
wealthv[is.na(wealthv)] <- 0
colnames(wealthv) <- c("VTI", "weekend")

# You should get the following outputs:
head(wealthv)
#               VTI           weekend
# 2001-05-31  0.000000000 0.000000000
# 2001-06-01  0.006944472 0.000000000
# 2001-06-04  0.004315932 0.004315932
# 2001-06-05  0.014536383 0.000000000
# 2001-06-06 -0.008525201 0.000000000
# 2001-06-07  0.005123837 0.000000000

tail(wealthv)
#              VTI              weekend
# 2024-08-23  0.0125792382  0.000000000
# 2024-08-26 -0.0025248895  0.001943425
# 2024-08-27  0.0009024783  0.000000000
# 2024-08-28 -0.0061166698  0.000000000
# 2024-08-29  0.0009797341  0.000000000
# 2024-08-30  0.0096012288 -0.005980922

# Plot the cumulative wealths.
# Your plot should be similar to seasonal_weekend.png

endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Weekend Strategy") %>%
  dySeries(name="VTI", strokeWidth=2, col="blue") %>%
  dySeries(name="weekend", strokeWidth=2, col="red") %>%
  dyLegend(width=300)

