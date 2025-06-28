#################################
### FRE7241 Homework #2 due at 6AM Tuesday September 17
#################################
# Max score 120pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the Sharpe and Dowd ratios 
# of VTI for different holding periods.

## Run the setup code below.

library(rutils)
# Calculate VTI percentage returns:
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)
# Define a vector of holding periods in days:
holdv <- c(1, 5, 25, 252)

## End of setup code.


# 1. (30pts)
# Perform two sapply() loops: first over the vector 
# holdv, and the second over (1:nrows).
# For each value of the holding period, calculate the 
# aggregated VTI returns by sampling from retp repeatedly, 
# nrows times. 
# Reset the random number generator inside the loop over 
# holdv, using the function set.seed().
# You don't need to resample in the case of the daily
# holding period.
# 
# Calculate the VaR, CVaR, and the Sharpe and Dowd ratios 
# for the aggregated VTI returns.
# Assume that the risk-free rate is equal to zero.
# Use the 2% confidence level.
# Multiply the ratios to obtain an annualized number.
# Multiply the daily ratios by sqrt(252), the weekly 
# ratios by sqrt(252/5), and the monthly ratios by 
# sqrt(252/25). 
# 
# You can use the functions sapply(), unname(), mean(), 
# sd(), quantile(), colnames(), and an anonymous function.
# You cannot use the package PerformanceAnalytics.


confl <- 0.02

dowdm <- sapply(holdv, function(holdp) {
  set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")  # Reset random number generator
  # Bootstrap VTI returns
  if (holdp > 1) {
    reta <- sapply(1:nrows, function(x) {
      mean(retp[sample.int(nrows, size=holdp, replace=TRUE)])
    })  # end sapply
  } else
    reta <- retp
  # Calculate the Sharpe and Dowd ratios
  stdev <- sd(reta)
  varisk <- unname(quantile(reta, probs=confl))
  cvar <- mean(reta[reta < varisk])
  sqrt(252/holdp)*mean(reta)/c(Sharpe=stdev, Dowd=-varisk, DowdC=-cvar)
}) # end sapply

colnames(dowdm) <- paste0("Hold=", holdv)

# You should get output similar to the following:
dowdm
#           Hold=1    Hold=5   Hold=25  Hold=252
# Sharpe 0.4467064 0.4589181 0.3623143 0.4520676
# Dowd   0.1968315 0.2203972 0.1744123 0.2712138
# DowdC  0.1335766 0.1628974 0.1441639 0.2230073

# This shows that the Dowd ratios increase with longer 
# holding periods, because the VaR and CVaR decrease.
# Stocks become less risky over longer holding periods.
# But the tendency is not perfect because of random
# noise in the simulated returns.

# Sort the columns of dowdm on the Dowd row.
# You can use the function order().

dowdm[, order(dowdm[2, ], decreasing=TRUE)]

# You should get the following output:
#         Hold=252    Hold=5    Hold=1   Hold=25
# Sharpe 0.4520676 0.4589181 0.4467064 0.3623143
# Dowd   0.2712138 0.2203972 0.1968315 0.1744123
# DowdC  0.2230073 0.1628974 0.1335766 0.1441639



############## Part II
# Summary: Find the optimal weights for a portfolio 
# of stocks and bonds, to maximize the risk-adjusted 
# wealth, for different holding periods.

# Simulate the wealth distributions of VTI and IEF
# portfolios for different holding periods.
# Perform bootstrap simulation of VTI and IEF returns, 
# and calculate the distribution of their terminal 
# wealths at maturity.

# Disclaimer: 
# The comments and conclusions below are based on 
# 20 years of very positive stock and bond returns, 
# when stocks and bonds have both been in a secular 
# bull market.
# The conclusions would not hold if stocks and bonds 
# had suffered from a bear market (losses).

## Run all the setup code below

library(rutils)
# Extract the percentage returns for VTI and IEF
retp <- rutils::etfenv$returns[, c("VTI", "IEF")]
retp <- zoo::coredata(na.omit(retp))
nrows <- NROW(retp)

# Bootstrap the retp returns, and calculate a list
# of simulated VTI and IEF returns called bootd.
# Performing the parallel loops may take some time.

library(parallel)  # Load package parallel
ncores <- detectCores() - 1  # Number of cores
nboot <- 1e4
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")

# Run only the parallel code for your computer system.

cluster <- makeCluster(ncores)  # Initialize compute cluster under Windows

# Perform parallel bootstrap under Windows
clusterSetRNGStream(cluster, 1121)  # Reset random number generator in all cores
clusterExport(cluster, c("retp", "nrows"))
bootd <- parLapply(cluster, 1:nboot, function(x) {
  retp[sample.int(nrows, replace=TRUE), ]
})  # end parLapply

# Perform parallel bootstrap under Mac-OSX or Linux
bootd <- mclapply(1:nboot, function(x) {
  retp[sample.int(nrows, replace=TRUE), ]
}, mc.cores=ncores)  # end mclapply

## End of setup code


# 1. (30pts)
# Run the code below.
# Define two holding periods - short and long:
holdv <- floor(nrows*c(0.1, 1.0))
# Define vector of VTI weights
weightv <- seq(0.05, 0.95, 0.05)

# Calculate for each element of the list bootd, the 
# portfolio wealths for the two holding periods in
# holdv and for the vector of VTI weights weightv.
# 
# You should perform a parallel loop over the list 
# bootd. You also need to perform two nested loops 
# inside - one over holdv and another over weightv. 
# Remember that the portfolio weights must be first 
# applied to the returns before they are compounded.

# Perform parallel bootstrap under Windows
wealthm <- parLapply(cluster, bootd, function(retp) {
  sapply(holdv, function(holdp) {
    sapply(weightv, function(weight) {
      prod(1 + retp[1:holdp, ] %*% c(weight, 1-weight))
    })  # end sapply
  })  # end sapply
})  # end parLapply

# Stop R processes over cluster under Windows.
stopCluster(cluster)

# Perform parallel bootstrap under MacOS
wealthm <- mclapply(bootd, function(retp) {
  sapply(holdv, function(holdp) {
    sapply(weightv, function(weight) {
      prod(1 + retp[1:holdp, ] %*% c(weight, 1-weight))
    })  # end sapply
  })  # end sapply
}, mc.cores=ncores)  # end mclapply

# You should get output similar to the following:
NROW(wealthm)
# [1] 10000
class(wealthm)
# "list" 
wealthm[[1]]
# You will likely get slightly different numbers.
#          [,1]     [,2]
# [1,] 1.066465 2.343447
# [2,] 1.090561 2.564210
# [3,] 1.114760 2.797556
# [4,] 1.139043 3.043188
# [5,] 1.163390 3.300670
# [6,] 1.187782 3.569415
# [7,] 1.212199 3.848677
# [8,] 1.236620 4.137554
# [9,] 1.261023 4.434979
# [10,] 1.285387 4.739725
# [11,] 1.309688 5.050403
# [12,] 1.333903 5.365475
# [13,] 1.358009 5.683255
# [14,] 1.381982 6.001924
# [15,] 1.405796 6.319543
# [16,] 1.429427 6.634071
# [17,] 1.452850 6.943383
# [18,] 1.476038 7.245291
# [19,] 1.498966 7.537569

# The first column above are the terminal portfolio 
# wealths for the short holding period = 503 days.
# The second column are the wealths for the long holding 
# period = 5030 days.
# The rows correspond to the vector weightv of VTI weights.


# 2. (20pts)
# Calculate the matrix of terminal portfolio wealths 
# for the short holding period.
# You can use the functions lapply(), do.call(), and rbind().

wealthsh <- lapply(wealthm, function(matv) {
  matv[, 1]
})  # end lapply
wealthsh <- do.call(rbind, wealthsh)

# You should get output similar to the following:
dim(wealthsh)
# [1] 10000    19
class(wealthsh)
# "matrix" "array"
tail(wealthsh[, 1:5])
# You will likely get slightly different numbers.
#               [,1]     [,2]      [,3]      [,4]      [,5]
# [9995,] 1.0574049 1.0679396 1.078251 1.0883321 1.0981741
# [9996,] 1.0732752 1.0897997 1.106359 1.1229457 1.1395552
# [9997,] 0.9563279 0.9860933 1.016525 1.0476283 1.0794073
# [9998,] 1.2645520 1.2447056 1.224738 1.2046648 1.1844991
# [9999,] 1.2344548 1.2451854 1.255627 1.2657705 1.2756073
# [10000,] 0.9693909 0.9608888 0.952188 0.9432942 0.9342134

# The columns of wealthsh are the portfolio wealths for 
# the short holding period corresponding to the different 
# VTI weights.
# The rows correspond to the wealth scenarios of the list 
# bootd.

# Calculate the matrix of terminal portfolio wealths 
# for the long holding period.

wealthl <- lapply(wealthm, function(matv) {
  matv[, 2]
})  # end lapply
wealthl <- do.call(rbind, wealthl)

# You should get output similar to the following:
dim(wealthl)
# [1] 10000    19
class(wealthl)
# "matrix" "array"
tail(wealthl[, 1:5])
# You will likely get slightly different numbers.
#              [,1]     [,2]     [,3]     [,4]     [,5]
# [9995,] 2.213825 2.240890 2.261837 2.276481 2.284691
# [9996,] 2.780135 3.059060 3.356557 3.672682 4.007337
# [9997,] 1.664108 1.797543 1.936475 2.080549 2.229338
# [9998,] 2.040140 2.370794 2.747682 3.175974 3.661205
# [9999,] 2.589740 2.768081 2.950441 3.136025 3.323960
# [10000,] 1.213681 1.398942 1.608098 1.843497 2.107605

# The columns of wealthl are the portfolio wealths for 
# the long holding period corresponding to the different 
# VTI weights.
# The rows correspond to the wealth scenarios of the list 
# bootd.


# 3. (20pts)
# Define the risk-adjusted wealth ratio

riskretfun <- function(wealthv) {
  # Set floor for risk
  riskv <- 0.01
  # Calculate risk only if wealth drops below par (1)
  if (min(wealthv) < 1)
    # Calculate risk as downside deviation
    riskv <- mean((1 - wealthv)[wealthv < 1])
  # Calculate risk-return ratio as wealth divided by downside deviation
  mean(wealthv)/riskv
}  # end riskretfun

# Calculate from wealthsh the vector of wealth ratios 
# for the short holding period.
# You can use the functions apply() and riskretfun().

riskretsh <- apply(wealthsh, 2, riskretfun)

# You should get output similar to the following:
riskretsh
# [1] 21.461067 24.893408 28.060488 29.123313 28.086192 26.483016 24.357160
# [8] 21.831410 19.724217 17.632744 15.737940 14.275370 13.010282 12.126679
# [15] 11.159353 10.446937  9.850391  9.241183  8.789688

# Calculate the VTI weight corresponding to the 
# maximum wealth ratio.
# You should use the function which.max().

maxshort <- weightv[which.max(riskretsh)]

# You should get output similar to the following:
maxshort
# [1] 0.2


# Calculate from wealthl the vector of wealth ratios 
# for the long holding period.

riskretl <- apply(wealthl, 2, riskretfun)

# You should get output similar to the following:
riskretl
# [1]  26.24131  46.58792  97.05854 111.37486 162.31836 221.14533 259.72131
# [8] 252.75984 112.63149  69.21112  51.82173  50.46453  47.83634  52.37428
# [15]  43.54196  37.65812  35.06495  34.19947  36.15627

# Calculate the VTI weight corresponding to the 
# maximum wealth ratio.

maxlong <- weightv[which.max(riskretl)]

# You should get output similar to the following:
maxlong
# [1] 0.35

# This shows that the optimal VTI weight corresponding 
# to the maximum wealth ratio is higher for the longer 
# holding period.  
# This shows that if stocks provide a positive return 
# over the long term, then they grow safer with longer 
# holding period, so their portfolio weight should be higher.
# That was true for U.S. stocks in the last 20 years.
# But it's not true for all stock markets, especially those 
# that have not grown.


# 4. (20pts)
# Plot the stock wealth risk-return ratios riskretsh 
# and riskretl.
# You can use the functions plot(), lines(), range(), 
# and legend().
# Your plot should be similar to wealth_vti_holding_long_short.png

# Plot short holding
plot(x=weightv, y=riskretsh, ylim=range(c(riskretsh, riskretl)),
     main="Stock-Bond Risk-Return Ratio as Function of Stock Weight",
     xlab="VTI Weight", ylab="Risk-return Ratio", 
     t="l", lwd=3, col="green")
# Add long holding
lines(x=weightv, y=riskretl, col="blue", lwd=3)
# Add legend
legend("topright", inset=0.1, cex=1.0, title=NULL,
       leg=c("Short holding", "Long holding"), bty="n", y.intersp=0.5,
       lwd=6, bg="white", col=c("green", "blue"))
ymax <- max(c(riskretsh, riskretl))
abline(v=maxshort, lty="dashed", lwd=1, col="green")
text(x=maxshort, y=0.4*ymax, pos=4, cex=0.9, col="green", 
     labels=paste("optimal VTI weight =", maxshort))
abline(v=maxlong, lty="dashed", lwd=1, col="blue")
text(x=maxlong, y=0.6*ymax, pos=4, cex=0.9, col="blue", 
     labels=paste("optimal VTI weight =", maxlong))

