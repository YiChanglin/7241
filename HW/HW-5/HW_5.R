#################################
### FRE7241 Homework #5 Solution Tuesday, October 8, 2024
#################################
# Max score 100pts

# The below solutions are examples.
# Slightly different solutions are also possible.


############## Part I
# Summary: Simulate a moving average strategy using 
# fast and slow trailing volatilities.

## Run the setup code below

library(rutils)
retp <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retp)
nrows <- NROW(retp)
lambdaf <- 0.8 # Fast lambda
lambdas <- 0.9 # Slow lambda

## End of setup code


# 1. (20pts) 
# Calculate the fast (volf) and slow (vols) trailing 
# volatilities of retp using lambdaf and lambdas.
# You can use the functions HighFreq::run_var()
# and sqrt(). 

volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])

# You should get the following outputs:
tail(volf)
# [1] 0.008446264 0.007914340 0.007109600 0.007048352 0.006305143 0.006469357
tail(vols)
# [1] 0.010431438 0.010013739 0.009508358 0.009323845 0.008845983 0.008717677


# Create a function called sim_volma(), which simulates 
# a moving average strategy using fast and slow trailing 
# volatilities.
# 
# The trading strategy is as follows:
# Buy one share of VTI if the fast volatility is below 
# the slow volatility.
# Sell one share of VTI short if the fast volatility is 
# above the slow volatility.
# 
# The function sim_volma() should return two columns:
# the PnLs and the positions of the strategy.
# 
# Adapt code from the slide: 
#   Simulation Function for the Dual EMA Crossover Strategy

sim_volma <- function(lambdaf=0.8, lambdas=0.9, bidask=0.0) {
  if (lambdaf >= lambdas) return(NA)
  # Calculate the volatilities
  volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
  vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])
  # Calculate the positions, either: -1, 0, or 1
  posv <- sign(vols - volf)
  # Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  # Calculate the PnLs of strategy
  pnls <- retp*posv
  costs <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  datav <- cbind(pnls, posv)
  colnames(datav) <- c("PnL", "Position")
  datav
}  # end sim_volma


# You should get the following output:

pnls <- sim_volma(lambdaf=lambdaf, lambdas=lambdas)
head(pnls)
#                     PnL Position
# 2001-06-01  0.000000000        0
# 2001-06-04  0.000000000        0
# 2001-06-05  0.014536383        1
# 2001-06-06  0.008525201       -1
# 2001-06-07 -0.005123837       -1
# 2001-06-08  0.008554372       -1

tail(pnls)
#                      PnL Position
# 2024-08-23  0.0125792382        1
# 2024-08-26 -0.0025248895        1
# 2024-08-27  0.0009024783        1
# 2024-08-28 -0.0061166698        1
# 2024-08-29  0.0009797341        1
# 2024-08-30  0.0096012288        1


# 2. (20pts) 
# Create a function called calc_sharpe(), which calls 
# sim_volma() to simulate a moving average strategy 
# using trailing volatilities, and calculates its 
# Sharpe ratio.
# Adapt code from the slide: 
#   Dual EMA Strategy Performance Matrix

# Calculate the Sharpe ratio of moving average strategy 
# using trailing volatilities.

calc_sharpe <- function(lambdaf, lambdas, bidask=0.0) {
  if (lambdaf >= lambdas) return(NA)
  pnls <- sim_volma(lambdaf=lambdaf, lambdas=lambdas, bidask=bidask)[, "PnL"]
  sqrt(252)*mean(pnls)/sd(pnls)
}  # end calc_sharpe

# You should get the following output:
sharper <- calc_sharpe(lambdaf=lambdaf, lambdas=lambdas)
sharper
# [1] 0.5762201


# Create two vectors of lambdas (run this):
lambdafv <- seq(from=0.98, to=0.999, by=0.001)
lambdasv <- seq(from=0.98, to=0.999, by=0.001)

# Calculate a matrix of Sharpe ratios for the vectors 
# lambdafv and lambdasv.
# You can perform two sapply() loops over the lambda 
# vectors.

sharpem <- sapply(lambdasv, function(lambdas) {
  sapply(lambdafv, function(lambdaf) {
    calc_sharpe(lambdaf=lambdaf, lambdas=lambdas)
  })  # end sapply
})  # end sapply
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv


# You should get the following output:
round(sharpem, 2)
#       0.98 0.981 0.982 0.983 0.984 0.985 0.986 0.987 0.988 0.989 0.99 0.991 0.992 0.993 0.994 0.995 0.996 0.997 0.998 0.999
# 0.98    NA  0.46  0.46  0.49  0.52  0.52  0.57  0.50  0.51  0.51 0.50  0.39  0.46  0.50  0.43  0.43  0.41  0.39  0.41  0.29
# 0.981   NA    NA  0.48  0.52  0.54  0.57  0.53  0.52  0.50  0.50 0.41  0.45  0.46  0.51  0.45  0.45  0.42  0.44  0.40  0.31
# 0.982   NA    NA    NA  0.53  0.58  0.55  0.54  0.52  0.53  0.48 0.41  0.46  0.48  0.45  0.48  0.46  0.40  0.36  0.33  0.31
# 0.983   NA    NA    NA    NA  0.54  0.53  0.52  0.56  0.51  0.42 0.43  0.45  0.45  0.44  0.48  0.43  0.39  0.40  0.38  0.31
# 0.984   NA    NA    NA    NA    NA  0.53  0.58  0.49  0.46  0.37 0.44  0.46  0.47  0.45  0.45  0.41  0.42  0.45  0.37  0.30
# 0.985   NA    NA    NA    NA    NA    NA  0.50  0.46  0.41  0.42 0.46  0.46  0.45  0.44  0.43  0.41  0.40  0.45  0.41  0.26
# 0.986   NA    NA    NA    NA    NA    NA    NA  0.43  0.39  0.44 0.43  0.42  0.43  0.49  0.40  0.40  0.42  0.46  0.37  0.26
# 0.987   NA    NA    NA    NA    NA    NA    NA    NA  0.43  0.46 0.44  0.45  0.46  0.44  0.38  0.38  0.45  0.39  0.32  0.22
# 0.988   NA    NA    NA    NA    NA    NA    NA    NA    NA  0.46 0.42  0.45  0.48  0.38  0.41  0.37  0.40  0.36  0.32  0.26
# 0.989   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA 0.46  0.48  0.37  0.34  0.33  0.38  0.34  0.33  0.31  0.28
# 0.99    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA  0.40  0.35  0.36  0.38  0.37  0.28  0.32  0.25  0.24
# 0.991   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA  0.36  0.36  0.37  0.32  0.24  0.30  0.21  0.25
# 0.992   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA  0.36  0.33  0.24  0.29  0.32  0.21  0.18
# 0.993   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA  0.26  0.26  0.27  0.30  0.22  0.21
# 0.994   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA  0.24  0.31  0.21  0.21  0.17
# 0.995   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA  0.25  0.23  0.24  0.19
# 0.996   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA  0.23  0.26  0.20
# 0.997   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA  0.17  0.11
# 0.998   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA    NA  0.13
# 0.999   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA


# 3. (20pts) 
# Calculate the PnLs for the optimal strategy.
# Adapt code from the slide: 
#   Optimal Dual EMA Strategy

whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdaf <- lambdafv[whichv[1]]
lambdas <- lambdasv[whichv[2]]
stratopt <- sim_volma(lambdaf=lambdaf, lambdas=lambdas)
pnls <- stratopt[, "PnL"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "MA Strat"
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Annualized Sharpe ratio of Dual EMA strategy
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
sharper <- round(sharper, 3)
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]

# You should get the following outputs:
lambdaf
# [1] 0.982
lambdas
# [1] 0.984
sharper
#     VTI   MA Strat 
#   0.447     0.583 


# Plot a dygraph of the optimal strategy with shading.
# Don't use end dates.
# Your plot should be similar to ewvol_optim.png

# Create colors for background shading
posv <- stratopt[, "Position"]
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot Optimal Dual EMA strategy
captiont <- paste("Optimal MA Volatility Strategy, Sharpe", "\n",
                  paste(paste(names(sharper), sharper, sep="="), collapse=", "))
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=captiont) %>% 
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  # end for
# Plot the dygraph object
dyplot


# Plot a dygraph of the optimal strategy combined with VTI.
# Your plot should be similar to ewvol_combo.png

wealthv <- cbind(wealthv, (retp+pnls)/2)
colnames(wealthv)[3] <- "Combined"
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
sharper <- round(sharper, 3)
captiont <- paste("MA Volatility Strategy With VTI, Sharpe", "\n",
                  paste(paste(names(sharper), sharper, sep="="), collapse=", "))
# Plot dygraph of stock index and momentum strategy
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300) %>%
  dyCSS("/Users/jerzy/Develop/lecture_slides/css/dygraph.css")

# Code in the file dygraph.css
# .dygraph-title {
#   font-weight: bold;
#   font-size: 16px;
# }



############## Part II
# Summary: Calculate the risk and return characteristics 
# of S&P500 stock returns.

## Run the setup code below

library(rutils)

# Delete all objects from your workspace
rm(list = ls())

# Load the S&P500 stock returns (download from the share drive)
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

# You should get the following output:
ls()
# [1] "retstock"    "retstock100"
dim(retstock)
# [1] 8733  702

# The variable retstock is a time series of daily returns for 702 stocks.

## End of setup code


# 1. (20pts)
# Calculate the number of NA values in the columns 
# of retstock.
# You can use the functions sapply(), sum(),
# and is.na().

nav <- sapply(retstock, function(x) sum(is.na(x)))

# You should get the following outputs:
head(nav)
# BIG  TMO  RIG  AYI  DNB   ED 
#   1    1  863 3009 7685    1 
tail(nav)
# NFLX LRCX   EA  MOS  MPC POOL 
# 3127   59   59    1 5415 1464 
sum(nav)
# [1] 1535170


# Calculate a matrix with the volatilities, Sharpe 
# ratios, and the Kelly ratios of all the columns 
# of retstock.
# 
# For simplicity, assume the risk-free rate is zero,
# so the Sharpe ratio is just the ratio of mean(x)/sd(x).
# And the Kelly ratio is mean(x)/var(x).
# 
# Your code should handle the case when a column
# of retstock has only NA values in it.
# 
# You can use the functions lapply(), na.omit(), 
# mean(), var(), sqrt(), c(), NROW(), rbind(), 
# do.call(),and an anonymous function. 

ratiom <- lapply(retstock, function(retp) {
  retp <- na.omit(retp)
  if (NROW(retp) > 0) {
    meanv <- mean(retp)
    varv <- var(retp)
    stdev <- sqrt(varv)
    return(c(stdev=stdev, sharpe=meanv/stdev, kelly=meanv/varv))
  }  # end if
}) # end sapply
ratiom <- do.call(rbind, ratiom)


# You should get the following outputs:
dim(ratiom)
# [1] 701   3
head(ratiom)
#         stdev       sharpe       kelly
# BIG 0.03396881 -0.003921900 -0.11545590
# TMO 0.01820502  0.027791167  1.52656657
# RIG 0.03489214 -0.001964942 -0.05631473
# AYI 0.02481215  0.022317790  0.89947035
# DNB 0.02181542 -0.031143867 -1.42760797
# ED  0.01223773  0.024300164  1.98567631
tail(ratiom)
#           stdev      sharpe     kelly
# NFLX 0.03543849 0.032081974 0.9052862
# LRCX 0.03480358 0.021074184 0.6055178
# EA   0.02937595 0.022281420 0.7584919
# MOS  0.02771435 0.003205996 0.1156800
# MPC  0.02525077 0.030964554 1.2262814
# POOL 0.02245752 0.038230917 1.7023661


# Sort all the columns of the matrix ratiom 
# by the column stdev, in descending order.
# You can use the function order().

ratiom <- ratiom[order(ratiom[, 1], decreasing=TRUE), ]

# You should get the following outputs:
head(ratiom)
#         stdev       sharpe       kelly
# STI   0.16546962 -0.109898625 -0.66416193
# SIVB  0.12843262 -0.004620433 -0.03597554
# EP    0.12008750  0.005511561  0.04589620
# SBNY  0.11602361 -0.003771113 -0.03250298
# CPWR  0.10351396 -0.006811119 -0.06579904
# SHLDQ 0.09247746 -0.010364230 -0.11207304
tail(ratiom)
#           stdev      sharpe     kelly
# GIS  0.012622691 0.02992060  2.370382
# WEC  0.012279908 0.03277254  2.668793
# ED   0.012237727 0.02430016  1.985676
# JAVA 0.009568795 0.03951117  4.129169
# EMC  0.009493536 0.02397525  2.525429
# CA   0.002180922 0.04581818 21.008625


# 2. (20pts)
# Perform a linear regression of the columns 
# sharpe versus stdev.
# Exclude the stocks with volatilities greater 
# than 0.05.
# Hint: Convert ratiom to a data frame, using
# the function as.data.frame().
# Pass the data frame through the argument data=.
# 
# Plot a scatter plot the columns sharpe versus 
# stdev.
# Add the regression line.
# Hint: Adapt the code from the slides 39 and 40
# in lecture #4.
#
# You can use the functions as.data.frame(),
# lm(), plot(), abline(), and text().

# Your plot should be similar to scatter_stocks_sharpe_stdev.png

# Comment: The beta coefficient is negative, which shows 
# that stocks with higher volatility tend to have lower 
# Sharpe ratios.

lovol <- (ratiom[, "stdev"] < 0.05)
ratiodf <- as.data.frame(ratiom[lovol, ])
regmod <- lm(sharpe ~ stdev, data=ratiodf)
plot(sharpe ~ stdev, data=ratiodf,
     xlab="Standard Deviation", ylab="Sharpe Ratio", 
     main="Sharpe Ratio vs. Standard Deviation")
abline(regmod, lwd=3, col="red")
text(x=mean(ratiodf$stdev),  y=quantile(ratiodf$sharpe, 0.999),
     labels=paste("beta=", round(regmod$coefficients[2], 3)), 
     font=2, col="blue")

