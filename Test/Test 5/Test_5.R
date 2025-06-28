#################################
### FRE7241 Test #5 Solutions Tuesday October 22, 2024
#################################
# Max score 150pts

# The below solutions are examples.
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the 200 most liquid stocks 
# in every year.

## Run the setup code below

library(rutils)
# Load the environment sp500env, with daily OHLC prices
# for S&P500 stocks.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

# Calculate the daily trading volume for MSFT.

volumv <- quantmod::Vo(sp500env$MSFT)

## End of setup code


# 1. (20pts)
# Calculate a time series of the daily trading volumes
# for all the stocks in the environment sp500env.
# You can use the functions eapply(), quantmod::Vo(), 
# do.call(), cbind(), and rutils::get_name().

volumv <- eapply(sp500env, quantmod::Vo)
volumv <- do.call(cbind, volumv)
colnames(volumv) <- rutils::get_name(colnames(volumv))

# You should get the following outputs:
class(volumv)
# "xts" "zoo"
dim(volumv)
# [1] 8733  702
tail(volumv)[, 1:7]
#                BIG     TMO      RIG    AYI     DNB      ED     EG
# 2024-08-23 1029508  777265 16496237 125846 4499075 1620024 143220
# 2024-08-26  466173  519485 14735399 217507 1838979 1221535 372071
# 2024-08-27 1671213  737395 12139850 273398 2029337  927271 357469
# 2024-08-28 2040161  790456 17161779 129084 1616645 1019536 287788
# 2024-08-29 9291069  722498 14617259 139857 1403436 1150898 296913
# 2024-08-30 4438224 1058979 14876698 252563 1353864 2366505 377360


# Calculate the total trading volumes for all the stocks.
# Sort the trading volumes.
# You can use the functions sapply(), sum() with na.rm=TRUE, 
# and sort() with decreasing=TRUE.

volumt <- sapply(volumv, sum, na.rm=TRUE)
volumt <- sort(volumt, decreasing=TRUE)

# You should get the following outputs:
head(volumt)
#       NVDA         AAPL        GOOGL         AMZN          BAC         MSFT 
# 3.892702e+12 3.129920e+12 1.083396e+12 9.440069e+11 4.996690e+11 4.806987e+11 
tail(volumt)
#      SII     JAVA       EP      EMC      WRK       CA 
# 94301868 51219542 40661722 15190910 11862000   598220


# 2. (30pts)
# Create a vector of strings with the years since 1999.
# You can use the function as.character().

yearv <- as.character(1999:2024)

# You should get the following output:
yearv
#  [1] "1999" "2000" "2001" "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009"
# [12] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019" "2020"
# [23] "2021" "2022" "2023" "2024"

# Calculate the symbols of the 200 most liquid stocks 
# in every year, since 1999.
# Hint: Perform two nested sapply() loops.
# You can use the functions sapply(), sum() with na.rm=TRUE, 
# sort() with decreasing=TRUE, names(), and head().

volumy <- sapply(yearv, function(yearn) {
  volumy <- sapply(volumv[yearn], sum, na.rm=TRUE)
  names(head(sort(volumy, decreasing=TRUE), 200))
}) # end sapply

# You should get the following outputs:
dim(volumy)
# [1] 200  26
head(volumy[, 1:3])
#       1999   2000   2001  
# [1,] "AAPL" "AAPL" "NVDA"
# [2,] "AMZN" "NVDA" "AAPL"
# [3,] "NVDA" "AMZN" "AMZN"
# [4,] "EBAY" "MSFT" "CSCO"
# [5,] "ORCL" "ORCL" "MSFT"
# [6,] "MSFT" "INTC" "EBAY"


# 3. (20pts)
# Calculate the unique symbols of the stocks that 
# were among the 200 most liquid stocks at any time.
# Hint: Use the function c() to combine the symbols 
# in volumy into a vector.
# You can use the functions c(), unique(), and 
# sort() with decreasing=TRUE.

symbolv <- sort(unique(c(volumy)))

# You should get the following outputs:
NROW(symbolv)
# [1] 429
head(symbolv)
# [1] "A"    "AA"   "AABA" "AAL"  "AAPL" "ABBV"
tail(symbolv)
# [1] "XOM"  "XRX"  "XTO"  "YUM"  "ZION" "ZTS" 


# Calculate the total trading volumes for all 
# the top symbols in symbolv, and sort them.
# You can use the function sort() with decreasing=TRUE.

volumtop <- sort(volumt[symbolv], decreasing=TRUE)

# You should get the following outputs:
head(volumtop)
#       NVDA         AAPL        GOOGL         AMZN          BAC         MSFT 
# 3.892702e+12 3.129920e+12 1.083396e+12 9.440069e+11 4.996690e+11 4.806987e+11
tail(volumtop)
#       ADT       CTVA       KODK          S        PBG        DNB 
# 5280831696 5233139460 4749719421 3992806615 3766442925 2479823348


# 4. (20pts)
# Copy the OHLC prices for all the top symbols 
# in symbolv into an environment called sp500top,
# and save it to a file called sp500top.RData.
# Hint: Convert the environment sp500env into a list,
# extract the prices, and then convert the list back
# into an environment.
# You can use the functions as.list(), as.environment(),
# and save().

sp500top <- as.list(sp500env)[symbolv]
sp500top <- as.environment(sp500top)
save(sp500top, file="/Users/jerzy/Develop/lecture_slides/data/sp500top.RData")



############## Part II
# Summary: Calculate the rescaled range of daily 
# stock prices.

## Run the setup code below

library(rutils)
# Load the environment sp500env, with daily OHLC prices
# for S&P500 stocks.
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

# Calculate the daily OHLC prices for MSFT.

ohlc <- log(sp500env$MSFT)
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retd <- (closep - openp)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
hilo <- highp - lowp

## End of setup code


# 1. (20pts)
# Calculate the rescaled range of daily prices for MSFT,
# equal to the ratio of the average of the daily range 
# of high minus low prices, divided by the volatility 
# of its daily returns.
# (This definition of the rescaled range is similar to 
# the Hurst exponent, but it's not the same.)

# You should get the following output:
mean(hilo)/sd(retd)
# [1] 1.433769

# The theoretical range for Brownian motion is:
sqrt(pi/2)
# [1] 1.253314

# Comments.
# The rescaled range for MSFT is larger than the 
# theoretical range for Brownian motion, indicating
# that intraday MSFT prices don't follow Brownian 
# motion exactly.
# 
# The large value of the rescaled range for MSFT 
# indicates that intraday MSFT prices tend to revert 
# to the open price, compared to a Brownian motion,
# which doesn't revert.


# 2. (20pts)
# Calculate the volatility of returns and the 
# rescaled range for all the stocks in the 
# environment sp500env.
# Ignore stocks with less than 200 daily prices.
# You can use the functions eapply(), NROW(), 
# do.call(), and rbind().

rangev <- eapply(sp500env, function(ohlc) {
  if (NROW(ohlc) < 200) return(NULL)
  ohlc <- log(ohlc)
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retd <- (closep - openp)
  highp <- quantmod::Hi(ohlc)
  lowp <- quantmod::Lo(ohlc)
  hilo <- highp - lowp
  c(sd(retd), mean(hilo)/sd(retd))
}) # end eapply
rangev <- do.call(rbind, rangev)
colnames(rangev) <- c("volatility", "ranger")

# You should get the following outputs:
class(rangev)
# "matrix" "array" 
dim(rangev)
# [1] 699   2
tail(rangev)
#      volatility   ranger
# NFLX 0.02778684 1.407479
# LRCX 0.03068244 1.355522
# EA   0.02664877 1.378508
# MOS  0.02388002 1.356699
# MPC  0.02110485 1.482273
# POOL 0.02056985 1.384219

# Select the stock with the smallest volatility.
# Hint: Use drop=FALSE to keep the row name.
rangev[which.min(rangev[, "volatility"]), , drop=FALSE]
# You should get the following output:
#      volatility   ranger
# EMC 0.006218747 1.329077

# Select the stock with the largest rescaled range.
rangev[which.max(rangev[, "ranger"]), , drop=FALSE]
# You should get the following output:
#      volatility   ranger
# EMBC 0.02856944 1.660571

# Calculate the percentage of stocks with a rescaled
# range greater than the theoretical range for Brownian
# Motion = sqrt(pi/2).

sum(rangev[, "ranger"] > sqrt(pi/2))/NROW(rangev)
# You should get the following output:
# [1] 0.8812589

# Comment.
# Over 88% of the stocks have a rescaled range greater 
# than the theoretical range of 1.25 for Brownian motion.


# Sort rangev on its second column, the 
# rescaled range.
# You can use the function order().

rangev <- rangev[order(rangev[, 2]), ]

# You should get the following outputs:
head(rangev)
#       volatility    ranger
# SIVB  0.33526860 0.2583539
# SBNY  0.18361772 0.3536513
# EP    0.17221251 0.4230429
# FRCB  0.18094358 0.5018533
# CPWR  0.10912480 0.5907056
# SHLDQ 0.09491277 0.8382747
tail(rangev)
#      volatility   ranger
# KVUE 0.01474945 1.567634
# VREX 0.02107691 1.569656
# AMCR 0.01353688 1.573493
# NE   0.02377795 1.577578
# VLTO 0.01395633 1.596383
# EMBC 0.02856944 1.660571


# 3. (20pts)
# Remove outliers from the data.
# Select the stocks with a rescaled range greater 
# than 1.0, and a volatility less than 0.1.

selv <- (rangev[, "ranger"] > 1.0) & (rangev[, "volatility"] < 0.1)
rangev <- rangev[selv, ]

# You should get the following output:
dim(rangev)
# [1] 692   2

# Plot the rescaled range versus volatility.
# Add a regression line.
# Your plot should be similar to rescaled_range.png
# You can use the functions plot(), abline(), text(),
# cov(), var(), and mean().

betac <- cov(rangev[, "ranger"], rangev[, "volatility"])/var(rangev[, "volatility"])
alphac <- mean(rangev[, "ranger"]) - betac*mean(rangev[, "volatility"])
plot(x=rangev[, "volatility"], y=rangev[, "ranger"], 
     xlab="Volatility", ylab="Rescaled Range", 
     main="Rescaled Range Versus Volatility for S&P500 Stocks")
abline(a=alphac, b=betac, col="red", lwd=3)
text(x=quantile(rangev[, "volatility"], 0.99), 
     y=quantile(rangev[, "ranger"], 0.99), 
     labels=paste0("beta = ", round(betac, 2)), 
     lwd=4, col="blue")
abline(h=sqrt(pi/2), col="darkgreen", lwd=3)
text(x=quantile(rangev[, "volatility"], 0.005), 
     y=(sqrt(pi/2)+0.05), 
     labels="Brownian range", lwd=4, col="darkgreen")

# Comments on the plot.
# Over 88% of the stocks have a rescaled range greater 
# than the theoretical range of 1.25 for Brownian motion.
# (The horizontal green line.)
# 
# The negative slope of the regression line indicates 
# that the rescaled range is inversely related to the
# stock volatility.
# More volatile stocks tend to have smaller rescaled
# ranges than less volatile stocks.

