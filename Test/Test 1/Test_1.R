#################################
### FRE7241 Test #1 Solution Tuesday September 17, 2024
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the largest drawdown (drop) of 
# log VTI prices over any 1 year period.  

## Run all the setup code below.

# Load the package rutils.
library(rutils)

# Calculate the log VTI prices.
pricev <- log(quantmod::Cl(rutils::etfenv$VTI))
# Calculate the VTI dates.
datev <- zoo::index(pricev)

## End of setup code.


# 1. (30pts)
# Calculate the VTI prices lagged by 1 year,
# and the price differences over 1 year.
# Assume that 1 year has 252 business days.
# You can use the function rutils::lagit().

yearb <- 252
pricel <- rutils::lagit(pricev, yearb, pad_zeros=FALSE)
priced <- (pricev - pricel)

# You should get the following price 
# differences over 1 year:
head(priced, 3)
#            VTI.Close
# 2001-05-31 0.000000000
# 2001-06-01 0.006944472
# 2001-06-04 0.011260405
tail(priced, 3)
#            VTI.Close
# 2024-08-28 0.2398484
# 2024-08-29 0.2263868
# 2024-08-30 0.2315621


# Calculate the largest drawdown (drop) of log VTI 
# prices over any 1 year period.  
# That means the largest drawdown between any two 
# dates separated by 1 year.
# You can use the function min().

min(priced)

# You should get the following output:
# [1] -0.6426144

# Calculate the start and end dates of the largest 
# drawdown.
# Your code should work even when the start of the
# drawdown is at the beginning of the series.
# You can use the functions which.min() and max().

indexmin <- which.min(priced)
datev[max(indexmin-yearb, 1)]
datev[indexmin]

# You should get the following outputs:
# [1] "2008-03-05"
# [1] "2009-03-05"


# Calculate the largest rally (increase) of log 
# VTI prices over any 1 year period.  
# Calculate the start and end dates of this rally.
# You can use the functions max() and which.max().

max(priced)
indexmax <- which.max(priced)
datev[max(indexmax-yearb, 1)]
datev[indexmax]

# You should get the following outputs:
# [1] 0.6177547
# [1] "2020-03-23"
# [1] "2021-03-23"



############## Part II
# Summary: Remove penny stocks from a time series
# of prices.

## Run the setup code below

# Load the daily S&P500 stock prices.
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")

# Select prices from 2000 to the present.
pricestock <- pricestock["2000/"]

# The time series called pricestock contains penny stocks.
# A penny stock is a stock with a price less than $1.

# Some of these stocks have very low adjusted prices
# in the past because they had many stock splits as
# their prices grew rapidly over 20 years.

## End of setup code


# 1. (20pts)
# Find the names of the stocks with prices that were
# less than $1 at any time.
# You can use the functions lapply(), sum(),
# na.omit(), unlist(), and names().

pricep <- lapply(pricestock, function(pricev) {
  pricev <- na.omit(pricev)
  if (sum(pricev < 1) > 0) TRUE
}) # end lapply
pricep <- unlist(pricep)
namev <- names(pricep)

# You should get the following outputs:
namev
#  [1] "BIG"   "RIG"   "EP"    "DNR"   "VTR"   "KMX"   "FOSL"  "F"     "ROL"   "DVA"   "CTRA" 
# [12] "RRC"   "RRD"   "ANDV"  "GILD"  "CTSH"  "CCI"   "WMB"   "GME"   "ON"    "GLW"   "SBNY" 
# [23] "BTU"   "JCI"   "WYND"  "ATVI"  "GNW"   "NKTR"  "AMZN"  "WRB"   "TSCO"  "HBI"   "TGNA" 
# [34] "QEP"   "AES"   "FITB"  "CPRT"  "FLIR"  "CMG"   "SIVB"  "ENPH"  "LUMN"  "AIV"   "DXCM" 
# [45] "PENN"  "ILMN"  "CPWR"  "SHLDQ" "MNST"  "AMT"   "NVDA"  "CSX"   "STI"   "CTL"   "LVS"  
# [56] "APH"   "RAI"   "ISRG"  "SWN"   "AKAM"  "AAPL"  "AXON"  "HBAN"  "SBAC"  "ODFL"  "FRCB" 
# [67] "BBBY"  "URBN"  "NFLX" 


# 2. (30pts)
# Find the names of the stocks with the last (most recent)
# non-NA prices that are less than $1.
# There are at least two ways of doing it.
# You can use the functions lapply(), xts::last(),
# na.omit(), zoo::na.locf(), unlist(), names(),
# drop(), and coredata().

pricel <- lapply(pricestock, function(pricev) {
  pricel <- xts::last(na.omit(pricev))
  if (pricel < 1) pricel
}) # end lapply
pricel <- unlist(pricel)
namev <- names(pricel)
# Or
pricena <- zoo::na.locf(pricestock, na.rm=FALSE)
pricel <- drop(coredata(xts::last(pricena)))
namev <- names(pricel[pricel < 1])

# You should get the following outputs:
namev
# [1] "BIG"   "DNR"   "SIVB"  "CPWR"  "SHLDQ" "STI"   "FRCB"  "BBBY"


# 3. (20pts)
# Calculate the column numbers in pricestock, corresponding 
# to the vector namev.
# You can use the functions match() and colnames().

colnum <- match(namev, colnames(pricestock))

# You should get the following output:
colnum
# [1] 1  18 386 444 449 510 657 660
colnames(pricestock)[colnum]
# [1] "BIG"   "DNR"   "SIVB"  "CPWR"  "SHLDQ" "STI"   "FRCB"  "BBBY"

# Remove the stocks in namev from the time series
# called pricestock.

pricestock <- pricestock[, -colnum]

# You should get the following output:
dim(pricestock)
# [1] 6205  694



