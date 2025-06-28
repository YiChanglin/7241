#################################
### FRE7241 Test #4 Solutions Tuesday October 8, 2024
#################################
# Max score 50pts

# The below solutions are examples.
# Slightly different solutions are also possible.


############## Part I
# Calculate the empirical volatilities of the 
# AR coefficients over time.

## Run the setup code below

library(rutils)
rm(list=ls())  # Delete the workspace
retp <- na.omit(rutils::etfenv$returns$VTI)
nrows <- NROW(retp)

## End of setup code


# 1. (10pts)
# Define the AR response and predictor matrices 
# for retp and orderp = 5.
# Hint: Copy the formulas from the lecture slides.

orderp <- 5
respv <- retp
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
colnames(predm) <- c("phi0", paste0("lag", 1:orderp))

# You should get the following outputs:
round(head(predm), 4)
#       phi0    lag1    lag2   lag3   lag4   lag5
# 2001-06-01    1  0.0000  0.0000 0.0000 0.0000 0.0000
# 2001-06-04    1  0.0069  0.0000 0.0000 0.0000 0.0000
# 2001-06-05    1  0.0043  0.0069 0.0000 0.0000 0.0000
# 2001-06-06    1  0.0145  0.0043 0.0069 0.0000 0.0000
# 2001-06-07    1 -0.0085  0.0145 0.0043 0.0069 0.0000
# 2001-06-08    1  0.0051 -0.0085 0.0145 0.0043 0.0069
round(tail(predm), 4)
#            phi0    lag1    lag2    lag3    lag4    lag5
# 2024-08-23    1 -0.0085  0.0050 -0.0024  0.0094  0.0024
# 2024-08-26    1  0.0126 -0.0085  0.0050 -0.0024  0.0094
# 2024-08-27    1 -0.0025  0.0126 -0.0085  0.0050 -0.0024
# 2024-08-28    1  0.0009 -0.0025  0.0126 -0.0085  0.0050
# 2024-08-29    1 -0.0061  0.0009 -0.0025  0.0126 -0.0085
# 2024-08-30    1  0.0010 -0.0061  0.0009 -0.0025  0.0126


# Calculate the AR coefficients.
# Hint: Copy the formulas from the lecture slides.

predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv

# You should get the following output:
drop(coeff)
# [1]  0.0003836185 -0.0818519029 -0.0051129887  0.0151378947 -0.0329724582 -0.0200119933

# Calculate the theoretical volatilities of 
# the AR coefficients. 

# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Calculate the variance of the residuals
resids <- (fcasts - respv)
varv <- sum(resids^2)/(nrows-NROW(coeff))
# Calculate the predictor matrix squared
pred2 <- crossprod(predm)
# Calculate the covariance matrix of the AR coefficients
covmat <- varv*MASS::ginv(pred2)
coefsd <- sqrt(diag(covmat))

# You should get the following output:
coefsd
# [1] 0.0001583056 0.0130802640 0.0131169487 0.0131159318 0.0131172591 0.0130806401


# 2. (20pts)
# Calculate the endpoints at the end of each year.
# You can use the function rutils::calc_endpoints().

endy <- rutils::calc_endpoints(retp, interval="years")

# You should get the following outputs:
head(endy)
# [1]    0  144  396  648  900 1152
tail(endy)
# [1] 4675 4928 5180 5431 5681 5849


# Perform an saapply() loop over the endpoints, and 
# calculate the annual AR coefficients over non-overlapping 
# 1-year intervals, starting from the first day of each 
# year, until the last day of each year.
# Hint: You can subset the matrix predm and the vector respv.
# You can use the functions sapply() and MASS::ginv().

coefm <- sapply(2:NROW(endy), function(x) {
  datev <- ((endy[x-1]+1):endy[x])
  predinv <- MASS::ginv(predm[datev, ])
  predinv %*% respv[datev, ]
}) # end sapply

# You should get the following outputs:
dim(coefm)
# [1]  6 24
round(t(coefm), 4)
#         [,1]    [,2]    [,3]    [,4]    [,5]    [,6]
# [1,] -0.0005  0.1252 -0.0587  0.1173  0.0118 -0.1587
# [2,] -0.0011 -0.0398 -0.0194 -0.0358 -0.0588 -0.0133
# [3,]  0.0011 -0.0777  0.0255  0.0074  0.0608 -0.0883
# [4,]  0.0005  0.0721 -0.0667  0.0231 -0.0612  0.0431
# [5,]  0.0003 -0.0574  0.0077 -0.0575 -0.0179  0.0006
# [6,]  0.0006  0.0524 -0.1776  0.0264  0.0189 -0.0216
# [7,]  0.0003 -0.1324 -0.0086  0.0476 -0.0880 -0.0701
# [8,] -0.0026 -0.1170 -0.2153  0.0959 -0.1308 -0.0245
# [9,]  0.0009 -0.0725  0.0121  0.0018  0.0934  0.0358
# [10,]  0.0007 -0.0166 -0.0223  0.0033 -0.0068 -0.0217
# [11,]  0.0001 -0.0479  0.0575 -0.1304  0.0486 -0.1708
# [12,]  0.0008  0.0147 -0.0081 -0.0843 -0.1124 -0.0873
# [13,]  0.0014 -0.0624 -0.0276 -0.0332 -0.0307 -0.1088
# [14,]  0.0005  0.0117  0.0349  0.0277 -0.0337 -0.0078
# [15,]  0.0000  0.0499 -0.1194 -0.0226 -0.1723 -0.0146
# [16,]  0.0005 -0.0582  0.0343  0.0140  0.0266 -0.0117
# [17,]  0.0011 -0.1459 -0.0989 -0.0762 -0.0839  0.0072
# [18,] -0.0002  0.0164 -0.0413  0.0933 -0.0484 -0.0175
# [19,]  0.0014 -0.0776 -0.0781  0.0002 -0.0208 -0.0968
# [20,]  0.0008 -0.2402  0.2736  0.0731 -0.1539  0.0612
# [21,]  0.0012 -0.0734 -0.0588 -0.0623 -0.0379 -0.0795
# [22,] -0.0010  0.0142 -0.0513 -0.0413 -0.0194  0.0016
# [23,]  0.0010  0.0371 -0.0314 -0.0117 -0.0129 -0.0047
# [24,]  0.0011  0.0447 -0.0855 -0.0731  0.1179 -0.1006


# Calculate the empirical (measured) volatilities 
# of the annual AR coefficients over time.
# You can use the functions apply() and sd().

coefsd2 <- apply(coefm, 1, sd)

# You should get the following output:
coefsd2
# [1] 0.0009239153 0.0814454459 0.0916984400 0.0619821225 0.0725505334 0.0605061124

# Note that the empirical volatilities are much larger 
# than the theoretical ones, because the theoretical
# are calculated for daily returns, while the empirical
# are for annual time intervals.



############## Part II
# Summary: Calculate the Dowd ratios for all 
# the columns of rutils::etfenv$returns.

library(rutils)

# 1. (20pts)
# Perform an sapply() loop over the columns of 
# etfenv$returns, and calculate a matrix of the 
# two Dowd ratios. 
# Assume that the risk-free rate is equal to zero.
# Use the 2% confidence level.
# Multiply the ratios by sqrt(252) to obtain an 
# annualized number.
# Sort the matrix on the first Dowd column.
# You can use the function sapply(), na.omit(), 
# mean(), quantile(), colnames(), order(), and 
# an anonymous function.
# You cannot use the package PerformanceAnalytics.

confl <- 0.02

dowdm <- sapply(rutils::etfenv$returns, function(retp) {
  retp <- na.omit(retp)
  meanr <- mean(retp)
  varisk <- quantile(retp, probs=confl)
  cvar <- mean(retp[retp < varisk])
  c(-meanr/varisk, -meanr/cvar)
}) # end sapply

dowdm <- t(dowdm)
dowdm <- sqrt(252)*dowdm
colnames(dowdm) <- c("Dowd", "DowdC")
dowdm <- dowdm[order(dowdm[, 1], decreasing=TRUE), ]

# You should get the following output:
dowdm
#               Dowd         DowdC
# USMV  0.416619648  0.255482451
# QUAL  0.337092714  0.220080785
# MTUM  0.294568233  0.203326310
# SPY   0.239228625  0.161886020
# IEF   0.231990531  0.178950203
# VLUE  0.228847825  0.150796165
# XLV   0.216740788  0.148555937
# GLD   0.211201763  0.148822440
# VTV   0.199808089  0.127600152
# VYM   0.198511378  0.124166051
# XLP   0.198275757  0.135971033
# VTI   0.196831527  0.133576592
# AIEQ  0.185780414  0.165920876
# XLY   0.176184668  0.123651464
# XLI   0.175896656  0.123181340
# IWB   0.174210725  0.118541819
# IVW   0.168267608  0.117086094
# IWD   0.168163728  0.109238432
# XLU   0.167799118  0.114910345
# IVE   0.161607196  0.106386754
# IWF   0.154753475  0.106142178
# XLB   0.154052839  0.108612520
# QQQ   0.148138613  0.107709640
# XLK   0.142752701  0.108949417
# EEM   0.140370789  0.089163449
# TLT   0.131844751  0.101570260
# XLE   0.127510331  0.085230072
# VNQ   0.120552090  0.070083484
# SVXY  0.093184335  0.049734123
# XLF   0.090226067  0.055278663
# VEU   0.078161012  0.048039746
# DBC   0.006948184  0.004947637
# USO  -0.132529413 -0.088322914
# VXX  -0.607785499 -0.476117592

