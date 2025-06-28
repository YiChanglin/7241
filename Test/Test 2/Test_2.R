#################################
### FRE7241 Test #2 Solution Tuesday September 24 2024
#################################
# Max score 100pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Remove NA values from time series in
# an environment.

## Run the setup code below

library(rutils)

# Download from the share drive the binary file rates_data_withNAs.RData:
# https://drive.google.com/drive/folders/1jNtoNCL8mLJyda0wu1pLCnjvqIPzd_WL

# Load the file rates_data_withNAs.RData
# Modify the path to the file rates_data_withNAs.RData as needed.
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data_withNAs.RData")

# The file rates_data_withNAs.RData contains the environment
# ratesenv, with time series with NA values.

## End of setup


# 1. (40pts)
# Calculate the number of NA values in the time series
# in ratesenv.
# You can use the functions sapply() or eapply(), is.xts(),
# sum(), and is.na().

sapply(ratesenv, function(x) sum(is.na(x)))

# You should get the following output:
# DGS10 DGS30  DGS1 DGS20  DGS2  DGS5
# 686     511   686  2375   518   686


# Remove the NA values from the time series in ratesenv.
# You must perform this in two different ways.

# First method:
# You can only use the functions for(), ls(), na.omit(),
# get(), and assign().

for (namev in ls(ratesenv)) {
  assign(x=namev, value=na.omit(get(namev, ratesenv)), envir=ratesenv)
} # end for

# Second method:
# You can only use the functions eapply(), na.omit(), assign(),
# and an anonymous function.
# The anonymous function should return NULL.

eapply(ratesenv, function(x) {
  assign(x=names(x), value=na.omit(x), envir=ratesenv)
  return(NULL)
}) # end eapply


# Calculate again the number of NA values in the time
# series in ratesenv.

# You should get the following output:
# DGS10 DGS30  DGS1 DGS20  DGS2  DGS5
#     0     0     0     0     0     0



############## Part II
# Summary: List the names and dimensions of the xts objects
# in the environment rutils::etfenv.

## Run the setup code below

library(rutils)

## End of setup


# 1. (20pts)
# Calculate the names of all the objects of class xts in
# the environment rutils::etfenv.
# Hint: You can first calculate a named Boolean vector
# that's TRUE for objects of class xts, then extract
# the names of those objects.
# You can use the functions sapply() or eapply(), is.xts(),
# unlist(), and names().

isxts <- sapply(rutils::etfenv, is.xts)
names(isxts[isxts])
# Or
isxts <- unlist(eapply(rutils::etfenv, is.xts))
names(isxts[isxts])

# You should get the following output:
#  [1] "XLP"     "SVXY"    "XLU"     "XLV"     "MTUM"    "IVE"     "XLY"     "VTV"    
#  [9] "QQQ"     "EEM"     "TLT"     "IWB"     "USMV"    "IWD"     "GLD"     "IWF"    
# [17] "IVW"     "VEU"     "VLUE"    "QUAL"    "XLB"     "USO"     "VXX"     "AIEQ"   
# [25] "XLE"     "prices"  "XLF"     "returns" "XLI"     "VYM"     "IEF"     "VNQ"    
# [33] "XLK"     "VTI"     "DBC"     "SPY"  


# 2. (20pts)
# Calculate a vector with the number of NA values in all
# the objects of class xts in the environment rutils::etfenv.
# You can use the functions eapply(), is.xts(), is.na(),
# sum(), and unlist().

unlist(eapply(rutils::etfenv, function(x) {
  if (is.xts(x)) sum(is.na(x))
})) # end eapply

# You should get the following output:
# XLP    SVXY     XLU     XLV    MTUM     IVE     XLY     VTV     QQQ     EEM 
#   0       0       0       0       0       0       0       0       0       0 
# TLT     IWB    USMV     IWD     GLD     IWF     IVW     VEU    VLUE    QUAL 
#   0       0       0       0       0       0       0       0       0       0 
# XLB     USO     VXX    AIEQ     XLE  prices     XLF returns     XLI     VYM 
#   0       0       0       0       0   92569       0   92603       0       0 
# IEF     VNQ     XLK     VTI     DBC     SPY 
#   0       0       0       0       0       0 


# 3. (20pts)
# Calculate the dimensions of all the objects of class
# xts in rutils::etfenv.
# You can use the functions eapply(), dim(), is.xts(),
# rbind(), and do.call().

dimat <- eapply(rutils::etfenv, function(x) if (is.xts(x)) dim(x))
dimat <- do.call(rbind, dimat)


# You should get the following output:
dimat
#         [,1] [,2]
# XLP     6464    5
# SVXY    3248    5
# XLU     6464    5
# XLV     6464    5
# MTUM    2863    5
# IVE     6104    5
# XLY     6464    5
# VTV     5182    5
# QQQ     6412    5
# EEM     5383    5
# TLT     5563    5
# IWB     6109    5
# USMV    3236    5
# IWD     6104    5
# GLD     4979    5
# IWF     6104    5
# IVW     6104    5
# VEU     4402    5
# VLUE    2863    5
# QUAL    2800    5
# XLB     6464    5
# USO     4630    5
# VXX     3923    5
# AIEQ     150    5
# XLE     6464    5
# prices  7954   34
# XLF     6464    5
# returns 7954   34
# XLI     6464    5
# VYM     4476    5
# IEF     5563    5
# VNQ     5015    5
# XLK     6464    5
# VTI     5850    5
# DBC     4674    5
# SPY     7954    5
