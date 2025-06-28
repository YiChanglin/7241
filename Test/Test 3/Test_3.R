#################################
### FRE7241 Test #3 Solutions Tuesday October 1, 2024
#################################
# Max score 80pts

# The below solutions are examples,
# Slightly different solutions are also possible.


############## Part I
# Summary: Calculate the first non-NA values in the
# columns of rutils::etfenv$returns.

# Run the following code:
library(rutils)


# 1. (20pts)
# Perform an sapply() loop to calculate a named vector
# containing the number of NA values in the columns of
# rutils::etfenv$returns.
# You can use the functions sapply(), sum(), and is.na().

sapply(rutils::etfenv$returns, function(x) sum(is.na(x)))

# You should get the following output:
#  VEU  GLD  VNQ USMV VLUE SVXY  SPY  IVE  XLB  XLE  XLF  EEM  DBC  IEF 
# 3553 2976 2940 4719 5092 4707    1 1851 1491 1491 1491 2572 3281 2392 
#  XLI  VTI  XLK  TLT  IWB AIEQ MTUM  IWD  XLP  IWF  IVW  VXX  QQQ  XLU 
# 1491 2105 1491 2392 1846 7805 5092 1851 1491 1851 1851 4032 1543 1491 
#  XLV  VTV QUAL  VYM  USO  XLY 
# 1491 2773 5155 3479 3325 1491 


# 2. (20pts)
# Calculate the first non-zero and non-NA value and its
# position (index) in the time series rutils::etfenv$returns$VXX
#
# You can use the functions match() and is.na().
# OR
# You can use the functions min(), which(), and is.na().

boolv <- (!is.na(rutils::etfenv$returns$VXX)) & (rutils::etfenv$returns$VXX != 0)
nonap <- match(TRUE, boolv)
# OR
nonap <- min(which(boolv))

nonav <- rutils::etfenv$returns$VXX[nonap]

# You should get the following output:
nonap
# [1] 4033
nonav
#                    VXX
# 2009-02-02 -0.002873565


# 3. (20pts)
# Perform an sapply() loop to calculate the first non-zero
# and non-NA values and their positions, for all the columns
# of the time series rutils::etfenv$returns.
# You can use the functions sapply(), match(), t(), and
# is.na().

nonav <- sapply(rutils::etfenv$returns, function(xtsv) {
  boolv <- (!is.na(xtsv)) & (xtsv != 0)
  nonap <- match(TRUE, boolv)
  c(index=nonap, value=xtsv[nonap])
})  # end sapply

nonav <- t(nonav)


# You should get the following output:
dim(nonav)
# [1]  34  2

nonav
#      index        value
# VEU   3554  0.006649741
# GLD   2977  0.008972694
# VNQ   2941  0.007992051
# USMV  4720  0.012149865
# VLUE  5096  0.014287395
# SVXY  4708  0.075244076
# SPY      2  0.007087202
# IVE   1852  0.015226597
# XLB   1492  0.010986497
# XLE   1492  0.020417553
# XLF   1492  0.014903406
# EEM   2573  0.011811161
# DBC   3282 -0.029352212
# IEF   2393 -0.011785569
# XLI   1492  0.017458395
# VTI   2106  0.006944472
# XLK   1492  0.023748225
# TLT   2393 -0.013298555
# IWB   1847 -0.002964962
# AIEQ  7806 -0.001917108
# MTUM  5093  0.012102432
# IWD   1852  0.011465550
# XLP   1492  0.023863921
# IWF   1852  0.018888817
# IVW   1852  0.040658958
# VXX   4033 -0.002873565
# QQQ   1544  0.004885208
# XLU   1492 -0.004032264
# XLV   1492  0.022126545
# VTV   2774  0.003243465
# QUAL  5156 -0.007807066
# VYM   3480  0.001584159
# USO   3326  0.002642785
# XLY   1492  0.004702203


# 4. (20pts)
# Calculate the dates of the first non-NA values for
# all the columns of the time series rutils::etfenv$returns.
# You CANNOT use the function sapply().
# You can use the function zoo::index().
# Hint: Use the variable nonav from p.3.

zoo::index(rutils::etfenv$returns)[nonav[, "index"]]

# You should get the following output:
#  [1] "2007-03-09" "2004-11-19" "2004-09-30" "2011-10-21" "2013-04-24" "2011-10-05"
#  [7] "1993-02-01" "2000-05-30" "1998-12-23" "1998-12-23" "1998-12-23" "2003-04-15"
# [13] "2006-02-07" "2002-07-29" "1998-12-23" "2001-06-01" "1998-12-23" "2002-07-29"
# [19] "2000-05-22" "2024-01-30" "2013-04-19" "2000-05-30" "1998-12-23" "2000-05-30"
# [25] "2000-05-30" "2009-02-02" "1999-03-11" "1998-12-23" "1998-12-23" "2004-02-02"
# [31] "2013-07-19" "2006-11-17" "2006-04-11" "1998-12-23"


