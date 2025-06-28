#################################
### FRE7241 Homework #1 Solution due at 6AM Tuesday September 10th.
#################################
# Max score 70pts

# The below solutions are examples.
# Slightly different solutions are also possible.


##############
# Summary: Calculate a matrix of the best performing ETFs
# in each year.  Create a scatterplot of alphas for the
# years 2008 and 2009.

## Run all the setup code below.

library(rutils)
library(PerformanceAnalytics)
symbolv <- c("VTI", "IEF", "VNQ", "USO", "XLY", "XLP", "XLE", "XLF", "XLK")
retp <- rutils::etfenv$returns[, symbolv]
retp <- na.omit(zoo::na.locf(retp))

## End of setup code.


# 1. (20pts)
# Create a vector of yearly end points of the returns.
# You can use the function rutils::calc_endpoints().

endd <- rutils::calc_endpoints(retp, interval="years")

# You should get the following output:
endd
# [1]    0  183  434  687  939 1191 1443 1693 1945 2197 2449 2701 2952 3203
# [15] 3455 3708 3960 4211 4461 4629

# Select the returns for the symbolv, and for the dates between 
# the last two end points.
# You can use the function NROW().

xtsv <- retp[endd[NROW(endd)-1]:endd[NROW(endd)], symbolv]

# You should get the following outputs:
head(xtsv)
#                     VTI          IEF          VNQ          USO          XLY
# 2023-12-29 -0.003828762 -0.002486790 -0.011924992 -0.007920538 -0.006188518
# 2024-01-02 -0.006725202 -0.003741819  0.008564398 -0.013291239 -0.009326958
# 2024-01-03 -0.009637601  0.002392221 -0.024190607  0.036134119 -0.020415855
# 2024-01-04 -0.002574335 -0.006043574 -0.001840985 -0.009133829 -0.007053683
# 2024-01-05  0.001502501 -0.003664734 -0.002536902  0.021377095  0.001565354
# 2024-01-08  0.014183427  0.003351139  0.014215532 -0.038844136  0.016888218
#                     XLP           XLE          XLF           XLK
# 2023-12-29  0.002084637 -0.0022636574 -0.003186407 -0.0030605652
# 2024-01-02  0.011182556  0.0106775246  0.004246291 -0.0265334580
# 2024-01-03 -0.007856150  0.0161539273 -0.008510690 -0.0102422002
# 2024-01-04 -0.001384658 -0.0176892096  0.003998406 -0.0073573499
# 2024-01-05 -0.002219449  0.0008269833  0.004247418 -0.0002715252
# 2024-01-08  0.007333136 -0.0116404688  0.006337492  0.0247826876
tail(xtsv)
#                      VTI           IEF          VNQ         USO          XLY
# 2024-08-23  0.0125792382  0.0046123179  0.021758538  0.02642144  0.019131112
# 2024-08-26 -0.0025248895 -0.0012278729 -0.000422119  0.02973227 -0.007801685
# 2024-08-27  0.0009024783 -0.0004096262  0.001476638 -0.01736695 -0.002632075
# 2024-08-28 -0.0061166698 -0.0005122688 -0.003907286 -0.01329229 -0.009728755
# 2024-08-29  0.0009797341 -0.0017436796 -0.003073176  0.01694622  0.001194160
# 2024-08-30  0.0096012288 -0.0034965071  0.009611904 -0.03216477  0.014968244
#                     XLP          XLE         XLF          XLK
# 2024-08-23  0.002814318  0.014823342 0.008589565  0.016068760
# 2024-08-26  0.007062004  0.008921255 0.003146070 -0.012626260
# 2024-08-27  0.002423656 -0.009363880 0.005147152  0.006040952
# 2024-08-28 -0.004245261 -0.006440173 0.002452348 -0.013894455
# 2024-08-29 -0.004996050  0.012949105 0.008867272 -0.009064751
# 2024-08-30  0.007909027  0.003731757 0.009445430  0.013203472


# Perform an lapply() loop over the neighboring end points 
# of endd.
# Inside the loop, select the returns for the symbolv and
# the neighboring end points.  
# Then for the selected returns, calculate a data frame of 
# statistics using table.CAPM(), and using VTI as the 
# benchmark asset "Rb".
# Simplify the column names and return the data frame.
# The output should be a list of data frames. 
# You can use the functions lapply(), sapply(), table.CAPM(),
# colnames(), strsplit(), NROW(), and an anonymous function.
# You don't need to use all of these functions.
# You can use any functions you choose.

capml <- lapply(2:NROW(endd), function(it) {
  xtsv <- retp[endd[(it-1)]:endd[it], symbolv]
  capmd <- PerformanceAnalytics::table.CAPM(Ra=xtsv[, -1],
                                            Rb=xtsv[, 1], scale=252)
  colnames(capmd) <- sapply(colnames(capmd),
                            function(str) {strsplit(str, split=" ")[[1]][1]})
  capmd
})  # end lapply

# You should get the following outputs:
is.list(capml)
# [1] TRUE
capml[[1]]
#                         IEF    VNQ     USO    XLY    XLP     XLE    XLF     XLK
# Alpha                0.0003 0.0007 -0.0016 0.0002 0.0004 -0.0004 0.0003 -0.0002
# Beta                 0.0326 0.9094  0.0865 0.9956 0.5466  1.2493 0.9259  1.1092
# Beta+                0.0831 0.8206  0.6261 1.0077 0.3818  1.4792 0.8758  1.0898
# Beta-                0.0188 0.6517  0.0731 0.9016 0.6239  1.5042 0.9472  0.9853
# R-squared            0.0069 0.4583  0.0015 0.7497 0.4770  0.3022 0.7175  0.7213
# Annualized Alpha     0.0689 0.1926 -0.3242 0.0632 0.1090 -0.0886 0.0769 -0.0604
# Correlation          0.0832 0.6770  0.0383 0.8658 0.6906  0.5498 0.8471  0.8493
# Correlation p-value  0.2631 0.0000  0.6064 0.0000 0.0000  0.0000 0.0000  0.0000
# Tracking Error       0.1144 0.1089  0.2668 0.0631 0.0801  0.2099 0.0642  0.0765
# Active Premium      -0.0577 0.1957 -0.4673 0.0684 0.0550 -0.0928 0.0739 -0.0578
# Information Ratio   -0.5040 1.7976 -1.7512 1.0842 0.6870 -0.4421 1.1511 -0.7553
# Treynor Ratio        2.2196 0.3582 -3.8978 0.1993 0.3385  0.0298 0.2203  0.0651

# Assign names to the list using the years corresponding to the endd.
# You can use the functions names(), format() with the "%Y" format, 
# and zoo::index(),

names(capml) <- format(zoo::index(retp[endd, ]), "%Y")

# You should get the following outputs:
names(capml)
#  [1] "2006" "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015"
# [11] "2016" "2017" "2018" "2019" "2020" "2021" "2022" "2023" "2024"


# 2. (10pts)
# Perform an sapply() loop over "capml".
# Inside the loop extract the data frame row called
# "Annualized Alpha", coerce it to a vector using unlist(),
# and return the vector.
# You can use the functions sapply(), unlist(),
# and an anonymous function,

alphav <- sapply(capml, function(capmd) {
  unlist(capmd["Annualized Alpha", ])
})  # end sapply

# alphav should be a matrix of annual alphas like this:
dim(alphav)
# [1]  8 19
alphav
#        2006    2007    2008    2009    2010    2011    2012    2013    2014    2015    2016
# IEF  0.0689  0.1137  0.1205 -0.0488  0.1229  0.1639  0.0790 -0.0470  0.1114  0.0152  0.0290
# VNQ  0.1926 -0.2144  0.2848 -0.2019  0.0418  0.0730  0.0489 -0.2262  0.2165  0.0116 -0.0101
# USO -0.3242  0.4603 -0.4475 -0.0128 -0.1545 -0.0072 -0.2647 -0.1209 -0.4477 -0.4537 -0.0814
# XLY  0.0632 -0.1814  0.0540  0.0706  0.0763  0.0481  0.0654  0.0602 -0.0286  0.1013 -0.0575
# XLP  0.1090  0.0921  0.0858  0.0086  0.0341  0.1348  0.0166  0.0090  0.0800  0.0608 -0.0252
# XLE -0.0886  0.2754  0.0866 -0.0996  0.0129  0.0183 -0.1196 -0.0710 -0.1912 -0.2141  0.1115
# XLF  0.0769 -0.2474 -0.0868 -0.3040 -0.0766 -0.1798  0.0701 -0.0409  0.0257 -0.0217  0.0622
# XLK -0.0604  0.1025 -0.1060  0.2115 -0.0430  0.0164 -0.0115 -0.0142  0.0523  0.0502  0.0123
#        2017    2018    2019    2020    2021    2022    2023    2024
# IEF  0.0726  0.0061  0.1485  0.1170 -0.0249 -0.1376  0.0253  0.0178
# VNQ -0.0382 -0.0305  0.1478 -0.2243  0.1978 -0.1146 -0.1366 -0.0517
# USO -0.0468 -0.1623  0.0594 -0.7307  0.3353  0.3262 -0.1133  0.1348
# XLY  0.0249  0.0743 -0.0200  0.0799 -0.0163 -0.1583  0.0398 -0.2107
# XLP  0.0350 -0.0508  0.1040 -0.0376  0.0715  0.1145 -0.1125  0.1733
# XLE -0.1553 -0.1372 -0.1606 -0.4797  0.1888  0.8886 -0.1496  0.0428
# XLF -0.0530 -0.0856 -0.0047 -0.2174  0.0957  0.0810 -0.1099  0.1570
# XLK  0.0696  0.0545  0.0496  0.1615  0.0111 -0.0488  0.1940 -0.1617


# 3. (20pts)
# Sort the last column of alphav in descending order, 
# and extract the ETF names.
# You can use the functions names(), sort(), and NCOL().

names(sort(alphav[, NCOL(alphav)], decreasing=TRUE))

# You should get the following output:
# [1] "XLP" "XLF" "USO" "XLE" "IEF" "VNQ" "XLK" "XLY"

# Sort the all the columns of alphav in descending order, 
# using an apply() loop over the columns of alphav, and 
# extract the ETF names.
# You can use the functions apply(), sort(), names(),
# and an anonymous function,

namev <- apply(alphav, MARGIN=2, FUN=function(colnum) {
  colnum <- sort(colnum, decreasing=TRUE)
  names(colnum)
})  # end apply

# You should get the following outputs:
dim(namev)
# [1]  8 19
namev
#       2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018  2019 
# [1,] "VNQ" "USO" "VNQ" "XLK" "IEF" "IEF" "IEF" "XLY" "VNQ" "XLY" "XLE" "IEF" "XLY" "IEF"
# [2,] "XLP" "XLE" "IEF" "XLY" "XLY" "XLP" "XLF" "XLP" "IEF" "XLP" "XLF" "XLK" "XLK" "VNQ"
# [3,] "XLF" "IEF" "XLE" "XLP" "VNQ" "VNQ" "XLY" "XLK" "XLP" "XLK" "IEF" "XLP" "IEF" "XLP"
# [4,] "IEF" "XLK" "XLP" "USO" "XLP" "XLY" "VNQ" "XLF" "XLK" "IEF" "XLK" "XLY" "VNQ" "USO"
# [5,] "XLY" "XLP" "XLY" "IEF" "XLE" "XLE" "XLP" "IEF" "XLF" "VNQ" "VNQ" "VNQ" "XLP" "XLK"
# [6,] "XLK" "XLY" "XLF" "XLE" "XLK" "XLK" "XLK" "XLE" "XLY" "XLF" "XLP" "USO" "XLF" "XLF"
# [7,] "XLE" "VNQ" "XLK" "VNQ" "XLF" "USO" "XLE" "USO" "XLE" "XLE" "XLY" "XLF" "XLE" "XLY"
# [8,] "USO" "XLF" "USO" "XLF" "USO" "XLF" "USO" "VNQ" "USO" "USO" "USO" "XLE" "USO" "XLE"
#      2020  2021  2022  2023  2024 
# [1,] "XLK" "USO" "XLE" "XLK" "XLP"
# [2,] "IEF" "VNQ" "USO" "XLY" "XLF"
# [3,] "XLY" "XLE" "XLP" "IEF" "USO"
# [4,] "XLP" "XLF" "XLF" "XLF" "XLE"
# [5,] "XLF" "XLP" "XLK" "XLP" "IEF"
# [6,] "VNQ" "XLK" "VNQ" "USO" "VNQ"
# [7,] "XLE" "XLY" "IEF" "VNQ" "XLK"
# [8,] "USO" "IEF" "XLY" "XLE" "XLY"


# 4. (20pts)
# Plot a scatterplot of the alphas for the years 
# "2008" and "2009", and add labels with ETF names.
# You can use the functions plot(), rownames(), 
# range(), and text().
# Hint: Use the parameters xlim and ylim in the 
# function plot(), to enlarge the dimensions of 
# the plot, and make room for the labels.
# Your plot should be similar to scatter_etfs.png

year1 <- "2008"
year2 <- "2009"
# Get the range of the alphas for year1 and year2
rangex <- range(alphav[, year1])
rangex[1] <- rangex[1] - 0.1
rangex[2] <- rangex[2] + 0.1
rangey <- range(alphav[, year2])
rangey[1] <- rangey[1] - 0.1
rangey[2] <- rangey[2] + 0.1
# Plot a scatterplot of the alphas for year1 and year2
plot(x=alphav[, year1], y=alphav[, year2],
     xlab=year1, ylab=year2,
     xlim=rangex, ylim=rangey)
# Add labels with ETF names
text(x=alphav[, year1], y=alphav[, year2],
     labels=rownames(alphav), pos=4, cex=0.8)


