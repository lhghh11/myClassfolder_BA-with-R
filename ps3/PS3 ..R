##################################
# Title: ps3.r
# Author: Yubo Wen
# Date: 2018-04-01
#################################
rm(list=ls(all=TRUE))

library(data.table)
library(sandwich) 
library(lmtest)
library(plm)
library(tseries)

##Question 1
## Data import and validation
context1    <- fread('hprice1.csv')
# ---------------------------------------------------------------------------------------------------
#     storage   display    value
# variable name   type    format     label      variable label
# ---------------------------------------------------------------------------------------------------
# price           float   %9.0g                 house price, $1000s
# assess          float   %9.0g                 assessed value, $1000s
# bdrms           byte    %9.0g                 number of bdrms
# lotsize         float   %9.0g                 size of lot in square feet
# sqrft           int     %9.0g                 size of house in square feet
# colonial        byte    %9.0g                 =1 if home is colonial style
# ---------------------------------------------------------------------------------------------------
#
#Model1
model1      <- lm(price~bdrms+lotsize+sqrft,data=context1)
coeftest(model1,vcov.=vcov) # Old school t test for significance (like summary)
# t test of coefficients:
#   
#                Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
# bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
# lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
# sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model1,vcov.=vcovHC) # White-corrected t test for significance

# t test of coefficients:
# 
# Estimate  Std. Error t value Pr(>|t|)   
# (Intercept) -21.7703086  41.0326944 -0.5306 0.597124   
# bdrms        13.8525219  11.5617901  1.1981 0.234236   
# lotsize       0.0020677   0.0071485  0.2893 0.773101   
# sqrft         0.1227782   0.0407325  3.0143 0.003406 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
#Model2
model2      <- lm(log(price)~bdrms+log(lotsize)+log(sqrft),data=context1)

coeftest(model2,vcov.=vcov) # Old school t test for significance (like summary)

# t test of coefficients:
#   
#                Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
# bdrms         0.036958   0.027531  1.3424   0.18308    
# log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
# log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

coeftest(model2,vcov.=vcovHC) # White-corrected t test for significance

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.850457 -1.5251  0.130988    
# bdrms         0.036958   0.035576  1.0389  0.301845    
# log(lotsize)  0.167967   0.053275  3.1528  0.002243 ** 
#   log(sqrft)    0.700232   0.121392  5.7683 1.298e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# a. Identify which variables are significant using the OLS test for model1.
# Size of lot in square feet and size of house in square feet.

# b. Which variables are still significant after using the White-corrected significance test for model1?
# Size of house in square feet.

# c. Identify which variables are significant using the OLS test for model2.
# Size of lot in square feet and size of house in square feet.

# d. Which variables are still significant after using the White-corrected significance test for model2?
# Size of lot in square feet and size of house in square feet.

# e. Keeping these results in mind, what is the effect of taking logs on heteroskedasticity in the data?
# It solves the heroskedasticity problem in the model


#Question 2
context2    <- fread('beveridge.csv')

#model3

model3      <- lm(urate~vrate,data=context2)


#Significance test for model3
coeftest(model3,vcov.=vcov) 

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    0.59200  28.918 < 2.2e-16 ***
#   vrate       -3.74145    0.20681 -18.091 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Newey-West-corrected siginificance test
coeftest(model3,vcov=NeweyWest(model3,lag=5))

# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    1.36561  12.536 < 2.2e-16 ***
#   vrate       -3.74145    0.39575  -9.454 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#Perform the level and trend KPSS tests on uratet and vratet

kpss.test((context2$urate),null="Level")

# KPSS Test for Level Stationarity
# 
# data:  (context2$urate)
# KPSS Level = 2.6835, Truncation lag parameter = 2, p-value = 0.01

kpss.test((context2$urate),null="Trend")

# KPSS Test for Trend Stationarity
# 
# data:  (context2$urate)
# KPSS Trend = 0.74085, Truncation lag parameter = 2, p-value = 0.01
# 
# Warning message:
#   In kpss.test((context2$urate), null = "Trend") :
#   p-value smaller than printed p-value

kpss.test((context2$vrate),null="Level")

# KPSS Test for Level Stationarity

# data:  (context2$vrate)
# KPSS Level = 1.2735, Truncation lag parameter = 2, p-value = 0.01
# 
# Warning message:
#   In kpss.test((context2$vrate), null = "Level") :
#   p-value smaller than printed p-value

kpss.test((context2$vrate),null="Trend")


# KPSS Test for Trend Stationarity
# 
# data:  (context2$vrate)
# KPSS Trend = 0.43255, Truncation lag parameter = 2, p-value = 0.01

#Perform the level and trend KPSS tests on Delta uratet and Delta vratet
kpss.test(diff(context2$urate),null="Level")
kpss.test(diff(context2$urate),null="Trend")
kpss.test(diff(context2$vrate),null="Level")
kpss.test(diff(context2$vrate),null="Trend")

#Perform the level and trend KPSS tests on Delta(Delta uratet) and Delta(Delta vratet) 
kpss.test(diff(diff(context2$urate)),null="Level")
kpss.test(diff(diff(context2$urate)),null="Trend")
kpss.test(diff(diff(context2$vrate)),null="Level")
kpss.test(diff(diff(context2$vrate)),null="Trend")

#model4
model4 <- lm(diff(urate)~diff(vrate),data=context2)
#Compute the OLS significance test results for model4
coeftest(model4,vcov.=vcov) 
#Newey-West-corrected significance test for model4
coeftest(model4,vcov=NeweyWest(model4,lag=5))

# Interpretations
# f. Do the OLS and NeweyWest significance tests show that the coefficient on the vanancy rate is significant
# or not (before we correct for stationarity)?
# Yes it is sighnificant
# g. Based on the KPSS findings, which transformation/transformations should we apply to the unemployment
# rate before modeling?
# First level difference
# h. Based on the KPSS fndings, which transformation/transformations should we apply to the vacancy rate
# before modeling?
# First level difference
# i. How have the significance tests changed from model3 to model4?
# The vanancy rate is significant in model3 but not significant in model4.
# j. Which model better describes the data?
# model3

##Question 3
## Data import and validation
context3    <- fread('JTrain.csv')
#---------------------------------------------------------------------------------------------------
# storage        display    value
# variable name   type    format     label      variable label
# ---------------------------------------------------------------------------------------------------
# year            int     %9.0g                 1987, 1988, or 1989
# fcode           float   %9.0g                 firm code number
# employ          int     %9.0g                 # employees at plant
# sales           float   %9.0g                 annual sales, $
#   avgsal          float   %9.0g                 average employee salary
# scrap           float   %9.0g                 scrap rate (per 100 items)
# rework          float   %9.0g                 rework rate (per 100 items)
# tothrs          int     %9.0g                 total hours training
# union           byte    %9.0g                 =1 if unionized
# grant           byte    %9.0g                 = 1 if received grant
# totrain         int     %9.0g                 total employees trained
# ---------------------------------------------------------------------------------------------------

#Generate new variables

context3$d88=ifelse(context3$year==1988,1,0)
context3$d89=ifelse(context3$year==1989,1,0)

for(i in 1:nrow(context3)) 
{
  if(i%%3 == 1) context3$grant_1[i] <- 0
  else  context3$grant_1[i] <- context3$grant[i-1]
}

#run model
context3    <- plm.data(context3,index=c("year"))
model5      <- plm(log(scrap)~d88+d89+grant+grant_1,model="pooling",data=context3)
summary(model5)

# Pooling Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_1, data = context3, 
#       model = "pooling")
# 
# Unbalanced Panel: n=3, T=54-54, N=162
# 
# Residuals :
#   Min.  1st Qu.   Median  3rd Qu.     Max. 
# -5.20260 -0.89599 -0.08461  1.02417  3.30029 
# 
# Coefficients :
#             Estimate Std. Error t-value Pr(>|t|)   
# (Intercept)  0.597434   0.203063  2.9421 0.003754 **
# d88         -0.239370   0.310864 -0.7700 0.442447   
# d89         -0.496524   0.337928 -1.4693 0.143748   
# grant        0.200020   0.338285  0.5913 0.555186   
# grant_1      0.048936   0.436066  0.1122 0.910792   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    355.75
# Residual Sum of Squares: 349.59
# R-Squared:      0.017311
# Adj. R-Squared: -0.0077257
# F-statistic: 0.691427 on 4 and 157 DF, p-value: 0.59893

model6      <- plm(log(scrap)~d88+d89+grant+grant_1,model="within",data=context3)

# summary(model6)
# 
# Oneway (individual) effect Within Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_1, data = context3, 
#       model = "within")
# 
# Unbalanced Panel: n=3, T=54-54, N=162
# 
# Residuals :
#   Min.  1st Qu.   Median  3rd Qu.     Max. 
# -5.20260 -0.89599 -0.08461  1.02417  3.30029 
# 
# Coefficients :
#          Estimate Std. Error t-value Pr(>|t|)
# grant   0.200020   0.338285  0.5913   0.5552
# grant_1 0.048936   0.436066  0.1122   0.9108
# 
# Total Sum of Squares:    350.37
# Residual Sum of Squares: 349.59
# R-Squared:      0.0022242
# Adj. R-Squared: -0.023197
# F-statistic: 0.17499 on 2 and 157 DF, p-value: 0.83963


# OLS significance test results
coeftest(model6,vcov.=vcov)
# t test of coefficients:
#   
#           Estimate Std. Error t value Pr(>|t|)
# grant   0.200020   0.338285  0.5913   0.5552
# grant_1 0.048936   0.436066  0.1122   0.9108


# HAC-corrected significance test
summary(model6, vcov=vcovHC(model6, method = "arellano"))

# k. Interpret the estimated coeffcient on grantit in model5.
# 0.200
# l. Interpret the estimated coeffcient on grantit????1 in model5
# 0.0489.
# m. How do you interpret the signs of beta3 and beta4?
# The signs is positive. This means if the grantit and granti,t-1 are received,the scrap rate for a manufacturing firm
# whould go up.
# n. Interpret the estimated coeffcient on grantit in model6.
# 0.200
# o. Interpret the estimated coeffcient on granti,t-1 in model6.
# 0.049
# p. How do you interpret the signs of beta3 and beta4 now?
# The signs is positive. This means if the grantit and granti,t-1 are received,the scrap rate for a manufacturing firm
# whould go up.
# q. How do the significance results change from using the HAC (Arellano) significance results compared to
# OLS?
# The virable which is not significant in model5 now is significant in model6.
