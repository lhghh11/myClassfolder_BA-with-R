###############################################################
# Title:        ex2.2.r
# Author:       Jason Parker
# Date:         2017-09-20
# Description:  Use the wage data to illustrate heteroskedasticity
###############################################################


###############################################################
## Correct WAGE1 data for heteroskedasticity
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)

## Data import and validation
context1    <- fread('WAGE1.csv')
# variable name   type    format     label      variable label
# wage            float   %8.2g                 average hourly earnings
# educ            byte    %8.0g                 years of education
# exper           byte    %8.0g                 years potential experience
# tenure          byte    %8.0g                 years with current employer

# Model %change in wage using a quadratic model of education
context1$educsq <- context1$educ^2
model      <- lm(log(wage)~educ+educsq,data=context1)
summary(model)

library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
 
# vcov(model) # Standard error matrix for coefficients
# vcovHC(model) # White-corrected standard error matrix

coeftest(model,vcov.=vcov) # Old school t test for significance (like summary)
coeftest(model,vcov.=vcovHC) # White-corrected t test for significance
# ?coeftest

## If lmtest isn't working use the following
# coeftest <- function(x,vcov.=vcov) {
#   estim <- coef(x)
#   sterr <- diag(vcov.(x))
#   tstat <- estim/sqrt(sterr)
#   pval  <- dnorm(-abs(tstat))*2
#   return(cbind(estim,sterr,tstat,pval))
# }
# coeftest(model,vcov.=vcovHC)





###############################################################
## Explore minwage232 time series data 
###############################################################

rm(list=ls(all=TRUE))

library(data.table)
library(sandwich) 
library(lmtest) # If lmtest isn't working use the following:
# coeftest <- function(x,vcov.=vcov) {
#   estim <- coef(x)
#   sterr <- diag(vcov.(x))
#   tstat <- estim/sqrt(sterr)
#   pval  <- dnorm(-abs(tstat))*2
#   return(cbind(estim,sterr,tstat,pval))
# }

context2 <- read.csv('minwage232.csv')
# variable name   type    format     label      variable label
# emp232          float   %9.0g                 employment, sector 232, 1000s
# wage232         float   %9.0g                 hourly wage, sector 232, $
# unem            float   %9.0g                 civilian unemployment rate, %
# cpi             float   %9.0g                 Consumer Price Index (urban), 1982-1984 = 100
# minwage         float   %8.0g                 Federal minimum wage, $/hour
summary(context2)
head(context2)

ts.plot(context2$emp)
ts.plot(context2$wage)
ts.plot(context2$cpi) # Data is monthly from 1947 to 1998


context2$time <- 1:612
model1 <- lm(log(emp)~log(wage)+time,data=context2)
summary(model1)

library(tseries)
kpss.test(log(context2$emp),null="Level")
kpss.test(log(context2$emp),null="Trend")
kpss.test(log(context2$wage),null="Level")
kpss.test(log(context2$wage),null="Trend")

kpss.test(diff(log(context2$emp)),null="Level")
kpss.test(diff(log(context2$emp)),null="Trend") #good
kpss.test(diff(log(context2$wage)),null="Level") #good
kpss.test(diff(log(context2$wage)),null="Trend")

## Without time
# model2 <- lm(diff(log(emp))~diff(log(wage)),data=context2)
# summary(model2)
# coeftest(model2,vcov=NeweyWest(model2,lag=10))

## With time
model3 <- lm(diff(log(emp))~diff(log(wage))+time[2:612],data=context2)
summary(model3)
coeftest(model3,vcov=NeweyWest(model3,lag=10))

# Wages are not related to employment in this industry.
