######################################
## Author:   Minke Li
## Date:     2018-02-15
## Title:    ps2.R
## Purpose:  Analyze the data in file attend.csv, CEOSAL2.csv, hprice1.csv, and JTRAIN2.csv.
######################################

rm(list=ls()) #drop all variables

library(data.table)


##Question1

## Data import
######################################
##import commands
context1 <- fread('attend.csv')

##              storage   display    value
##variable name   type    format     label      variable label
##---------------------------------------------------------------------------------------------------
##attend          byte    %8.0g                 classes attended out of 32
##termGPA         float   %9.0g                 GPA for term
##priGPA          float   %9.0g                 cumulative GPA prior to term
##ACT             byte    %8.0g                 ACT score
##final           byte    %8.0g                 final exam score
##frosh           byte    %8.0g                 =1 if freshman
##soph            byte    %8.0g                 =1 if sophomore
##hw              byte    %8.0g                 number of homeworks turned in out of 8


##creat new variables
context1$attendrt <- context1$attend/32
context1$hwrt <- context1$hw/8

## Run Linear model
model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt,data = context1)

## Summarize the linear model
summary(model1)
##Call:
##lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)
##
##Residuals:
##  Min       1Q   Median       3Q      Max 
##-1.87210 -0.28100  0.00001  0.30164  1.49711 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
##  priGPA       0.548962   0.042418  12.942  < 2e-16 ***
##  ACT          0.036099   0.006051   5.966 3.92e-09 ***
##  attendrt     1.052246   0.155436   6.770 2.81e-11 ***
##  hwrt         0.913031   0.116932   7.808 2.22e-14 ***
##  ---
##  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##
##Residual standard error: 0.4788 on 675 degrees of freedom
##Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
##F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16

##predictions
##Predict the termGPA for a student with a 32 ACT and a 2.2 priGPA who attended 28 lectures and
##turned-in 8 homework assignments.
pre1 <- data.frame(ACT=32,priGPA=2.2,attendrt=(28/32),hwrt=(8/8))
predict(model1,pre1)
##2.909664
##Predict the termGPA for a student with a similar attendance and homework pattern who had a 20 ACT
##and a 3.9 priGPA.
pre2 <- data.frame(ACT=20,priGPA=3.9,attendrt=(28/32),hwrt=(8/8))
predict(model1,pre2)
##3.409706 
##Predict the termGPA for a student with a 25 ACT and a 3.0 priGPA who attends all the classes, but only
##fnishes half the homework assignments.
pre3 <- data.frame(ACT=25,priGPA=3.0,attendrt=1,hwrt=0.5)
predict(model1,pre3)
##2.771152 
##Predict the termGPA for a similarly qualified student who turns in all the homework assignments, but
##only attends half the classes.
pre4 <- data.frame(ACT=25,priGPA=3.0,attendrt=0.5,hwrt=1)
predict(model1,pre4)
##2.701545

## Interpretations
## a. Every 10% increases on attendence rate, the termGPA increase 0.1052
## b. Every 10% increases on homework turning in rate, the termGPA increase 0.0913
## c. termGPA is 2.9
## d. termGPA is 3.4
## e. priGPA is more important, since the coefficient of priGPA is higher than ACT's.
## f. termGPA is 2.8
## g. termGPA is 2.7
## h. attendance is more important
## i. because the range of attendrt and hwrt is between 0 and 1, much more smaller than the range of priGPA and ACT score.

##------------------------------------------------------------------------------------------------------------------------
##Question2
rm(list=ls()) #drop all variables

## Data import
######################################
##import commands
context2 <- fread('CEOSAL2.csv')

##              storage   display    value
##variable name   type    format     label      variable label
##---------------------------------------------------------------------------------------------------
##salary          int     %9.0g                 1990 compensation, $1000s
##age             byte    %9.0g                 in years
##college         byte    %9.0g                 =1 if attended college
##grad            byte    %9.0g                 =1 if attended graduate school
##comten          byte    %9.0g                 years with company
##ceoten          byte    %9.0g                 years as ceo with company
##sales           float   %9.0g                 1990 firm sales, millions
##profits         int     %9.0g                 1990 profits, millions
##mktval          float   %9.0g                 market value, end 1990, mills.

##Run the linear models
model2 <- lm(log(salary)~log(mktval)+profits+ceoten,data = context2)
model3 <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales),data = context2)

#Summarize the linear models
summary(model2)
##Call:
##lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)
##Residuals:
##  Min       1Q   Median       3Q      Max 
##-2.63382 -0.34660  0.00627  0.35059  1.96220 
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
##  log(mktval) 0.2386220  0.0559166   4.267 3.25e-05 ***
##  profits     0.0000793  0.0001566   0.506   0.6132    
##  ceoten      0.0114646  0.0055816   2.054   0.0415 *  
##  ---
##  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##Residual standard error: 0.5289 on 173 degrees of freedom
##Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
##F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11
summary(model3)
##Call:
##  lm(formula = log(salary) ~ log(mktval) + profits + ceoten + log(sales), 
##     data = context2)
##Residuals:
##  Min       1Q   Median       3Q      Max 
##-2.48792 -0.29369  0.00827  0.29951  1.85524 
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
##  log(mktval) 1.018e-01  6.303e-02   1.614   0.1083    
##  profits     2.905e-05  1.503e-04   0.193   0.8470    
##  ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
##  log(sales)  1.622e-01  3.948e-02   4.109 6.14e-05 ***
##  ---
##  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##Residual standard error: 0.5062 on 172 degrees of freedom
##Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
##F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

##Interpretations
## a. Because the profit can be negative, we can not get its natural log.
## b. Every one unit increases on log mktval, with a 0.2386 increase on log salary.
## c. Every one unit increases on log mktval, with a 0.1018 increase on log salary.
## d. We add more explanatory variable in model3, which can explane the dependent variable more accurately.
## e. No.
## f. Every one unit increases on log sales, with a 0.1622 increase on log salary.

##----------------------------------------------------------------------------------------------------------
##Question3
rm(list=ls()) #drop all variables

## Data import
######################################
##import commands
context3 <- fread('hprice1.csv')

##              storage   display    value
##variable name   type    format     label      variable label
##---------------------------------------------------------------------------------------------------
##price           float   %9.0g                 house price, $1000s
##assess          float   %9.0g                 assessed value, $1000s
##bdrms           byte    %9.0g                 number of bdrms
##lotsize         float   %9.0g                 size of lot in square feet
##sqrft           int     %9.0g                 size of house in square feet
##colonial        byte    %9.0g                 =1 if home is colonial style

##Run the linear models
model4 <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial,data = context3)
model5 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial,data = context3)

##Summarize the linear models
summary(model4)
##Call:
##  lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
##     data = context3)
##Residuals:
##  Min       1Q   Median       3Q      Max 
##-109.603  -38.258   -4.325   22.984  220.766 
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)  -2030.455    210.967  -9.625 3.68e-15 ***
##  bdrms           18.572      9.308   1.995   0.0493 *  
##  log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
##  log(sqrft)     225.508     30.072   7.499 6.41e-11 ***
##  colonial         4.134     14.509   0.285   0.7764    
##---
##  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##Residual standard error: 59.66 on 83 degrees of freedom
##Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
##F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16
summary(model5)
##Call:
##  lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
##       colonial, data = context3)
##Residuals:
##  Min       1Q   Median       3Q      Max 
##-0.69479 -0.09750 -0.01619  0.09151  0.70228 
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)  -1.34959    0.65104  -2.073   0.0413 *  
##  bdrms         0.02683    0.02872   0.934   0.3530    
##log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
##  log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
##  colonial      0.05380    0.04477   1.202   0.2330    
##---
##  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##Residual standard error: 0.1841 on 83 degrees of freedom
##Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
##F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16


##Interpretations
## a. Every one unit increases on log lotsize, with a $61.45 increase on price.
## b. Every one unit increases on log lotsize, with a 0.1678 increase on log price.
## c. Every one unit increases on colonial, with a $4.13 increase on price.
## d. First model is better
## e. 18.57+225.51*log1.1=40.06  40k+20k-50k=10k > 0  Yes, you should pursue the expansion.

##------------------------------------------------------------------------------------
##Question4
rm(list=ls()) #drop all variables

## Data import
######################################
##import commands
context4 <- fread('JTRAIN2.csv')

##              storage   display    value
##variable name   type    format     label      variable label
##---------------------------------------------------------------------------------------------------
##train           byte    %9.0g                 =1 if assigned to job training
##age             byte    %9.0g                 age in 1977
##educ            byte    %9.0g                 years of education
##black           byte    %9.0g                 =1 if black
##hisp            byte    %9.0g                 =1 if Hispanic
##married         byte    %9.0g                 =1 if married
##nodegree        byte    %9.0g                 =1 if no high school degree
##mosinex         byte    %9.0g                 # mnths prior to 1/78 in expmnt
##re74            float   %9.0g                 real earns., 1974, $1000s
##re75            float   %9.0g                 real earns., 1975, $1000s
##re78            float   %9.0g                 real earns., 1978, $1000s
##unem74          byte    %9.0g                 =1 if unem. all of 1974
##unem75          byte    %9.0g                 =1 if unem. all of 1975
##unem78          byte    %9.0g                 =1 if unem. all of 1978

##Run the linear models
model6 <- lm(re78~re75+train+educ+black,data = context4)

##Summarize the linear models
summary(model6)
##Call:
##lm(formula = re78 ~ re75 + train + educ + black, data = context4)
##Residuals:
##  Min     1Q Median     3Q    Max 
##-9.120 -4.377 -1.756  3.353 54.058 
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)   
##(Intercept)  1.97686    1.89028   1.046   0.2962   
##  re75         0.14697    0.09811   1.498   0.1349   
##  train        1.68422    0.62700   2.686   0.0075 **
##  educ         0.41026    0.17267   2.376   0.0179 * 
##  black       -2.11277    0.82941  -2.547   0.0112 * 
##  ---
##  Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
##Residual standard error: 6.496 on 440 degrees of freedom
##Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
##F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

##Interpretations
## a. Every one dollar increases on real earnings in 75, with a $0.15 increase on real earnings in 78.
## b. If the job is assigned to train, $1.68 higher real earnings in 78 can be gotten, and the coefficient is significant.
## c. If you are black, you will get $2.11 lower on real earnings in 78. 
