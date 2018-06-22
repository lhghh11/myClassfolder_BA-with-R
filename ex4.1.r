###############################################################
# Title:        ex4.2.r
# Author:       Jason Parker
# Date:         2017-10-25
# Description:  Show the GLM model
###############################################################

## MROZ.csv

rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(ggplot2)
library(lmtest)
library(sandwich)
library(glmx)
library(margins)

## Data import and validation
context1    <- fread('MROZ.csv')
# variable name   type    format     label      variable label
# inlf            byte    %9.0g                 =1 if in lab frce, 1975
# hours           int     %9.0g                 hours worked, 1975
# kidslt6         byte    %9.0g                 # kids < 6 years
# kidsge6         byte    %9.0g                 # kids 6-18
# age             byte    %9.0g                 woman's age in yrs
# educ            byte    %9.0g                 years of schooling
# wage            float   %9.0g                 est. wage from earn, hrs
# repwage         float   %9.0g                 rep. wage at interview in 1976
# hushrs          int     %9.0g                 hours worked by husband, 1975
# husage          byte    %9.0g                 husband's age
# huseduc         byte    %9.0g                 husband's years of schooling
# huswage         float   %9.0g                 husband's hourly wage, 1975
# faminc          float   %9.0g                 family income, 1975
# mtr             float   %9.0g                 fed. marg. tax rte facing woman
# motheduc        byte    %9.0g                 mother's years of schooling
# fatheduc        byte    %9.0g                 father's years of schooling
# unem            float   %9.0g                 unem. rate in county of resid.
# city            byte    %9.0g                 =1 if live in SMSA
# exper           byte    %9.0g                 actual labor mkt exper
summary(context1)

context1$expersq <- context1$exper^2

model1 <- lm(inlf~educ+exper+expersq+age+kidslt6+kidsge6,data=context1)
summary(model1)
coeftest(model1,vcov.=vcovHC)

newwomen <- data.table(V1=c(16,20,11),V2=c(5,15,3),
                       V3=c(30,40,30),V4=c(0,0,3),V5=c(0,2,0))
colnames(newwomen) <- c('educ','exper','age','kidslt6','kidsge6')
newwomen$expersq <- newwomen$exper^2
predict(model1,newwomen)
# First woman has an 81% chance of being in the labor force
# Second woman has a 108% chance of being in the labor force ??? 
# Third woman has a -22% chance of being in the labor force ???

model2 <- glm(inlf~educ+exper+expersq+age+kidslt6+kidsge6,family=binomial(link="logit"),data=context1)
summary(model2)

coeftest(model1)
coeftest(model2)
coeftest(model1,vcov.=vcovHC)
coeftest(model2,vcov.=vcovHC)

predict(model2,newwomen)
predict(model2,newwomen,type="response")
# First woman has an 85% chance of being in the labor force
# Second woman has a 96% chance of being in the labor force
# Third woman has a 2% chance of being in the labor force

margins(model2)
#Every year of education makes you 3.3% more likely to be in the labor force controlling for
#Every year of experience makes you 2.6% more likely to be in the labor force controlling for
#Every child less than 6 makes you 25.7% more likely to be in the labor force controlling for
?margins



p <- ggplot(context1,aes(x=educ,y=inlf)) + geom_point()
p <- p + geom_line(aes(y=predict(lm(inlf~educ,data=context1))),color="red",size=2)
p <- p + geom_line(aes(y=predict(glm(inlf~educ,family=binomial(link="logit"),data=context1),type="response")),color="blue",size=2)
p

## Crime1.csv

rm(list=ls(all=TRUE))

## Data import and validation
context2    <- fread('CRIME1.csv')
# variable name   type    format     label      variable label
# narr86          byte    %9.0g                 # times arrested, 1986
# nfarr86         byte    %9.0g                 # felony arrests, 1986
# nparr86         byte    %9.0g                 # property crme arr., 1986
# pcnv            float   %9.0g                 proportion of prior convictions
# avgsen          float   %9.0g                 avg sentence length, mos.
# tottime         float   %9.0g                 time in prison since 18 (mos.)
# ptime86         byte    %9.0g                 mos. in prison during 1986
# qemp86          float   %9.0g                 # quarters employed, 1986
# inc86           float   %9.0g                 legal income, 1986, $100s
# durat           float   %9.0g                 recent unemp duration
# black           byte    %9.0g                 =1 if black
# hispan          byte    %9.0g                 =1 if Hispanic
# born60          byte    %9.0g                 =1 if born in 1960
summary(context2)

model3 <- lm(narr86~tottime+qemp86+inc86+black+hispan,data=context2)
summary(model3)

model4 <- glm(narr86~tottime+qemp86+black+hispan,family=poisson(link="log"),data=context2)
summary(model4)

coeftest(model3)
coeftest(model4)
coeftest(model3,vcov.=vcovHC)
coeftest(model4,vcov.=vcovHC)
