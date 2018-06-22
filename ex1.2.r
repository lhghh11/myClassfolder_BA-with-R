##############################################
## Author:   Jason Parker
## Date:     2018-04-10
## Title:    ex1.2.R
## Purpose:  Stepwise regression and the EM algorithm
##############################################

## Import packages
library(data.table)

## Prepare workspace
rm(list=ls(all=TRUE))
context1 <- fread("BWGHT.csv")

summary(context1)
table(context1$fatheduc)
unique(context1$fatheduc)

## Try dropping fatheduc
context1 <- context1[!is.na(motheduc)]
context1$smokes    <- as.numeric(context1$cigs>0)
context2 <- context1[!is.na(fatheduc)]
model1   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context2)
summary(model1)

## Step-wise regression for variable selection using BIC (backwards induction)
model2   <- step(model1,direction="backward",k=log(nrow(context2)))
summary(model2)

## EM-Algorithm for missing data
context3 <- context1
Xdata <- cbind(context1$cigs,context1$motheduc,context1$faminc,context1$male,context1$white)
for(i in 1:100) {
  model3            <- lm(log(bwght)~Xdata+fatheduc, data=context3)
  bminus            <- coef(model3)[1:6]
  b                 <- coef(model3)[7]
  pred              <- (log(context1$bwght) - (cbind(rep(1,nrow(context1)),Xdata) %*% bminus))/b
  pred              <- ifelse(pred>=1,pred,1)
  pred              <- ifelse(pred<=18,pred,18)
  pred              <- ifelse(is.na(context1$fatheduc),pred,context1$fatheduc)
  context3$fatheduc <- pred
}
model3   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context3)
summary(model3)

model4 <- step(model3,direction="backward",k=log(nrow(context2)))
summary(model4)
