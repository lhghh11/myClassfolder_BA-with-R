###############################################################
# Title:        ex3.1.r
# Author:       Jason Parker
# Date:         2017-09-27
# Description:  Use the wagepan data to show panel data concepts
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(plm)
library(sandwich) 
?plm

## Data import and validation


context1    <- fread('wagepan.csv')
context1    <- plm.data(context1,index=c("nr","year"))
# variable name   type    format     label      variable label
# nr              int     %9.0g                 person identifier
# year            int     %9.0g                 1980 to 1987
# agric           byte    %9.0g                 =1 if in agriculture
# black           byte    %9.0g                 =1 if black
# bus             byte    %9.0g                 
# construc        byte    %9.0g                 =1 if in construction
# ent             byte    %9.0g                 
# exper           byte    %9.0g                 labor mkt experience
# fin             byte    %9.0g                 
# hisp            byte    %9.0g                 =1 if Hispanic
# poorhlth        byte    %9.0g                 =1 if in poor health
# hours           int     %9.0g                 annual hours worked
# manuf           byte    %9.0g                 =1 if in manufacturing
# married         byte    %9.0g                 =1 if married
# min             byte    %9.0g                 
# nrthcen         byte    %9.0g                 =1 if north central
# nrtheast        byte    %9.0g                 =1 if north east
# occ1            byte    %9.0g                 
# occ2            byte    %9.0g                 
# occ3            byte    %9.0g                 
# occ4            byte    %9.0g                 
# occ5            byte    %9.0g                 
# occ6            byte    %9.0g                 
# occ7            byte    %9.0g                 
# occ8            byte    %9.0g                 
# occ9            byte    %9.0g                 
# per             byte    %9.0g                 
# pro             byte    %9.0g                 
# pub             byte    %9.0g                 
# rur             byte    %9.0g                 
# south           byte    %9.0g                 =1 if south
# educ            byte    %9.0g                 years of schooling
# tra             byte    %9.0g                 
# trad            byte    %9.0g                 
# union           byte    %9.0g                 =1 if in union
# lwage           float   %9.0g                 log(wage)
# d81             byte    %9.0g                 =1 if year == 1981
# d82             byte    %9.0g                 
# d83             byte    %9.0g                 
# d84             byte    %9.0g                 
# d85             byte    %9.0g                 
# d86             byte    %9.0g                 
# d87             byte    %9.0g                 
# expersq         int     %9.0g                 exper^2
summary(context1)
head(context1)

# Model %change in wage 
model1      <- plm(lwage~educ+exper,model="pooling",data=context1)
model2      <- plm(lwage~educ+exper,model="within",data=context1)
model3      <- plm(lwage~educ+exper,model="between",data=context1)

summary(model1) # pooled ols
summary(model2) # fixed effects
summary(model3)

# White correction for heteroskedasticity
summary(model1, vcov=vcovHC(model1, method = "white1"))
summary(model2, vcov=vcovHC(model2, method = "white1"))
# summary(model3, vcov=vcovHC(model3, method = "white1"))

# HAC estimator. Note: n must be >> T
summary(model1, vcov=vcovHC(model1, method = "arellano"))
summary(model2, vcov=vcovHC(model2, method = "arellano"))
# summary(model3, vcov=vcovHC(model3, method = "arellano"))
