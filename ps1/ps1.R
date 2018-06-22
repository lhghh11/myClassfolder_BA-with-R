######################################
## Author:   Minke Li
## Date:     2018-01-26
## Title:    ps1.R
## Purpose:  Analyze the data in file Wage1.csv 
######################################

rm(list=ls()) #drop all variables

library(data.table)

## Data import
######################################
#import commands
context1 <- fread('Wage1.csv')

## Get summary statistics for all variables
summary(context1)

## Data description from WAGE1_labels.txt
#              storage   display    value
#variable name   type    format     label      variable label
#---------------------------------------------------------------------------------------------------
#wage            float   %8.2g                 average hourly earnings
#educ            byte    %8.0g                 years of education
#exper           byte    %8.0g                 years potential experience
#tenure          byte    %8.0g                 years with current employer
#nonwhite        byte    %8.0g                 =1 if nonwhite
#female          byte    %8.0g                 =1 if female
#married         byte    %8.0g                 =1 if married
#numdep          byte    %8.0g                 number of dependents
#smsa            byte    %8.0g                 =1 if live in SMSA
#northcen        byte    %8.0g                 =1 if live in north central U.S
#south           byte    %8.0g                 =1 if live in southern region
#west            byte    %8.0g                 =1 if live in western region
#construc        byte    %8.0g                 =1 if work in construc. indus.
#ndurman         byte    %8.0g                 =1 if in nondur. manuf. indus.
#trcommpu        byte    %8.0g                 =1 if in trans, commun, pub ut
#trade           byte    %8.0g                 =1 if in wholesale or retail
#services        byte    %8.0g                 =1 if in services indus.
#profserv        byte    %8.0g                 =1 if in prof. serv. indus.
#profocc         byte    %8.0g                 =1 if in profess. occupation
#clerocc         byte    %8.0g                 =1 if in clerical occupation
#servocc         byte    %8.0g                 =1 if in service occupation

## New variable for the natural logarithm of wage
context1$lwage <- log(context1$wage)

## Run Linear models 
model1 <- lm(wage~educ,data = context1)
model2 <- lm(wage~educ+exper+tenure, data = context1)
model3 <- lm(lwage~educ+exper+tenure, data = context1)

## Interpret the estimated coefficients and intercepts
coef(model1)
coef(model2)
coef(model3)

## Interpretations
#a. Every 1 year increase in total years of education is
#   with $0.5447 increase in the average 
#   hourly wage
#b. Every 1 year increase in total years of education is
#   with $0.6027 increase in the average 
#   hourly wage
#c. Every 1 year increase in total years of experience is
#   with $0.0225 increase in the average 
#   hourly wage
#d. Every 1 year increase in total years of tenure is
#   with $0.1700 increase in the average 
#   hourly wage
#e. For a worker without education, experience, and tunure
#   the model predicts a -$2.914 average hourly wage,
#   which means this worker cannot get wage or this kind of 
#   worker will not be accepted
#f. Every 1 year increase in total years of education is
#   with a 9.226% increase in the natural 
#   logarithm of average hourly wage
#g. Every 1 year increase in total years of experience is
#   with a 0.4137% increase in the natural 
#   logarithm of average hourly wage
#h. Every 1 year increase in total years of tenure is
#   with a 2.211% increase in the natural 
#   logarithm of average hourly wage
