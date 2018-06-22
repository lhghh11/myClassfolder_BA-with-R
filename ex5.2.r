###############################################################
# Title:        ex5.2.r
# Author:       Jason Parker
# Date:         2017-03-27
# Description:  Show recursive partitioning for classification
###############################################################

rm(list=ls())

library(data.table)
library(rpart)
library(tree)
library(party)
library(evtree)

# read raw data file (change to work on your system)
titanic	<-	read.csv("titanic.csv")
head(titanic)

# create numeric outcome variable (instead of the character var "survived")
titanic$alive		<-	ifelse(titanic$Survived=="Yes",1,0)
# titanic$alive		<-	as.numeric(titanic$Survived=="Yes")
titanic$alive   <- as.factor(titanic$alive)

# Formula to estimate
frmla <- alive ~ Class + Age + Sex

###
# package: rpart
###
library(rpart)

fitRpart	<-	rpart(frmla,method="anova",data=titanic)
?rpart
#plotcp(fitRpart)
fitRpart	# shows women and children first (except 3rd class)
summary(fitRpart)

# display the tree (not so good on my machine)
plot(fitRpart, uniform=TRUE,main="Regression Tree (rpart)")
text(fitRpart, use.n=TRUE, cex=0.8)

###
# package: tree
###
library(tree)

fitTree		<-	tree(frmla,data=titanic)

fitTree
summary(fitTree)
# display tree (again, not good)
plot(fitTree)
text(fitTree)

###
# package: ctree (from pkg: party)
###
library(party)

fitCt		<-	ctree(frmla, data=titanic)
fitCt
# display tree; looks better
plot(fitCt,main="Conditional Inference Tree (Titanic)")

###
# package: evtree
###
library(evtree)

fitEv		<-	evtree(frmla,data=titanic)
# model took MUCH longer; plot of tree is MUCH better
plot(fitEv)


sqrt(mean((predict(fitEv)-predict(fitCt))^2))
sqrt(mean((predict(fitEv)-predict(fitRpart))^2))
sqrt(mean((predict(fitEv)-predict(fitTree))^2))
sqrt(mean((predict(fitCt)-predict(fitRpart))^2))
sqrt(mean((predict(fitCt)-predict(fitTree))^2))
sqrt(mean((predict(fitRpart)-predict(fitTree))^2))


context1 <- read.csv('MROZ.csv')
summary(context1)
model1 <- ctree(inlf~educ+exper+age+kidslt6+kidsge6,data=context1)
plot(model1)

context2 <- read.csv('CRIME1.csv')
summary(context2)
model2 <- ctree(narr86~tottime+avgsen+qemp86+black+hispan,data=context2)
summary(model2)
plot(model2)

context3 <- read.csv('WAGE1.csv')
summary(context3)
model3 <- ctree(wage~educ+exper+tenure,data=context3)
plot(model3)
err = context3$wage-predict(model3)
sqrt(mean(err^2))

model4 <- lm(wage~educ+exper+tenure,data=context3)
err = context3$wage-predict(model4)
sqrt(mean(err^2))
