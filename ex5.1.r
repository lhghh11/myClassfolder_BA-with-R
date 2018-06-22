###############################################################
# Title:        ex5.1.r
# Author:       Jason Parker
# Date:         2017-11-01
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
fitEv
