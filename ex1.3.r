##############################################
## Author:   Jason Parker
## Date:     2018-04-10
## Title:    ex1.2.R
## Purpose:  Stepwise regression and the EM algorithm
##############################################

## Import packages
library(data.table)
library(DBI)
library(RSQLite)

## Prepare workspace
rm(list=ls(all=TRUE))
con <- dbConnect(SQLite(),'wooldridge.db')
context1 <- data.table(dbReadTable(con,'bwght'))
dbReadTable(con,'bwght_labels')
dbDisconnect(con)

summary(context1)
table(context1$fatheduc)
unique(context1$fatheduc)
nrow(context1)

## Try dropping fatheduc
context1 <- context1[!is.na(motheduc)]
context1$smokes    <- as.numeric(context1$cigs>0)
context2 <- context1[!is.na(fatheduc)]
model1   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context2)
summary(model1)
BIC(model1)
summary(lm(log(bwght)~cigs+male+white, data=context2))
BIC(lm(log(bwght)~cigs+male+white, data=context2))

## Step-wise regression for variable selection using BIC (backwards induction)
model2   <- step(model1,k=log(nrow(context2)))  #BIC
model2   <- step(model1)                        #AIC
summary(model2)

## EM-Algorithm for missing data
context3 <- context1
Xdata <- cbind(context1$cigs,context1$motheduc,context1$faminc,context1$male,context1$white)
blast <- rep(0,7)
for(i in 1:100) {
  model3            <- lm(log(bwght)~Xdata+fatheduc, data=context3)
  bminus            <- coef(model3)[1:6]
  b                 <- coef(model3)[7]
  pred              <- (log(context1$bwght) - (cbind(rep(1,nrow(context1)),Xdata) %*% bminus))/b
  pred              <- ifelse(pred>=1,pred,1)
  pred              <- ifelse(pred<=18,pred,18)
  pred              <- ifelse(is.na(context1$fatheduc),pred,context1$fatheduc)
  context3$fatheduc <- pred
  if(sum((coef(model3)-blast)^2)<1e-10) break
  blast <- coef(model3)
}
i
sum((coef(model3)-blast)^2)
model3   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context3)
summary(model3)
BIC(model3)

?step
model4 <- step(model3,k=log(nrow(context2)))
model4 <- step(model3)
summary(model4)

context1$smokes    <- as.numeric(context1$cigs>0)
model5 <- glm(smokes~faminc+fatheduc+motheduc,family=binomial(),data=context1)
coeftest(model5,vcov.=vcovHC)
model6 <- glm(smokes~faminc+motheduc,family=binomial(),data=context1)
coeftest(model6,vcov.=vcovHC)
library(margins)
margins(model6)
# Every 1 year increase in mother's education makes the probability of her being a smoker decrease by 2.8%
# Every $1000 increase in family income makes the probability of the mother being a smoker decrease by .24%
model7 <- glm(cigs~faminc+motheduc+fatheduc,family=poisson(),data=context1)
coeftest(model7,vcov.=vcovHC)
# Every 1 year increase in mother's education makes cigarette smoking decrease by 19%
# Every $1000 increase in family income makes cigarette smoking decrease by 1.7%

context1$smokes <- as.factor(context1$smokes)
context1$smokes <- as.numeric(context1$cigs>0)

library(party)
library(evtree)
model8 <- ctree(smokes~motheduc+faminc+fatheduc,data=context1)
model9 <- evtree(smokes~motheduc+faminc+fatheduc,data=context1)
model8
model9
plot(model8)
plot(model9)

model10 <- ctree(cigs~motheduc+faminc+fatheduc,data=context1)
model10
plot(model10)
model11 <- ctree(bwght~cigs+motheduc+faminc+fatheduc+male+white,data=context1)
model11
plot(model11)

library(dplyr)
colnames(context1)
context2 <- context1 %>% select(-index,-lbwght,-smokes,-bwghtlbs,-packs,-cigtax,-cigprice,-lfaminc)
context2 <- context2[!is.na(fatheduc)]
context2 <- context2[!is.na(motheduc)]

seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context2,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")
set.seed(seed)
model12 <- kmeans(context2,centers=3,nstart=10)
model12$centers
context2$clusters <- model12$cluster
model13   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context2[clusters==1])
model14   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context2[clusters==2])
model15   <- lm(log(bwght)~cigs+motheduc+fatheduc+faminc+male+white, data=context2[clusters==3])
summary(model13)
summary(model14)
summary(model15)
context2$group1 <- ifelse(context2$clusters==1,1,0)
context2$group2 <- ifelse(context2$clusters==2,1,0)
context2$group3 <- ifelse(context2$clusters==3,1,0)
model16 <- lm(log(bwght)~cigs+male+white+group1+group2+group3-1, data=context2)
summary(model16)
coeftest(model16,vcov.=vcovHC)

context3 <- context1 %>% select(faminc,fatheduc,motheduc,parity,male,white)
context3 <- context3[!is.na(fatheduc)]
context3 <- context3[!is.na(motheduc)]
pca <- prcomp(context3)
eigen <- pca$sdev^2
varplot <- sum(eigen)-cumsum(eigen) 
plot(0:6,varplot[1:7])
lines(0:6,varplot[1:7])
pcs <- pca$x[,1:2]
for(i in 1:2) pcs[,i] <- pcs[,i]/sd(pcs[,i])
cov(pcs)
context2$pc1 <- pcs[,1]
context2$pc2 <- pcs[,2]

model17 <- lm(log(bwght)~cigs+pc1+pc2, data=context2)
summary(model17)
# Every 1 cig smoked by the mother per day during preganancy is assocated with a .5% decrease in the birthweight controlling for the principal components from faminc,fatheduc,motheduc,parity,male, and white
# How did we/R deal with missing data in Model17? We dropped it.
