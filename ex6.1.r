###############################################################
# Title:        ex6.1.r
# Author:       Jason Parker
# Date:         2017-03-27
# Description:  K-means, hierarchical clustering and PCA
# Sources:      Various, Jay Scott, Walter Johnston
###############################################################

rm(list=ls())
library(data.table)
context1 <- read.csv("Boston.csv")
context1 <- context1[,2:15]

############################
## K-means Estimation
############################
?kmeans
seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

## Run the model
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers
groups1 <- model1$cluster
groups1



############################
## Clustering in Practice
############################
context1 <- fread("Boston.csv")
context1$cluster <- groups1

model2 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1[cluster==1])
model3 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1[cluster==2])
model4 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1[cluster==3])
model5 <- lm(log(medv)~log(nox)+rm+ptratio,data=context1)

context1$rich <- ifelse(context1$cluster==1,1,0)
context1$group2 <- ifelse(context1$cluster==2,1,0)

context1$group3 <- ifelse(context1$cluster==3,1,0)

model55 <- lm(log(medv)~log(nox)+rm+ptratio+rich+group2+group3-1,data=context1)

table(groups1)
summary(model2)
summary(model3)
summary(model4)
summary(model5) ## pooled model
summary(model55) ## fixed-effects

##############
## Hierarchical Clustering
##############

context2 <- context1[,2:15]
?hclust
hmodel1 <- hclust(dist(context2),method="average")
hmodel2 <- hclust(dist(context2),method="complete")
hmodel3 <- hclust(dist(context2),method="centroid")
?rect.hclust

plot(hmodel1)
rect.hclust(hmodel1,k=7,border="red")
rect.hclust(hmodel1,k=6,border="purple")
rect.hclust(hmodel1,k=5,border="blue")
rect.hclust(hmodel1,k=4,border="green")
rect.hclust(hmodel1,k=3,border="yellow")
rect.hclust(hmodel1,k=2,border="orange")
groups2 <- cutree(hmodel1,k=5)   #get the groups for hierarchical clustering

plot(hmodel2)
plot(hmodel3)


############################
## Dimension reduction
############################
context1 <- read.csv("Boston.csv")
Xdata <- context1[,2:14] # exclude the price
head(Xdata)

pca <- prcomp(Xdata)
screeplot(pca,type="lines") # looks like there are 2 principal components

pca$rotation[,1:2]*100 # because we have 2 prin. comp.s 

# get the principal components
factors <- pca$x[,1:2]
head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors) #mean zero, var one

# modeling with the factors
model9 <- lm(log(medv)~factors,data=context1)
summary(model9) # Note the R-squared here is not the best

# clustering with the factors
groups5 <- apply(factors^2,1,which.max)
context1$cluster <- groups5

summary(subset(context1,cluster==1))
summary(subset(context1,cluster==2))

model6 <- lm(log(medv)~log(nox)+rm+ptratio,data=subset(context1,cluster==1))
model7 <- lm(log(medv)~log(nox)+rm+ptratio,data=subset(context1,cluster==2))
summary(model6)
summary(model7)

## Clustering the variables using PCA
pca$rotation[,1:2]*1000
apply(pca$rotation[,1:2]^2,1,which.max)
model8 <- lm(log(medv)~log(nox)+rm+ptratio+b,data=context1)
summary(model8)


# algebra to standardize the principal components
library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors) #mean zero, var one

# modeling with the factors
model9 <- lm(log(medv)~factors,data=context1)
summary(model9) # Note the R-squared here is not the best

### END OF DIMENSION REDUCTION
### START OF CLUSTERING WITH PCs

# clustering with the factors
groups5 <- apply(factors^2,1,which.max)
context1$cluster <- groups5
context1$cluster

## Clustering the variables using PCA
pca$rotation[,1:2]
pca$rotation[,1:2]^2
apply(pca$rotation[,1:2]^2,1,which.max)

model10 <- lm(log(medv)~log(nox)+log(dis)+rm+ptratio+chas,data=context1)
summary(model10)
