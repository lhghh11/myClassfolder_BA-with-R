###############################################################
# Title:        ex6.2.r
# Author:       Jason Parker
# Date:         2018-04-03
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

context1$group1   <- ifelse(context1$cluster==1,1,0)
context1$group2 <- ifelse(context1$cluster==2,1,0)
context1$group3 <- ifelse(context1$cluster==3,1,0)

model52 <- lm(log(medv)~log(nox)+rm+ptratio+group1+group2+group3-1,data=context1)
model55 <- lm(log(medv)~log(nox):group1+log(nox):group2+log(nox):group3+rm+ptratio+group1+group2+group3-1,data=context1)

table(groups1)
summary(model2)
summary(model3)
summary(model4)
summary(model5) ## pooled model
summary(model52) ## fixed-effects
summary(model55) ## fixed-effects + different slopes for groups

##############
## Hierarchical Clustering
##############

context2 <- context1[,2:15]
?hclust
hmodel1 <- hclust(dist(context2),method="average") # Mean distance
hmodel2 <- hclust(dist(context2),method="complete") # Maximum distance
hmodel3 <- hclust(dist(context2),method="centroid") # Median distance
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
eigen <- pca$sdev^2
varplot <- sum(eigen)-cumsum(eigen) 

plot(0:10,varplot[1:11])
lines(0:10,varplot[1:11]) # looks like there are 1 principal components
## SHOULD USE 1 PCA, IM GOING TO USE 4 TO SHOW THE METHOD

pca$rotation[,1:4]*100 # because we have 4 prin. comp.s 

# get the principal components
factors <- pca$x[,1:4]  # z variables
head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
for(j in 1:4)factors[,j] <- factors[,j]/sd(factors[,j])
cov(factors) #mean zero, var one

# modeling with the factors
model9 <- lm(log(medv)~factors,data=context1)
summary(model9) # Note the R-squared here is not the best


### Switch to 1 Principal Component (which is what you should do!)
pca$rotation[,1]*100 # because we have 4 prin. comp.s 

# get the principal components
factors <- pca$x[,1]
head(factors)
summary(factors)

# algebra to standardize the principal components
factors <- factors/sd(factors)
var(factors)

# modeling with the factors
model11 <- lm(log(medv)~factors,data=context1)
summary(model11) # Note the R-squared here is not the best
BIC(model11)
BIC(model5)
model57 <- lm(log(medv)~log(nox)+rm+ptratio+factors,data=context1)
BIC(model57)


### END OF DIMENSION REDUCTION
### START OF CLUSTERING WITH PCs

# clustering with the factors
factors <- pca$x[,1:4]  # z variables   
for(j in 1:4)factors[,j] <- factors[,j]/sd(factors[,j])
groups5 <- apply(factors^2,1,which.max)
context1$cluster <- groups5
context1$cluster

## Clustering the variables using PCA
pca$rotation[,1:4]^2
apply(pca$rotation[,1:4]^2,1,which.max)

model10 <- lm(log(medv)~log(nox)+log(dis)+rm+ptratio+chas,data=context1)
summary(model10)



