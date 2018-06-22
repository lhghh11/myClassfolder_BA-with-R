##############################################
## Author:   Jason Parker
## Date:     2017-01-30
## Title:    mc2.1.R
## Purpose:  Demonstrate endogeneity using Monte Carlo
##############################################

rm(list=ls(all=TRUE))
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(broom)
simn  <- 1000 #repetitions

##############################################
## OLS without endogeneity
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2


outp <- matrix(data=0,simn*5,3)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients
    model         <- tidy(lm(y~x))
    outp[rowcount,2]   <- model[2,2]
    outp[rowcount,3]   <- (model[2,2]-2)/model[2,3]
    outp[rowcount,1]   <- n
    rowcount <- rowcount + 1
  }
}
outp <- data.frame(outp)
colnames(outp) <- c('sample','no_endo','no_endo_t')
outp$sample <- as.factor(outp$sample)

plot_no_endo <- ggplot(outp,aes(no_endo,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_no_endo

plot_no_endo_t <- ggplot(outp,aes(no_endo_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_no_endo_t

##############################################
## OLS with an omitted variable (uncorrelated with x)
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
# new vars
b2    <- 1
zmean <- 3
zsd   <- 1.5
outp$omit_uncorr <- rep(0,simn*5)
outp$omit_uncorr_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    z             <- rnorm(n,mean=zmean,sd=zsd) #z independent of x so no problem
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + b2*z + e
    
    # estimate the ols coefficients WITHOUT INCLUDING z
    model         <- tidy(lm(y~x))
    # model         <- tidy(lm(y~x+z)) 
    outp$omit_uncorr[rowcount]   <- model[2,2]
    outp$omit_uncorr_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_omit_uncorr <- ggplot(outp,aes(omit_uncorr,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_uncorr
plot_omit_uncorr_t <- ggplot(outp,aes(omit_uncorr_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_uncorr_t

##############################################
## OLS with an omitted variable (CORRELATED with x)
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
b2    <- 1
sig   <- 1.2
xmean <- 4
xsd   <- 2
zmean <- 3
zsd   <- 1.5
xzsd  <- 1.25
outp$omit_corr <- rep(0,simn*5)
outp$omit_corr_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    w             <- rnorm(n,mean=0,sd=xzsd) #covariance between x and z
    x             <-  w + rnorm(n,mean=xmean,sd=sqrt(xsd*xsd-xzsd))
    z             <-  w + rnorm(n,mean=zmean,sd=sqrt(zsd*zsd-xzsd))
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + b2*z + e
    
    # estimate the ols coefficients WITHOUT INCLUDING z
    model         <- tidy(lm(y~x))
    # model         <- tidy(lm(y~x+z)) 
    outp$omit_corr[rowcount]   <- model[2,2]
    outp$omit_corr_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_omit_corr <- ggplot(outp,aes(omit_corr,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_corr
plot_omit_corr_t <- ggplot(outp,aes(omit_corr_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_omit_corr_t

##############################################
## OLS with reverse regression
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
outp$reverse <- rep(0,simn*5)
outp$reverse_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients BACKWARDS
    model         <- tidy(lm(x~y))
    outp$reverse[rowcount]   <- 1/model[2,2]
    outp$reverse_t[rowcount] <- (model[2,2]-0.5)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_reverse <- ggplot(outp,aes(reverse,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_reverse
plot_reverse_t <- ggplot(outp,aes(reverse_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_reverse_t

##############################################
## OLS with measurement error
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
# new vars
exsd  <- 1
eysd  <- 2
outp$meas_error <- rep(0,simn*5)
outp$meas_error_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=sig)
    y             <- b0 + b1*x + e
    
    # we don't observe x and y, we only have data on xstar and ystar
    xstar         <- x + rnorm(n,mean=0,sd=exsd)
    ystar         <- y + rnorm(n,mean=0,sd=eysd)
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(ystar~xstar))
    outp$meas_error[rowcount]   <- model[2,2]
    outp$meas_error_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_meas_error <- ggplot(outp,aes(meas_error,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_meas_error
plot_meas_error_t <- ggplot(outp,aes(meas_error_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_meas_error_t


##############################################
## Measurement error in bwght
##############################################

outp$bwght <- rep(0,simn*5)
outp$bwght_t <- rep(0,simn*5)
outp$ssize <- rep(0,simn*5)
rowcount <- 1

for (ns in 1:5) {
  if (ns == 1) {n <- 100}
  if (ns == 2) {n <- 250}
  if (ns == 3) {n <- 500}
  if (ns == 4) {n <- 1000}
  if (ns == 5) {n <- 2500}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    smokes        <- as.numeric(runif(n)>0.9)
    cigs          <- smokes*runif(n,min=0,max=9)^2
    bwght         <- cigs*-0.3911663+rnorm(n,mean=120,sd=20)
    cigs_star     <- cigs*runif(n,min=0.5,max=1)
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(bwght~cigs_star))
    outp$bwght[rowcount]   <- model[2,2]
    outp$bwght_t[rowcount] <- (model[2,2])/model[2,3]
    outp$ssize[rowcount]   <- n
    rowcount = rowcount + 1
  }
}
outp$ssize <- as.factor(outp$ssize)


plot_bwght <- ggplot(outp,aes(bwght,fill = ssize)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(-1,0)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_bwght
plot_bwght_t <- ggplot(outp,aes(bwght_t,fill = ssize)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_bwght_t
mean(outp$bwght_t)
var(outp$bwght_t)


mean(outp$bwght)
factor <- mean(outp$bwght)
factor <- (factor+0.5)/-0.5
factor <- factor + 1
-0.5/factor


##############################################
## Heteroskedasticity
##############################################



## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2

outp$hetero <- rep(0,simn*5)
outp$hetero_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  for (isim in 1:simn) {
    # create random data
    x             <- rnorm(n,mean=xmean,sd=xsd)
    e             <- rnorm(n,mean=0,sd=1)*(abs(x-xmean)/xsd)*sig
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients
    model         <- tidy(lm(y~x))
    outp$hetero[rowcount]   <- model[2,2]
    outp$hetero_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount <- rowcount + 1
  }
}

plot_hetero <- ggplot(outp,aes(hetero,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_hetero

plot_hetero_t <- ggplot(outp,aes(hetero_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_hetero_t

##############################################
## OLS with Time series
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
## New parameters
rho   <- 0.5
rhox  <- 0.5

outp$ts1 <- rep(0,simn*5)
outp$ts1_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- arima.sim(n = n, list(order = c(1,0,0), ar = c(rhox), sd = xsd)) + xmean
    e             <- arima.sim(n = n, list(order = c(1,0,0), ar = c(rho), sd = sig))
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(y~x))
    outp$ts1[rowcount]   <- model[2,2]
    outp$ts1_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_ts1 <- ggplot(outp,aes(ts1,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts1
plot_ts1_t <- ggplot(outp,aes(ts1_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts1_t

##############################################
## OLS with Unit root
##############################################

## Define the data generating process
b0    <- 3
b1    <- 2
sig   <- 1.2
xmean <- 4
xsd   <- 2
## New parameters
rho   <- 0.5
rhox  <- 0.5

outp$ts2 <- rep(0,simn*5)
outp$ts2_t <- rep(0,simn*5)
rowcount <- 1

## Simulate
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  coefs           <- rep(0,simn)
  for (isim in 1:simn) {
    # create random data
    x             <- arima.sim(n = n, list(order = c(1,1,0), ar = c(rhox), sd = xsd)) + xmean
    e             <- arima.sim(n = n, list(order = c(1,1,0), ar = c(rho), sd = sig))
    y             <- b0 + b1*x + e
    
    # estimate the ols coefficients using the measured (with error) x and y
    model         <- tidy(lm(y~x))
    outp$ts2[rowcount]   <- model[2,2]
    outp$ts2_t[rowcount] <- (model[2,2]-2)/model[2,3]
    rowcount = rowcount + 1
  }
}

plot_ts2 <- ggplot(outp,aes(ts2,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Beta position",limits=c(1.5,2.5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts2
plot_ts2_t <- ggplot(outp,aes(ts2_t,fill = sample)) +
  geom_density(position="identity",alpha=0.5) + 
  scale_x_continuous(name="Null t distribution",limits=c(-5,5)) +
  scale_y_continuous(name="Density") + 
  theme_bw() + 
  scale_colour_brewer(palette="Accent")
plot_ts2_t

#############
## Plots
#############

plot_no_endo
plot_omit_uncorr
plot_omit_corr
plot_reverse
plot_meas_error

plot_no_endo_t
plot_omit_uncorr_t
plot_omit_corr_t
plot_reverse_t
plot_meas_error_t

plot_bwght
plot_bwght_t

plot_no_endo
plot_hetero
plot_no_endo_t
plot_hetero_t

plot_no_endo
plot_ts1
plot_ts2

plot_no_endo_t
plot_ts1_t
plot_ts2_t
