########################################################################################################
## Author:   Jason Parker
## Date:     2017-08-28
## Title:    mc1.1.R
## Purpose:  Demonstrate synthetic data, control structures, and Monte Carlo
########################################################################################################

## Drop everything
rm(list=ls(all=TRUE))

## Set a default sample size
n <- 200


########################################################################################################
## Synthetic data
########################################################################################################

## Uniform distribution
runif(1)
runif(10)
z <- runif(n)
mean(z)
min(z)
max(z)
?runif

z <- runif(10,min=1,max=3)
z
c(min(z),max(z))
var(runif(10*n))
1/12  # Variance of uniform distribution from wikipedia


## Normal distribution
rnorm(10)
?rnorm
rnorm(10,mean=5,sd=0.5)
x <- rnorm(n,mean=5,sd=0.5)
c(mean(x),sqrt(var(x)))





########################################################################################################
## Control Structures
########################################################################################################

## User-defined function (runs as block)
std <- function(w){
  return(sqrt(var(w)))
}
c(mean(x),std(x))

## If/else statements
p <- 0#1
if (p == 1) {
  x <- rnorm(n)
} else {
  x <- runif(n)
}
mean(x)

## Loops
x <- runif(n)
meanx <- 0
for(i in 1:n) 
{
  meanx <- meanx + x[i]
}
meanx <- meanx/n
meanx

## Loop time delay demonstration
n = 100000
x <- runif(n)
t0 <- Sys.time()
  meanx <- 0
  for(i in 1:n) {
    meanx <- meanx + x[i]
  }
  meanx <- meanx/n
Sys.time() - t0
t0 <- Sys.time()
  mx <- mean(x)
Sys.time() - t0
c(meanx,mx)  # same answer, but takes an order of magnitude longer



########################################################################################################
## Monte Carlo
########################################################################################################


## Monte Carlo simulation x1
simn  <- 1000
n     <- 200
mx    <- rep(0,simn)
for (isim in 1:simn) {
  x         <- rnorm(n,mean=5,sd=0.5)
  mx[isim]  <- mean(x)
}
hist(mx) # mean around 5, small stdev
mean(mx) 
std(mx) # calculate stdev

## Monte Carlo simulation x5
simn  <- 1000
outp  <- matrix(data=0,nrow=5,ncol=3)
for (ns in 1:5) {
  if (ns == 1) {n <- 10}
  if (ns == 2) {n <- 25}
  if (ns == 3) {n <- 50}
  if (ns == 4) {n <- 100}
  if (ns == 5) {n <- 250}
  mx          <- rep(0,simn)
  for (isim in 1:simn) {
    x         <- rnorm(n,mean=5,sd=0.5)
    mx[isim]  <- mean(x)
  }
  outp[ns,]   <- c(n,mean(mx),std(mx))
  #plot histogram (distribution of sample means)
  #  set limits around 4.5 to 5.5 for consistent estimation
  hist(mx,xlim=c(4.5,5.5))
  #pause
  invisible(readline(prompt="Press [enter] to continue"))
}
outp ## Note that the standard deviation is converging to 0 as n goes to infinity


##############################################
## Distribution of the sample mean for normal data
##############################################

outp  <- matrix(data=0,nrow=5*10*11,ncol=5)
rowcount <- 1
for (ns in 1:5) {  #change n
  for (ms in -5:5) { #change popn mean
    for (sds in 1:10) { #change popn sd
      pmean <- ms #popn mean =  -5,-4,...,5
      psd <- sds/10 #popn sd = 0.1,0.2,...,1.0
      if (ns == 1) {n <- 10}
      if (ns == 2) {n <- 25}
      if (ns == 3) {n <- 50}
      if (ns == 4) {n <- 100}
      if (ns == 5) {n <- 250}
      mx              <- rep(0,simn)
      for (isim in 1:simn) {
        x             <- rnorm(n,mean=pmean,sd=psd)
        mx[isim]      <- mean(x)
      }
      #save information about the distribution of the sample mean
      outp[rowcount,] <- c(n,pmean,psd,mean(mx),var(mx))
      rowcount        <- rowcount + 1
    }
  }
}
head(outp)

context     <- data.frame(outp)
colnames(context) <- c('sample','pmean','psd','m_xbar','var_xbar')
head(context)

model1 <- lm(m_xbar~       sample+pmean+psd,    data=context)
summary(model1)
model1 <- lm(m_xbar~       pmean,               data=context)
summary(model1) #R-squared close to 1 means the model is a good fit
model2 <- lm(var_xbar~     sample+pmean+psd,    data=context)
summary(model2) 
model2 <- lm(log(var_xbar)~log(sample)+log(psd),data=context)
summary(model2) #R-squared close to 1 means the model is a good fit

#m_xbar        = pmean
#log(var_xbar) = -1*log(n) + 2*log(psd)
#log(var_xbar) = log(pop_var/n)
#var_xbar      = pop_var/n

##############################################
## Central limit theorem
##############################################

simn <- 100000

plot(density(rnorm(simn)),xlim=c(-2.5,2.5))

for (ns in 1:5) {
  if (ns == 1) {n <- 1}
  if (ns == 2) {n <- 2}
  if (ns == 3) {n <- 3}
  if (ns == 4) {n <- 5}
  if (ns == 5) {n <- 10}
  mx_spec         <- rep(0,simn)
  for (isim in 1:simn) {
    x             <- runif(n,min=-1,max=1)
    #mx_spec is the standardized sample mean (mean=0,sd=1)
    #i.e., just the mean times some stuff because the popn mean
    #  is zero
    mx_spec[isim] <- mean(x)*sqrt(n)*sqrt(3) 
  }
  plot(density(mx_spec),xlim=c(-2.5,2.5))
  invisible(readline(prompt="Press [enter] to continue"))
} # Sample mean follows a normal distribution as n => infty

##############################################
## Hypothesis testing
##############################################

simn <- 1000
outp <- matrix(data=0,nrow=5,ncol=2)
mu   <- 0  #Ho:mu==0; Ha:mu!=0
# mu   <- 0.5 #Ho:mu==0.5; Ha:mu!=0.5
for (ns in 1:5) {
  if (ns == 1) {n <- 3}
  if (ns == 2) {n <- 5}
  if (ns == 3) {n <- 10}
  if (ns == 4) {n <- 25}
  if (ns == 5) {n <- 50}
  rejections      <- 0
  for (isim in 1:simn) {
    x             <- runif(n,min=-1,max=1)
    mx            <- mean(x)
    sterr         <- sqrt(var(x)/n) #estimate for st. dev. of mx
    #t_stat is just mx_spec from above
    t_stat        <- (mx-mu)/sterr
    #use the normal distribution b/c it works as n->infty
    if(abs(t_stat)>1.96) {rejections <- rejections + 1}
  }
  outp[ns,]       <- c(n,rejections/simn)
}
outp 
# When null is true, rejection rate => 0.05 as n => infty
#   We call this rejection rate the "size" of the test
# When null is false, rejection rate => 1 as n => infty
#   We call this rejection rate the "power" of the test