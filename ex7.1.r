library(tseries)

repkpss <- function(x,test=c("Level","Trend"),dmax=5,level=0.05) {
  diff <- 0
  while (diff<=dmax) {
    suppressWarnings  (  results  <-  kpss.test(x,null="Level")  )
    if   (results$p.value>=level) 
      return(c(diff,"Level",round(results$statistic,digits=3),round(results$p.value,digits=3)))
    if (test == "Trend") {
      suppressWarnings(  results  <-  kpss.test(x,null="Trend")  )
      if (results$p.value>=level) 
        return(c(diff,"Trend",round(results$statistic,digits=3),round(results$p.value,digits=3)))
    }
    diff <- diff + 1
    x    <- diff(x)
  }
  return(c(NaN,NaN,NaN,NaN))
}


context1 <- read.csv('49_Industry_Portfolios.CSV')
head(context1)
for(j in 2:50){
  suppressWarnings(pval <- kpss.test(context1[,j],null="Level")$p.value)
  print(c(j,colnames(context1)[j],pval))
}
pca <- prcomp(context1[,2:50])
eigen <- pca$sdev^2
varplot <- sum(eigen)-cumsum(eigen) 
plot(0:10,varplot[1:11])
lines(0:10,varplot[1:11])
factors <- pca$x[,1:3]
for(j in 1:3)factors[,j] <- factors[,j]/sd(factors[,j])

ts.plot(factors[,1])
ts.plot(factors[,2])
ts.plot(factors[,3])
