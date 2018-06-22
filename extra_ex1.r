
rm(list=ls())
library(data.table)
library(tseries)

context1 <- fread('phillips.csv',header=TRUE)
# variable name   type    format     label      variable label
# year            int     %9.0g                 1948 through 2003
# unem            float   %9.0g                 civilian unemployment rate, %
# inf             float   %9.0g                 percentage change in CPI
# inf_1           float   %9.0g                 inf[_n-1]
# unem_1          float   %9.0g                 unem[_n-1]
# cinf            float   %9.0g                 inf - inf_1
# cunem           float   %9.0g                 unem - unem_1
summary(context1)

model1 <- lm(unem~inf+year,data=context1)
summary(model1)

kpss.test(context1$unem,null="Level")
kpss.test(context1$unem,null="Trend")
kpss.test(diff(context1$unem),null="Level") 
# unemployment rate is level stationary after first-differencing
# kpss.test(diff(context1$unem),null="Trend")

kpss.test(context1$inf,null="Level")
# %change in cpi is level stationary without differencing

context1$inf <- context1$inf/100
context1$unem <- context1$unem/100

model2 <- lm(diff(log(unem))~inf[2:56],data=context1)
summary(model2)
coeftest(model2,vcov=NeweyWest(model2,lag=5))

# a 10% increase in the CPI is associated with a 3.2% 
#     increase in the unemployment rate