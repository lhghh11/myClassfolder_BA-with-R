library(tidyverse)
library(DBI)
library(RSQLite)
library(data.table)
library(modelr)
rm(list=ls(all=TRUE))

connection <- dbConnect(SQLite(),'wooldridge.db')
dbListTables(connection)
context1 <- data.table(dbReadTable(connection,'wage1'))
dbReadTable(connection,'wage1_labels')
# index  variable.name    type format                  variable.label
# 1      0          wage float  %8.2g         average hourly earnings
# 2      1          educ  byte  %8.0g              years of education
# 3      2         exper  byte  %8.0g      years potential experience
# 4      3        tenure  byte  %8.0g     years with current employer
# 5      4      nonwhite  byte  %8.0g                  =1 if nonwhite
# 6      5        female  byte  %8.0g                    =1 if female
# 7      6       married  byte  %8.0g                   =1 if married
# 8      7        numdep  byte  %8.0g            number of dependents
# 9      8          smsa  byte  %8.0g              =1 if live in SMSA
# 10     9      northcen  byte  %8.0g =1 if live in north central U.S
# 11    10         south  byte  %8.0g   =1 if live in southern region
# 12    11          west  byte  %8.0g    =1 if live in western region
# 13    12      construc  byte  %8.0g  =1 if work in construc. indus.
# 14    13       ndurman  byte  %8.0g  =1 if in nondur. manuf. indus.
# 15    14      trcommpu  byte  %8.0g  =1 if in trans, commun, pub ut
# 16    15         trade  byte  %8.0g    =1 if in wholesale or retail
# 17    16      services  byte  %8.0g        =1 if in services indus.
# 18    17      profserv  byte  %8.0g     =1 if in prof. serv. indus.
# 19    18       profocc  byte  %8.0g    =1 if in profess. occupation
# 20    19       clerocc  byte  %8.0g    =1 if in clerical occupation
# 21    20       servocc  byte  %8.0g     =1 if in service occupation
# 22    21         lwage float  %9.0g                       log(wage)
# 23    22       expersq   int  %9.0g                         exper^2
# 24    23       tenursq   int  %9.0g                        tenure^2

model1 <- lm(log(wage)~educ+exper+tenure,data=context1)

dbDisconnect(connection)
data("mpg")
summary(mpg)
