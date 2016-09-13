# San

Getting Started
===============
```r
install.packages("devtools")
library(devtools)
install_github("ohdsi/SqlRender", args="--no-multiarch")
install_github("ohdsi/DatabaseConnector", args="--no-multiarch")
install_github("ohdsi/Pace", args="--no-multiarch")

library(SqlRender)
library(DatabaseConnector)
library(San)

#Data of data source (1st column: Age, 2nd column: Gender ('M' or 'F'), 3rd column: result values)
data1<-data.frame(11:15,c("M","M","F","F","F"), c(10,20,10,30,30))
data2<-data.frame(11:15,c("M","M","F","F","F"), c(10,20,10,30,30))

results<-runSan(data1, data2)
results
```