# Applied Multivariate Analysis Project 

#reading dataset 
x<-read.csv("winequality-red.csv", header=TRUE)
head(x)
str(x)
class(x)
summary(x)

#checking if there are any 'Na's' in the data
sum(is.na(x))

# displaying relationship between variables 
pairs(x[,1:12])

# spliting dataset into two different groups based on the score which is the quality rating of wine 
# group 1 -> df1 represents good quality wine in which the quality rating is greater than 6.5
df1=split(x,x$score)$'1'
head(df1)
nrow(df1) #number of observations in df1
summary(df1)
pairs(df1[,1:12])

# group 2 -> df2 represents bad quality wine in which the quality rating is less than 6.5
df2=split(x,x$score)$'0'
head(df2)
summary(df2)
nrow(df2) #number of observations in df2
pairs(df2[,1:12])

# installing library robustX 
if(!require("robustX"))
install.packages("robustX")
# loading library 
library(robustX)

# using mvBACON on first group df1 excluding the last variable "score"

df1=as.matrix(df1)
output= mvBACON(df1[,1:11])
output
#plotting 
y = cbind(1:217,output$dis)
colnames(y) <- c("Index","Distance"); quartz();
plot(y, pch=19, main = "BACON Distances: Red Wine Quality Data (n=217)")
abline(h=output$limit, col= "red", lty=2)
points(y[ ! output$subset, ], pch = 4, col = 2, cex = 1.5)

# identifying points on the graph to detect outliers 
identify(1:217,output$dis,labels=1:217)

# using mvBACON on second group df2 excluding the last variable "score"
df2=as.matrix(df2)
output= mvBACON(df2[,1:11])
output
#plotting
y = cbind(1:1382,output$dis)
colnames(y) <- c("Index","Distance"); quartz();
plot(y, pch=19, main = "BACON Distances: Red Wine Quality Data (n=1382)")
abline(h=output$limit, col= "red", lty=2)
points(y[ ! output$subset, ], pch = 4, col = 2, cex = 1.5)

# indentifying points on the graph to detect outliers 
identify(1:1382,output$dis,labels=1:1382)

# Hotelling’s T-Square Test

if(!require("DescTools"))
install.packages("DescTools")
# loading library 
library(DescTools)
#Classical (non-robust) Hotelling T2 test
out1=HotellingsT2Test(df1[,1:11],df2[,1:11])
out1

#A robust version Hotelling T2 test
if(!require("rrcov"))
install.packages("rrcov")
# loading library 
library(rrcov)

out2=T2.test(df1[,1:11],df2[,1:11])
out2
