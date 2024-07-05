# reading data 
x<-read.csv("winequality-red.csv", header=TRUE)

if (!require("grDevices")) install.packages("grDevices")
library(grDevices)
# 
pairs(x[,1:11])
quartz();
# checking normality 
# Load the required libraries
library(MASS)
library(ggplot2)

# Plotting pairs.panel 

if (!require("psych")) install.packages("psych")
library(psych)
pairs.panels(x[,1:11])
quartz();

# Plotting the QQ plot
qqnorm(x$fixed.acidity)
qqline(x$fixed.acidity)
quartz();

qqnorm(x$volatile.acidity)
qqline(x$volatile.acidity)
quartz();


qqnorm(x$citric.acid)
qqline(x$citric.acid)
quartz();

qqnorm(x$residual.sugar)
qqline(x$residual.sugar)
quartz();

# scaling the data 
z=scale(x, center = TRUE, scale = TRUE)
# using only numeric values so we can procced with PCA
z=z[,1:11] 
z=as.matrix(z)
summary(z) # mean of all variables is equal to zero
sd(z) # standard deviation of z is 0.999 approximately 1

pairs(z)
quartz();

# correlation matrix
if (!require("ggcorrplot")) install.packages("ggcorrplot")
library(ggcorrplot)
corr_matrix <- cor(z)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)
quartz();

# loading required libraries 
library(robustX) 
library(MASS)
library(grDevices)

## Classical PCA
pc=princomp(z, cor=T);
summary(pc,loadings=T)
plot(pc) # Scree plot
quartz();
v=pc$sd^2; sv=sum(v); cs=cumsum(v)/sv; 
m=length(cs[cs<0.95]) 
if(m>1) pairs(pc$scores[,1:m]);
# Outputs: pc$scores; pc$sd; pc$loadings

# pairs.panels plot 
pairs.panels(pc$scores[,1:m],
             gap=0,
             bg = c("red", "yellow", "blue")[x$quality],
             pch=19)
# Scree plot with percentages 
if (!require("factoextra")) install.packages("factoextra")
library(factoextra)

fviz_eig(pc, addlabels = TRUE)
quartz();



## Robust PCA
##BACON 
b=mvBACON(z)
b

#plotting 
y = cbind(1:1599,b$dis)
colnames(y) <- c("Index","Distance"); quartz();
plot(y, pch=19, main = "BACON Distances: Red Wine Quality Data (n=1599)")
abline(h=b$limit, col= "red", lty=2)
points(y[ ! b$subset, ], pch = 4, col = 2, cex = 1.5)

# identifying points on the graph to detect outliers 
identify(1:1599,b$dis,labels=1:1599)
quartz();

## Robust PCA 

d=diag(b$cov)^-0.5;
D=diag(d);
R=D%*%b$cov%*%D
pcr=princomp(z,covmat=R)
summary(pcr,loadings=T)
plot(pc) # Scree plot
quartz();

# Scree plot with percentages 
library(factoextra)
fviz_eig(pc, addlabels = TRUE)
quartz();




