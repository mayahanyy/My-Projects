# reading data 
x<-read.csv("winequality-red.csv", header=TRUE)
head(x)
attach(x)

library(grDevices)
pairs(x[,1:11])
quartz();
# spliting dataset into three different groups based on the score which is the quality rating of wine 
# group 1 -> df1 represents bad quality wine in which the quality rating is less than 4
df1=split(x,x$score)$'1'
head(df1)
nrow(df1) #number of observations in df1
summary(df1)
pairs(df1[,1:11])
quartz();
# group 2 -> df2 represents bad quality wine in which the quality rating is less than 6.5
df2=split(x,x$score)$'2'
head(df2)
summary(df2)
nrow(df2) #number of observations in df2
pairs(df2[,1:11])
quartz();
# group 3 -> df3 represents good quality wine in which the quality rating is greater than 6.5
df3=split(x,x$score)$'3'
head(df3)
summary(df3)
nrow(df3) #number of observations in df3
pairs(df3[,1:11])
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
#group1 
qqnorm(df1$fixed.acidity)
qqline(df1$fixed.acidity)
quartz();

qqnorm(df1$volatile.acidity)
qqline(df1$volatile.acidity)
quartz();

#group2
qqnorm(df2$citric.acid)
qqline(df2$citric.acid)
quartz();

qqnorm(df2$residual.sugar)
qqline(df2$residual.sugar)
quartz();

#group3
qqnorm(df3$chlorides)
qqline(df3$chlorides)
quartz();

qqnorm(df3$free.sulfur.dioxide)
qqline(df3$free.sulfur.dioxide)
quartz();



# normalizing data
x1=x[,1:11]
z= scale(x1, center = TRUE, scale = TRUE)
head(z)
summary(z) ## mean for all varaibles = 0
sd(z) ## 0.99 almost 1 
pairs(z)
quartz();

# Apply Fisher Linear Discriminant Analysis
library(MASS)
 flda=function(x,class) {
 cat("Fisher Linear Discriminant:\n")
 a = lda(x, class); d = predict(a)
 t=table(class, d$class); print(t)
 er=100*(sum(t)-sum(diag(t)))/nrow(x)
 cat("Error Rate =",er,"%\n")
 return(d)
}
## leave one out 
rslt=flda(z,x[,13])
loo=function(x,class){
  n=length(class)
  rslt={}
  for(i in 1:n){
     a = lda(x[-i,], class[-i])
     b = predict(a,x[i,])
     rslt[i]=b$class #[i]==class[i]
  }
return(rslt)
}

rslt=loo(z,x[,13])
cat("\nExternal validation:\n")
print(table(class,rslt))

a=lda(z,x[,13])
b = predict(a,z)
cat("\nInternal validation:")
class=score
print(table(class,b$class))
plot(b$x, pch=19, col=class)
quartz();

# Fisher Linear Discriminant Analysis 2; p = 2 and k = 2
 flda2=function(x, class) {
 if(ncol(x)!=2) {cat("Data should be 2-dimensional\n" ); return()}
t=factor(class); level=levels(t);
 if(length(level)!=2) {cat("Data should have only two groups\n" ); return()}
 y=x[class==level[1],]; x=x[class==level[2],]
 n1=nrow(x); n2=nrow(y); n=n1+n2; xcenter=colMeans(x); ycenter=colMeans(y)
 xcov=cov(x); ycov=cov(y); sp=(n1-1)*xcov+(n2-1)*ycov; sp=sp/(n-2)
 d=xcenter-ycenter; m=(xcenter+ycenter)/2; a=solve(sp)%*%d; class=c(rep(1,n1),rep(2,n2)); p=1;
 z=rbind(x,y); pred=z-matrix(m,ncol=2,nrow=n, byrow=T); pred=as.matrix(pred)
 pred=(pred%*%a)<log(p); C=(class!=pred+1); ce=sum(C)
 cat("--------------------------------------------------\n")
 cat(" Correct Incorrect\n Class Classification Classification Total\n")
 cat("--------------------------------------------------\n")
 cd1=n1-sum(C[1:n1]); cat(" 1 ",cd1," ", n1-cd1," ",n1,"\n")
 cd2=n2-sum(C[(n1+1):n]); cat(" 2 ",cd2," ", n2-cd2," ",n2,"\n")
 cat("--------------------------------------------------\n")
 cat(" Total: ",cd1+cd2," ", n-(cd1+cd2)," ",n,"\n")
 cat("--------------------------------------------------\n")
 cat("Error Rate = ",100*(ce/n),"%\n"); const=(sum(a*m)+log(p))/a[2]; slope=-a[1]/a[2]
 z=rbind(x,y); print(rbind(xcenter[1:2],ycenter[1:2]))
 plot(z[,c(1,2)],col=class,pch=19); abline(const,slope, col = 'blue');
 points(rbind(xcenter[1:2],ycenter[1:2]),pch=19,col=3,cex=1.5);
 segments(xcenter[1], xcenter[2], ycenter[1], ycenter[2],col=3)
 list(xcenter=xcenter[2:1],ycenter=ycenter[2:1],xcov=xcov,ycov=ycov,sp=sp,a=a,slope=slope,
 const=const,ce=ce,m=m,z=z) }

attach(x)
# rows include all observations of group1 and group3 
# columns include second and fifth variable 
rows=c(1:10,1383:1599); columns=c(2,5)
x1=z[rows,columns]
class=score[rows]

a=flda2(x1, class)
b=predict(a,x1) #projection 
quartz();
plot(x1,pch=19,col=class) #plot without showing projection lines
quartz();


##multinomial
library(nnet)

z=as.data.frame(z)
mn = multinom(score~., data = z)

results = predict(mn)
table(x[,13], results)


## leave one out for external validation (multinomial) 
#x_subset is the data without the class variable

library(nnet)
looMN=function(x_subset,class){
  n=length(class)
  rslt={}
  for(i in 1:n){
    y=x_subset[-i,]
    a = multinom(class[-i]~. , x_subset[-i,])
    b = predict(a,x_subset[i,])
    rslt[i]=b
  }
  return(rslt)
}

z=as.data.frame(z)
rslt=looMN(z, x[,13])
cat("Multinomial")
cat("\nExternal validation:\n")
print(table(score,rslt))
t2= table(score,rslt)
er2=100*(sum(t2)-sum(diag(t2)))/nrow(z)
cat("Error Rate =",er2,"%\n")

######
# Leave One out Cross Validation
library(MASS); library(nnet)


loo=function(x,class){
  n=length(class)
  rslt={}
  for(i in 1:n){
     a = lda(x[-i,], class[-i])
     b = predict(a,x[i,])
     rslt[i]=b$class #[i]==class[i]
  }
return(rslt)
}

rslt=loo(x,class)
cat("\nExternal validation:\n")
print(table(class,rslt))

########

# clustring
## euclidean 
d = dist(z, method = "euclidean")
hc = hclust(d, method="ward.D2")
plot(hc) # display dendrogram
clusters=cutree(hc, k=3) # cut tree into k clusters
# draw dendogram with red borders around them
rect.hclust(hc, k=3, border="red")
quartz();

###euclidean

d = dist(z, method = "euclidean")
# method:"euclidean"
hc = hclust(d, method="average")
# method: "average"
plot(hc) # display dendrogram
clusters=cutree(hc, k=3) # cut tree into k clusters
# draw dendogram with red borders around them
rect.hclust(hc, k=3, border="red")
quartz();

##euclidean
d = dist(z, method = "euclidean")
# method:"euclidean"
hc = hclust(d, method="complete")
# method: "complete"
plot(hc) # display dendrogram
clusters=cutree(hc, k=3) # cut tree into k clusters
# draw dendogram with red borders around them
rect.hclust(hc, k=3, border="red")
quartz();

##manhattan
d = dist(z, method = "manhattan")
# method:"manhattan"
hc = hclust(d, method="average")
# method: "average"
plot(hc) # display dendrogram
clusters=cutree(hc, k=3) # cut tree into k clusters
# draw dendogram with red borders around them
rect.hclust(hc, k=3, border="red")
quartz();

##manhattan
d = dist(z, method = "manhattan")
# method:"manhattan"
hc = hclust(d, method="complete")
# method: "complete"
plot(hc) # display dendrogram
clusters=cutree(hc, k=3) # cut tree into k clusters
# draw dendogram with red borders around them
rect.hclust(hc, k=3, border="red")
quartz();

##manhattan
d = dist(z, method = "manhattan")
# method:"manhattan"
hc = hclust(d, method="ward.D2")
plot(hc) # display dendrogram
clusters=cutree(hc, k=3) # cut tree into k clusters
# draw dendogram with red borders around them
rect.hclust(hc, k=3, border="red")
quartz();

##goodness of fit test
# Computes R2
R2=function(x,clusters,k=3){
n=nrow(x); tss=var(x); tss=(n-1)*sum(diag(tss));
wss=0
for(j in 1:k){
cj=x[clusters==j,]; nj=nrow(cj);
vj=var(cj); wssj=0
if(is.matrix(cj)) wssj=(nj-1)*sum(diag(vj));
wss=wss+wssj
}
r2=1-wss/tss; cat("R2 = ",r2,"\n")
return(r2)
}

r2=R2(z,3,3)

####

#k=3
#k-means
k=3; kmc = kmeans(z, k);
clusters=kmc$cluster
plot(z, pch=19, col = kmc$cluster)
points(kmc$centers, col = 1:k, pch = 8, cex=2)
points(kmc$centers, col = 1:k, pch = 19, cex=1)
clusters; table(clusters)
kmc$centers
quartz();

## L-Curve 
wss = (nrow(x1)-1)*sum(apply(x1,2,var))
for (i in 2:10) {
wss[i] = sum(kmeans(x1,centers=i)$withinss)}
plot(wss, type="b", pch=19, xlab="k",
ylab="WSS", main="The L-Curve")
quartz();
