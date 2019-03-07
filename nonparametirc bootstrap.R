data<-read.table("/Users/yizhou/Desktop/Yi\ Zhou-101086017-a2/QC.txt",header = TRUE)

## Need to generate some "real" data from the true model
dat<-data$Tday
n<-60


## Now apply the nonparametric bootstrap
B<-10000
g<-rep(0,B)
gendat<-matrix(0,n,B)


for (i in 1:B) {
  gendat[,i]<-sample(dat,replace=TRUE)
  g[i]<-1/2*(gendat[,i][12]+gendat[,i][13])
  mean.g<-mean(g)/2
  sd.g<-sqrt(sd(g))
}
mean.g
sd.g


## Graphic summary about distribution of g
hist(g,col=grey(0.7),freq=F)
lines(density(g),col="red",lwd=2)

## Construct a 95% non-parametric bootstrap CI for g
# Definite pivot z
z<-(g-mean.g)/sd.g
q<-quantile(z,c(0.025,0.975))
ql<-min(q)
qu<-max(q)

# Compute 100(1-a)% CI for g
(gl<-mean.g-1.96*sd.g)
(gu<-mean.g+1.96*sd.g)

