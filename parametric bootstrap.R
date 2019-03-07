data<-read.table("/Users/yizhou/Desktop/Yi\ Zhou-101086017-a2/QC.txt",header = TRUE)
## Need to generate some "real" data from the true model

dat<-data$Tday
n<-60
r<-1.6
c<-(-log(1-0.2))^(5/8)

## Compute an estimate from the data
(alphahat<-(mean(dat^r))^(1/r))

## Now apply the bootstrap
S<-10000
gendat<-matrix(0,n,S)
alphahats<-rep(0,S)
g<-rep(0,S)
for (i in 1:S) {
  gendat[,i]<-rweibull(n,r,alphahat)
  alphahats[i]<-mean((gendat[,i])^(r))^(1/r)
  g[i]<-c*alphahats[i]
  mean.g<-mean(g)
  sd.g<-sd(g)
}

mean.g
sd.g

## Graphic summary about distribution of g
hist(g,col=grey(0.7),freq=F)
lines(density(g),col="red",lwd=2)


