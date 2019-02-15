
rarwei<-function(n,k,lambda) {
  xmode<-lambda*(((k-1)/k)^(1/k))
  dweibullmax<-dweibull(xmode,k,lambda)
  u<-runif(n)
  y<-runif(n)
  accept<-(u<= (dweibull(y,k,lambda)/dweibullmax))
  return(y[accept])
}

system.time(rarwei(1000000,1.5,2 ))

lambda<-0.05
k<-1.5
AR1<-rarwei(1000,1.5,0.05)
AR2<-rarwei(10000,1.5,0.05)
AR3<-rarwei(100000,1.5,0.05)
AR4<-rarwei(1000000,1.5,0.05)


#Graphical summary
hist(AR4,freq=F)
s<-seq(0,1,length=1000)
lines(s,dweibull(s,k,lambda),col="blue",ylim=c(0,15))

#Numerical summary
meanAR<-mean(AR1)
varAR<-var(AR1)
meant<-lambda*gamma(1+1/k)
vart<-(lambda^2)*(gamma(1+2/k)-(gamma(1+1/k))^2)
