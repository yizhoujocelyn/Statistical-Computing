
rweibull<-function(n,lambda,k) {
  u<-runif(n)
  out<-lambda*((-log(u))^(1/k))
  return(out)
}

system.time(rweibull(100000,2,1.5))

lambda<-2
k<-3
inv1<-rweibull(1000,2,3)
inv2<-rweibull(10000,2,3)
inv3<-rweibull(100000,2,3)
inv4<-rweibull(1000000,2,3)


#Graphical summary
hist(inv4,freq=F)
x<-seq(0,4,length=1000000)
lines(x,dweibull(x,k,lambda),col="blue")

#Numerical summary
mean1<-mean(inv1)
varr<-var(inv1)
meant<-lambda*gamma(1+1/k)
vart<-(lambda^2)*(gamma(1+2/k)-(gamma(1+1/k))^2)
