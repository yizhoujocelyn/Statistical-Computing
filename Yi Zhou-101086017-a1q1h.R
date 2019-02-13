k<-1.5
lambda<-0.05


f<-function(x){
  sqrt((x^2)*dweibull(x,k,lambda))
}
xmode<-lambda*(((k-1)/k)^(1/k))
ru<-sqrt(dweibull(xmode,k,lambda))
rv<-optimize(f,interval=c(0, 1),maximum=TRUE)


rouweibull<-function(n) {
  u<-runif(n,0,ru)
  v<-runif(n,0,rv$objective)
  y<-v/u
  index<-u<=sqrt(dweibull(y,k,lambda))
  out<-y[index]
  out
}

system.time(rouweibull(100000))


OU1<-rouweibull(1000)
OU2<-rouweibull(10000)
OU3<-rouweibull(100000)
OU4<-rouweibull(1000000)

#Graphical summary
hist(OU4,freq=F)
s<-seq(0,0.3,length=1000)
lines(s,dweibull(s,k,lambda),col="blue",ylim=c(0,15))

#Numerical summary
meanOU1<-mean(OU1)
varOU1<-var(OU1)
meant<-lambda*gamma(1+1/k)
vart<-(lambda^2)*(gamma(1+2/k)-(gamma(1+1/k))^2)
