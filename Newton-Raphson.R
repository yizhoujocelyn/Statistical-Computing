load("/Users/yizhou/Desktop/Yi\ Zhou-101086017-a3/nonlinreg.Robj")

X<-dat[,1]
Y<-dat[,2]
plot(X,Y)

# Choose the initial  value of Beta0
xx<-seq(0,2.5,length=1000)
LogisGrow<-function(x,B0,B1,B2){
  out<-B0/(1+exp(B1+B2*x))
}
# Try until we discover the most approx inintialization value of Beta0                    
lines(xx,LogisGrow(xx,32,0,-3),col="red",lwd=2)

## Definite the NR Method
f<-function(x,y,Beta0=NULL,Beta1=NULL){
  
  # Initialize a starting value of Beta0
  if (is.null(Beta0)) {
    Beta0<-matrix(c(32,0,-3),3,1)
  }
  # Initialize a starting value of Beta1
  if (is.null(Beta1)) {
    Beta1<-Beta0+1e-8
  }
  
  
  # Initialize a starting value of H0
  H<-function(x,y,Beta){
  b0<-Beta[1,]
  b1<-Beta[2,]
  b2<-Beta[3,]
  u<-exp(b1+b2*x)
  h11<-2*sum(1/(1+u)^2)
  h12<-(-2)*sum(2*b0*u/(1+u)^3-y*u/(1+u)^2)
  h13<-(-2)*sum(2*b0*x*u/(1+u)^3-y*x*u/(1+u)^2)
  h21<-2*sum(-b0*u/(1+u)^3+x*y*u/(1+u)^2)
  h22<-2*sum(y*b0*u*(1-u)/(1+u)^3-b0^2*u*(1-2*u)/(1+u)^4)
  h23<-2*sum(y*x*b0*u*(1-u)/(1+u)^3-b0^2*x*u*(1-2*u)/(1+u)^4)
  h31<-2*sum(-b0*x*u/(1+u)^3+x*x*y*u/(1+u)^2)
  h32<-2*sum(y*x*b0*u*(1-u)/(1+u)^3-b0^2*x*u*(1-2*u)/(1+u)^4)
  h33<-2*sum(y*x^2*b0*u*(1-u)/(1+u)^3-b0^2*x^2*u*(1-2*u)/(1+u)^4)
  H<-matrix(c(h11,h12,h13,h21,h22,h23,h31,h32,h33),3,3,byrow = TRUE)
  out<-H 
  }  
  
# Gradient - First partial derivate
  g<-function(x,y,Beta){
    beta0<-Beta[1,]
    beta1<-Beta[2,]
    beta2<-Beta[3,]
    h<-exp(beta1+beta2*x)
    g0<-(-2)*sum((y-beta0/(1+h))*(1/(1+h)))
    g1<-2*sum((y-beta0/(1+h))*(beta0*h/((1+h)^2)))
    g2<-2*sum((y-beta0/(1+h))*(beta0*h*x/((1+h)^2)))
    Gradient<-matrix(c(g0,g1,g2),3,1)
    out<-Gradient
  }
  
    # Use NR Method
  for (i in 1:1000) {
    Grad<-g(x,y,Beta1)
    H0<-H(x,y,Beta1)
    Beta2<-Beta1-solve(H0)%*%Grad
    if (sum(Grad^2)<1e-8) {
      out<-NULL
      out$betahat<-Beta2
      out$i<-i
      return(out)
    }
    H0<-H0
    Beta0<-Beta1
    Beta1<-Beta2
  }
}

# Numberial Result
out<-f(X,Y)

# Output the estimated value of Beta
(out$betahat)


