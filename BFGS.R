load("/Users/yizhou/Desktop/Yi\ Zhou-101086017-a3/nonlinreg.Robj")

X<-dat[,1]
Y<-dat[,2]
plot(X,Y)

# Choose the initial starting value of Beta0
xx<-seq(0,2.5,length=1000)
# To make sure whether graph based on Beta0 is fitted well with Logistic Growth Model based on our real data
LogisGrow<-function(x,B0,B1,B2){
  out<-B0/(1+exp(B1+B2*x))
}
# Try until we discover the most approx inintialization value of Beta0                    
lines(xx,LogisGrow(xx,32,0,-3),col="red",lwd=2)

## Definite the BFGS Method
f<-function(x,y,Beta0=NULL,Beta1=NULL){
  
  # Initialize a starting value of Beta0
  if (is.null(Beta0)) {
    Beta0<-matrix(c(32,0,-3),3,1)
  }
  # Initialize a starting value of Beta1
  if (is.null(Beta1)) {
    Beta1<-Beta0+1e-8
  }
  # Initialize a starting value of A0
  b0<-Beta0[1,]
  b1<-Beta0[2,]
  b2<-Beta0[3,]
  r<-sum((y-b0/(1+exp(b1+b2*x)))^2)
  a<-max(r,1)
  A0<-diag(a,3,3)
  # Calculate A0.I - inverse matrix of A0
  A0.I<-solve(A0)
  
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
  
  for (i in 1:1000) {
    # Inverse Fisher Information - Matrix A.I
    h<-Beta1-Beta0
    T<-g(x,y,Beta1)-g(x,y,Beta0)
    A1.I<-A0.I+drop(1/(t(T)%*%h))*((h-A0.I%*%T)%*%t(h)+h%*%t(h-A0.I%*%T))-drop(t(T)%*%(h-A0.I%*%T)/((t(T)%*%h)^2))*(h%*%t(h))
    
    # Use BFGS Method
    Grad<-g(x,y,Beta1)
    Beta2<-Beta1-A1.I%*%Grad
    if (sum(Grad^2)<1e-8) {
      out<-NULL
      out$betahat<-Beta2
      out$iFinfo<-A1.I
      out$i<-i
      return(out)
    }
    A0.I<-A1.I
    Beta0<-Beta1
    Beta1<-Beta2
  }
}

# Numberial Result
out<-f(X,Y)

# Output the estimated value of Beta
(out$betahat)

  
  