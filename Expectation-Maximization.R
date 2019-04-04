#-------------- EM Algorithm for Normal Mixture Model------------------

NormMixEM<-function(Y,clusters,tol,maxits,showits=TRUE){
  
  
  # Initialize iteration count
  iter.count<-0
  
  # Initialize starting values of theta
  prob<-matrix(c(0.1,0.9),2,1)
  mu<-matrix(c(0.1,0.5),2,1)
  var<-matrix(c(0.1,0.5),2,1)
  
  # Initialize probability of cluster membership for each observation i
  n<-nrow(Y)
  wi<-matrix(1,n,clusters)
  
  # Initialize convergence
  converged<-FALSE
  # Show iterations if showits == true  
  if (showits)     
    cat(paste("Iterations of EM:", "\n"))
  while(!converged & iter.count<maxits){ 
    prob.old<-prob
    wi.old<-wi
    
    #------------------------------E-Step----------------------------------
    #--------------------Compute responsibilities wi-----------------------
    for (j in 1:clusters){
      wi[,j]<-prob[j,]*dnorm(Y,mu[j,],sqrt(var[j,]),log=FALSE) 
    }
    wi<-wi/rowSums(wi)
    
    
    #-------------------------------M-Step----------------------------------
    #---------------------Update the value of theta-------------------------
    
    for (j in 1:clusters){
      varmat<-matrix(0, ncol=ncol(Y), nrow=ncol(Y))
      for (i in 1:n){
        varmat<-varmat+wi[i,j]*Y[i,]*Y[i,]
      }
      mu[j,]<-(t(wi[,j]) %*% Y)/sum(wi[,j])
      var[j,]<-varmat/sum(wi[,j])-t(mu[j,])%*%mu[j,]
      # Update p
      prob[j,]<-sum(wi[,j])/n
    }
    
    # Create cluster membership
    cluster<-which(round(wi)==1,arr.ind=TRUE)
    # Order accoring to row rather than cluster
    cluster<-cluster[order(cluster[,1]),2]
    
    
    ## Compare old to new for convergence
    parameter.old<-c(prob.old)
    parameter.new<-c(prob)
    iter.count<-iter.count+1
    if (showits&iter.count==1|iter.count%%5==0)
      cat(paste(format(iter.count),"...","\n",sep=""))
    converged<-min(abs(parameter.old-parameter.new))<tol
  }
  
  out<-list(prob=prob,mu=mu,var=var,wi=wi,cluster=cluster)
}

load("/Users/yizhou/Desktop/Yi\ Zhou-101086017-a4/a4q1e.Robj")
Y<-data.matrix(dat)

## Run and get theta
theta<-NormMixEM(Y,clusters=2,tol=1e-8,maxits=1000)

## Numerical Result
mu1<-theta$mu[1,]
mu2<-theta$mu[2,]
var1<-theta$var[1,]
var2<-theta$var[2,]
p<-theta$prob[1,]

(matrix(c(mu1,mu2,var1,var2,p),5,1))

