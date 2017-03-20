# problem 1
n<-1000
a<-array(dim=n)
b<-array(dim=n)
# initialize
a[1]<-1
b[1]<-1
# MCMC
for(i in 2:n){
  b[i]<-rgamma(1,shape=7,scale=1/(1+3*a[i-1]))
  a[i]<-rgamma(1,shape=5,scale=1/(1+3*b[i]))
}
I <- sum(a*b)/n
I
# integral vs n
I_N<- function(n){
  a<-array(dim=n)
  b<-array(dim=n)
  # initialize
  a[1]<-1
  b[1]<-1
  # MCMC
  for(i in 2:n){
    b[i]<-rgamma(1,shape=7,scale=1/(1+3*a[i-1]))
    a[i]<-rgamma(1,shape=5,scale=1/(1+3*b[i]))
  }
  return(sum(a*b)/n)
}

n = seq.int(100,5000,5)
results1 <- lapply(n,I_N)
plot(n, results1)

# problem 2
n<-1000
# pi(x)
pi <- function(x){
  return(exp(-1/2*x**2)/(1+x**2))
}
# normalized density
f <- function(x){
  result=integrate(pi,-Inf,Inf)$val
  return(pi(x)/result)
}
# q(x1|x2)
q <- function(x1,x2,sigma){
  return(dnorm(x1,-x2,sigma))
}
# alpha(x1,x2)
alpha <- function(x1,x2,sigma){
  return(min(1,pi(x1)*q(x2,x1,sigma)/(pi(x2)*q(x1,x2,sigma))))
}

x<-array(dim=n)
u<-array(dim=n)
x[1]=-0.01
u[1]=0.1
sigma=1
for(i in 2:n){
  x[i]<-rnorm(1,-x[i-1],sigma)
  u[i]<-runif(1,0,1)
  if(u[i]>=alpha(x[i],x[i-1],sigma)){
    x[i]=x[i-1]
  }
}
I <- sum(x)/n
I
hist(x,freq=FALSE)
xfit<-seq(min(x),max(x),length=40) 
yfit<-lapply(xfit,f)
lines(xfit, yfit, col="blue")

# problem 3
n<-1000
# pi(x)
pi <- function(x){
  a<-1
  m<-2
  return(exp(a*x)*exp(-m*exp(x))*exp(-1/2*x**2))
}
# normalized density
library(cubature)
f <- function(x){
  result=integrate(pi,-50,50)$val
  return(pi(x)/result)
}
# q(x1,x2)
q <- function(x1,x2){
  #return(dnorm(x1,x2,1))
  return(dnorm(x1,0,1))
}
# alpha(x1,x2)
alpha <- function(x1,x2){
  return(min(1,pi(x1)*q(x2,x1)/(pi(x2)*q(x1,x2))))
}

x<-array(dim=n)
u<-array(dim=n)
x[1]=0
u[1]=0.1
for(i in 2:n){
  x[i]<-rnorm(1,0,1)
  u[i]<-runif(1,0,1)
  if(u[i]>=alpha(x[i],x[i-1])){
    x[i]=x[i-1]
  }
}
I <- sum(x)/n
I
hist(x,freq=FALSE)
xfit<-seq(min(x),max(x),length=40) 
yfit<-sapply(xfit,f)
lines(xfit, yfit, col="blue")