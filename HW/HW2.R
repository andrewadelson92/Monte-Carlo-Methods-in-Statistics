# problem 2
n<-1000
x<-array(dim=n)
# initialize
x[1]<-sample(0:1, 1, replace = TRUE)
for(i in 2:n){
  x[i]<-sample(c(0,1), 1, replace = TRUE, prob=c(factorial(x[i-1])*factorial(2-x[i-1])/3,factorial(1+x[i-1])*factorial(1-x[i-1])/3))
}
hist(x)

# problem 3
n<-5000
x<-array(dim=n)
x[1] <- rnorm(1, 0, 1)
rho <- 0
for(i in 2:n){
  x[i]<-rnorm(1, rho*x[i-1], sqrt(1-rho**2))
}
g <- sqrt(2*pi)/(1+x**4)
I <- sum(g)/n
I

I_N_rho <- function(n,rho,x1){
  x<-array(dim=n)
  x[1] <- x1
  for(i in 2:n){
    x[i]<-rnorm(1, rho*x[i-1], sqrt(1-rho**2))
  }
  g <- sqrt(2*pi)/(1+x**4)
  return(sum(g)/n)
}

I_N <- function(n){
  return(I_N_rho(n,rho,rnorm(1, 0, 1)))
}

var_rho <- function(rho){
  g <-array(dim=1000)
  for(i in 1:1000){
    g[i] <- I_N_rho(1000,rho,rnorm(1, 0, 1))
  }
   return(sum((g-mean(g))**2)/(1000-1))
}

n = 100:2000
results1 <- lapply(n,I_N)
plot(n, results1)

m=100
rho=seq(from=-1, to=1,length.out=m)
variance <- lapply(rho,var_rho)
plot(rho, variance, ylim=c(0.000, 0.01))