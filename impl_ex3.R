n <- 100000; x1 <- runif(n); x2 <- runif(n)
library(MASS)
truehist(sqrt(-2*log(x1))*cos(2*pi*x2))
truehist(x1)
truehist(x2)
truehist(sqrt(-2*log(x1))*sin(2*pi*x2))



n <- 100000; x1 <- rnorm(n); x2 <- rnorm(n); x3<-rnorm(n)

truehist(x1^2 + x2^2 + x3^2)

n <- 1000
rho <- 0.7
x1 <- rnorm(n,0,1)
x2 <- rnorm(n,x1*rho,sqrt(1-rho^2))
plot(x1,x2)

##EXERCICE 3
##1.

my_rpois_one<-function(l) {
  u=runif(1)
  i=0
  while(1) {
    if (u < ppois(i,lambda = l)) {
      return(i)
    }
    i=i+1
  }
}

my_rpois<-function(n,lambda) {
  res<-rep(NA,n)
  for (i in 1:n) {
    res[i]<-my_rpois_one(lambda)
  }
  return(res)
}

par(mfrow = c(1,2))
truehist(my_rpois(100000,0.5))
truehist(rpois(100000,0.5))

lambda=2.56
n=10000
res<-my_rpois(n,lambda)
plot(table(res)/n)

ValX=seq(0,12)
ValY=dpois(ValX,lambda)
points(ValX,ValY,col="red")

##2

my_rdiscret_one<-function() {
  u=runif(1)
  sumc=cumsum(c(0.1,0.4,0.3,0.2))
  index=length(sumc)- sum(u<sumc) +1
  return(c(-3,1.3,7,15.2)[index])
}

my_rdiscret<-function(n) {
  res <- replicate(n,my_rdiscret_one())
  return(res)
}
par(mfrow = c(1,1))
truehist(my_rdiscret(10000))
plot(table(my_rdiscret(10000))/10000)

##3

my_rlaplace<-function(n) {
  res<-rep(NA,n)
  for (i in 1:n) {
    r=runif(1)
    res[i]<-my_qlaplace(r)
  }
  return(res)
}

my_dlaplace<-function(x){
  return(1/2*exp(-abs(x)))
}

my_plaplace<-function(x){
  if (x>0) {
    return(1-1/2*exp(-x))
  }
  else {
    return(1/2*exp((x)))
  }
}

my_qlaplace<-function(p){
  if (p>(1/2)) {
    return(-log(2*(1-p)))
  }
  else {
    return(log(2*p))
  }
}

par(mfrow = c(2,1))
library(rmutil)
x=seq(1:100)
res=my_dlaplace(x)
plot(x,res)
plot(x,dlaplace(x))


x=seq(1:100)
res<-rep(NA,100)
for (i in 1:100) {
  res[i]=my_plaplace(i)
}
plot(x,res)
plot(x,plaplace(x))

u=0.3
qlaplace(u)
my_qlaplace(u)

n=100000
par(mfrow = c(2,1))
truehist(my_rlaplace(n))
truehist(rlaplace(n))


## 4.

my_rnorm<-function(n){
  u=runif(n)
  x=my_rlaplace(n)
  return(x[which(u<=h_maximum(x))])
}

h_maximum<-function(x) {
  m=sqrt(2*exp(1)/pi)
  return( ((1/sqrt(2*pi)) * exp((-x**2)/2) )/(my_dlaplace(x)*m) )
}
par(mfrow = c(1,1))
x=my_rnorm(10000)
truehist(x)
points(seq(-4,4,0.1),dnorm(seq(-4,4,0.1)),type="l",col="red")
#truehist(rnorm(1000000))

## 5.
plot(seq(-5,5,0.1),f(seq(-5,5,0.1)))
curve(f, -5,10)
f<-function(x) {
  f=0.2*dnorm(x,mean=-3,sd=2) + 0.5*dnorm(x,mean=0,sd=1) + 0.3*dnorm(x,mean=5,sd=3)
}


dprop<-function(n){
  return(dunif(1))
}


rprop<-function(y,delta){
  return(runif(1,min=y-delta,max=y+delta))
}

n=100000
delta=1.5
##discuter du delta pour la qualité de la chaine
res=NA
i=0
my_rtarget<-function(n,delta,res,i) {
  while(length(res) < n) {
    y=rprop(i,delta)
    p=min(1,f(y)/f(i))
    nb=runif(1)
    if (nb<p) {
  
      res =c(res,y)
      i=y
    }
    else {
      res =c(res,i)
      i=i
    }
  }
  return(res)
}

truehist(my_rtarget(n,delta,res,i))
#points(seq(-4,4,0.1),f(seq(-4,4,0.1)),type="l",col="red")

curve(f, -10,15,add=TRUE,col="red")






