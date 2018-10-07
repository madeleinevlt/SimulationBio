estim_pi<-function(n,L,D) {
  ns=0
  for (i in seq(1,n)) {
    x1=runif(1,-0.1,D+0.1)
    random_angle=runif(1,(-pi/2),pi/2)
    x2=x1+sin(random_angle)*(L/D)
    if (x1<=0 || x1>=D || x2<=0 || x2>=D) {
      ns=ns+1
      }
  }
  return(ns/n)
}
n=20
L=4
D=10
p=estim_pi(n,L,D)
pi_d=2*(1/p)*(L/D)
pi_d

##CORRECTION
estim_pi<-function(n,r) {
  x1=runif(n,min=0,max=1)
  theta=runif(n,min=-pi/2,max=pi/2)
  x2<-x1+sin(theta)*r
  cross<-rep(0,n)
  cross[ x2 > 1 | x2<0] <- 1
  return(2*n/sum(cross)*r)
  
}

n=10000
r=0.7
nrep=10000
res<-rep(NA,nrep)
for (i in 1:nrep) {
  res[i]<-estim_pi(n,r)
}
require(MASS)

truehist(res)
abline(v=pi,col="red")
print(mean(res))
print(var(res))


mse<-sum((res-pi)*2)/nrep

mse_pi<-function(nrep,n,r) {
  res<-rep(NA,nrep)
  for (i in 1:nrep) {
    res[i]<-estim_pi(n,r)
  }
  return(sum((res-pi))*2/nrep)
}

mse_pi(1000,100,0.3)

n<-100
nrep<-10000

r_vec<-seq(from=0,1,to=1.1,by=0.1)
m <-legth(r_vec)
mse_vec<-rep(NA,m)

#for (i in 1:m) {
#  mse_vec[i]<-mse_pi
#}