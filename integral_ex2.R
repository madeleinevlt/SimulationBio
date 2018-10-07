##mail : bruno.toupance@mnhn.fr

g<-function(x) {
  return((exp(x)-1) / (exp(1) -1))
}


valX<-seq(from=0,to=2,len=1000)
valY<-g(valX)
plot(valX,valY,xlab="a",ylab="g(x)")

I <- (exp(2)-3)/(exp(1)-1)
print(I)
print(integrate(g,0,2))

m=g(2)
n=100
u=runif(n,0,2)
v=runif(n,0,m)
ns=sum(v < g(u))
print(ns)
I=m*(2-0)*(ns/n)


MC_EC<-function(n) {
  x<-runif(n,0,2)
  h<-g(x) / dunif(x,0,2) ## loi de densité sur les valeurs x
  return(mean(h))
}
## f densité = 1/b-a si x[a,b]
I=(2-0)*mean(g(x))

nrep=100000
n=100
res2=rep(NA,nrep)

for (i in 1:nrep) {
  res2[i]=MC_EC(n)
}
mse2=(sum(res2-I)**2/nrep)
truehist(res2)
mean(res2)
var(res2)

MC_Beta<-function(n) {
  x=2*rbeta(n,2,1)
  h<-g(x) / (dbeta(x/2,2,1)/2) ## loi de densité sur les valeurs de x biaisé (x/2 pour revenir à 0-1
  return(mean(h))
}

nrep=100000
n=100
res3=rep(NA,nrep)

for (i in 1:nrep) {
  res3[i]=MC_Beta(n)
}
mse3=(sum(res3-I)**2/nrep)
truehist(res3)
mean(res3)
var(res3)


###BEST ALPHA BETA
MC_Beta<-function(n,alpha,beta) {
  x=2*rbeta(n,alpha,beta)
  h<-g(x) / (dbeta(x/2,alpha,beta)/2) ## loi de densité sur les valeurs de x biaisé (x/2 pour revenir à 0-1
  return(mean(h))
}

nrep=1000
n=100
alpha=1
beta=1
msemax=1
for (j in seq(1,10,0.01)) {
  if (j<5) {
    alpha=j
  }
  if (j>5) {
    alpha=1
    beta=j-5 +1
  }
  res4=rep(NA,nrep)
  for (i in 1:nrep) {
    res4[i]=MC_Beta(n,alpha,beta)
  }
  mse4=(sum(res4-I)**2/nrep)
  cat(alpha,beta,mse4)
  print("separator")
  if (mse4<msemax) {
    msemax=mse4
    alpha_max=alpha
    beta_max=beta
    }
}
print(msemax)
print(alpha_max)
print(beta_max)

res4=rep(NA,nrep)
for (i in 1:nrep) {
  res4[i]=MC_Beta(n,1.5,1)
}

mse4=(sum(res4-I)**2/nrep)
###
##CORRECTION


MC_BW<-function(n) {
  u<-runif(n,0,2)
  m<-g(2)
  v<-runif(n,0,m)
  ns<-sum(v < g(u))
  return(m*(2-0)*(ns/n))
}

nrep=100000
n=100
res1=rep(NA,nrep)

for (i in 1:nrep) {
  res1[i]=MC_EC(n)
}
mse1=(sum(res1-I)**2/nrep)
truehist(res1)
mean(res1)
var(res1)

##regarder les densités pour comprendre
