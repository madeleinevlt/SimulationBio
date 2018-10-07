## ----message=FALSE-------------------------------------------------------
g_support = function(scal) { return( expm1(scal)/expm1(1) ) } ##exp()-1
MSE = function(vect_res)
{
  nbRep = length(vect_res)
  soluce = (exp(2)-3)/expm1(1)
  return( sum( (vect_res-soluce)**2)/nbRep )
}

## ----message=FALSE-------------------------------------------------------
MC_NB = function(a, b, g_support, nb_points)
{
  maj = g_support(2) #Meilleur majorant: maj = g_support(2) 
  x = runif(nb_points, a, b)
  y = runif(nb_points, 0, maj)
  
  n_success = sum( y < g_support(x) )
  return(maj * (b-a) * n_success/nb_points)  
}

## ------------------------------------------------------------------------
library(MASS)
I <- (exp(2)-3)/(exp(1)-1)
nrep=100000
n=100
res1=rep(NA,nrep)

for (i in 1:nrep) {
  res1[i]=MC_NB(0,2,g_support,n)
}
mse1=(sum(res1-I)**2/nrep)
truehist(res1)
mean(res1)
var(res1)

## ----message=FALSE-------------------------------------------------------
MC_simple = function(a, b, g_support, nb_points)
{
  x_unif = runif(nb_points, a, b)
  return ( (b-a) * mean(g_support(x_unif)) )
}

## ------------------------------------------------------------------------

nrep=100000
n=100
res2=rep(NA,nrep)

for (i in 1:nrep) {
  res2[i]=MC_simple(0,2,g_support,n)
}
mse2=(sum(res2-I)**2/nrep)
truehist(res2)
mean(res2)
var(res2)

## ----message=FALSE-------------------------------------------------------
MC_beta = function(alpha, beta, k, nb_points, g_support)
{ #k pour [0, k]
  ech_beta = k * rbeta(nb_points, alpha, beta)
  f_beta = function(scal) { return( dbeta(scal/k, alpha, beta) ) }
  
  return ( k * mean(g_support(ech_beta)/f_beta(ech_beta)) )
}

## ------------------------------------------------------------------------

nrep=100000
n=100
res3=rep(NA,nrep)

for (i in 1:nrep) {
  res3[i]=MC_beta(2,1,2 ,n,g_support)
}
mse3=(sum(res3-I)**2/nrep)
truehist(res3)
mean(res3)
var(res3)

## ----message=FALSE-------------------------------------------------------
nbRep = 10000
nb_points = 200

## ------------------------------------------------------------------------
vect_res_NB = rep(0, nbRep)
vect_res_SIMPLE = rep(0, nbRep)
vect_res_BETA_2_1 = rep(0, nbRep)
vect_res_BETA_1_2 = rep(0, nbRep)

for (i in 1:nbRep)
{
  vect_res_NB[i] = MC_NB(a=0, b=2, g_support, nb_points)
  vect_res_SIMPLE[i] = MC_simple(a=0, b=2, g_support, nb_points)
  vect_res_BETA_2_1[i] = MC_beta(alpha=2, beta=1, k=2, nb_points, g_support)
  vect_res_BETA_1_2[i] = MC_beta(alpha=1, beta=2, k=2, nb_points, g_support)
}

## ----echo=T--------------------------------------------------------------
print( paste( "MOY_NB =", mean(vect_res_NB), "| MSE_NB =", MSE(vect_res_NB) ) )
print( paste( "MOY_SIMPLE =", mean(vect_res_SIMPLE), 
              "| MSE_SIMPLE =", MSE(vect_res_SIMPLE) ) )
print( paste( "MOY_BETA_2_1 =", mean(vect_res_BETA_2_1), 
              "| MSE_BETA_2_1 =", MSE(vect_res_BETA_2_1) ) )
print(paste("MOY_BETA_1_2 =", mean(vect_res_BETA_1_2), 
            "| MSE_BETA_1_2 =", MSE(vect_res_BETA_1_2)  ) )

## ------------------------------------------------------------------------
graphics.off()
par(mfrow=c(1, 3))

curve(expm1(x)/expm1(1), from=0, to=2, xname="x", 
          main="alpha=1 beta=2",
          xlab="alpha", ylab="beta")
curve(dbeta(x/2, 1, 2)/2, from=0, to=2, xname="x", add=T)

curve(expm1(x)/expm1(1), from=0, to=2, xname="x", 
          main="UNIFORME",
          xlab="alpha", ylab="beta")
abline(a=1/2, b=0)

curve(expm1(x)/expm1(1), from=0, to=2, xname="x", 
          main="alpha=2 beta=1",
          xlab="alpha", ylab="beta")
curve(dbeta(x/2, 2, 1)/2, from=0, to=2, xname="x", add=T)

## ----message=FALSE-------------------------------------------------------
MSE_BETA = function(vect_param)
{
  my_alpha = vect_param[1] ; my_beta = vect_param[2]
  nbRep = 10000
  g_support = function(scal) { return( expm1(scal)/expm1(1) ) }
  
  vect_res = rep(0, nbRep)
  
  for (rep in 1:nbRep)
  {
    vect_res[rep] = MC_beta(alpha=my_alpha, beta=my_beta, 
                            k=2, 200, g_support)  
  }
  soluce = (exp(2)-3)/expm1(1)
  return( sum( (vect_res-soluce)**2)/nbRep )
}

## ------------------------------------------------------------------------
pas = 1
range_alpha = seq(pas, 5, by=pas)
range_beta = seq(pas, 3, by=pas)
long_alpha = length(range_alpha)
long_beta = length(range_beta)

graphics.off()
par(mfrow = c(long_alpha, long_beta), mar = rep(2, 4))
for (my_alpha in range_alpha)
{
  for (my_beta in range_beta)
  {
    curve(expm1(x)/expm1(1), from=0, to=2, xname="x", 
          main=paste("alpha=", my_alpha, "beta=", my_beta),
          xlab="alpha", ylab="beta")
    curve(dbeta(x/2, my_alpha, my_beta)/2, from=0, to=2, xname="x", add=T)
  }
}

## ----main_loop-----------------------------------------------------------
system.time(
{  
  pas_beta = 0.5
  pas_alpha = 1
  range_alpha = seq(pas_alpha, 5, by=pas_alpha)
  range_beta = seq(pas_beta, 2-pas_beta, by=pas_beta)
  
  mat_res = matrix(data=0, nrow=long_alpha, ncol=long_beta)
  i = 1
  
  for (my_alpha in range_alpha)
  {
    j = 1
    for (my_beta in range_beta)
    {
      vect_param = c(my_alpha, my_beta)
      mat_res[i, j] = MSE_BETA(vect_param)
      j = j + 1
    }
    i = i + 1
  }
})

## ----message=FALSE-------------------------------------------------------
#library(rgl) #mininstall -c r r-rgl
#x = scan()

#persp3d(range_alpha, range_beta, 1/mat_res, xlab="alpha", ylab="beta")
#persp(range_alpha, range_beta, 1/mat_res,  xlab="alpha", ylab="beta",
      #ticktype="detailed")

## ----message=FALSE-------------------------------------------------------
#res_final = optim(c(2, 1), fn=MSE_BETA, method=c("BFGS"))

## ----message=FALSE-------------------------------------------------------
my_rpois_one = function (lambda)
{
  one_x_unif = runif(1)
  compt = 0
  flag = T
  
  while (flag)
  {
    if ( ppois(compt, lambda) < one_x_unif )
    {
      compt = compt + 1 #On incrémente le compteur
      
    } else { flag = F }
  } 
  
  return (compt)    
}

my_rpois = function (n, lambda)
{
  return( replicate(n, my_rpois_one(lambda)) )
}

## ------------------------------------------------------------------------
nbPoints = 10000
lambda = 10
nb_tires = my_rpois(nbPoints, lambda)

plot( table(nb_tires)/nbPoints, xlab="n", ylab="proba")
title(main="Histogramme des fréq des entiers générés",
      sub="(en rouge, la valeur attendue)", outer=F)

x_vect = 0:max(nb_tires)
y_vect = dpois( x_vect, lambda )
points(x_vect, y_vect, col="red")

## ----message=FALSE-------------------------------------------------------
my_rdiscret_one = function()
{
  support_discret = c(-3, 1.3, 7, 15.2)
  f_rep_discret = cumsum( c(0.1, 0.4, 0.3, 0.2) )
  one_x_unif = runif(1)
  
  for ( i in 1:length(support_discret) )
  {
    if (one_x_unif < f_rep_discret[i]) { return (support_discret[i]) }
  }
}


my_rdiscret = function (n)
{
  return( replicate(n, my_rdiscret_one()) )
}

## ------------------------------------------------------------------------
nbPoints = 10000
lambda = 10
nb_tires = my_rdiscret(nbPoints)
png("rdiscret")
plot( table(nb_tires)/nbPoints, xlab="Fréquence", ylab="Probabilité")
title(main="",
      sub="")

x_vect = c(-3, 1.3, 7, 15.2)
y_vect = c(0.1, 0.4, 0.3, 0.2)
points(x_vect, y_vect, col="red")
dev.off()

## ----message=FALSE-------------------------------------------------------
my_dlaplace = function(scal) { return ( exp(-abs(scal))/2 ) }

my_plaplace = function(scal)
{
  if (scal > 0) { return ( 1-exp(-scal)/2 ) }
  return ( exp(scal)/2 )
}

my_qlaplace = function()
{
  proba = runif(1)
  if (proba < 0.5) { return ( log(2*proba) ) }
  return( -log(2*(1-proba)) )
}

my_rlaplace = function(n)
{
  return( replicate(n, my_qlaplace()) )
}


## ------------------------------------------------------------------------
nbPoints = 10000
png("laplace")
truehist(my_rlaplace(nbPoints), xlim=c(-10, 10), xlab="Fréquence", ylab="Probabilité")
title(main="",
      sub="")

x_vect = seq(-10, 10, len=100)
lines(x_vect, my_dlaplace(x_vect), col="red" )
dev.off()

## ------------------------------------------------------------------------
nb_points = 10000
maj = sqrt( 2*exp(1)/pi )

my_rnorm = function(nb_points, maj)
{
  vect_x_i = my_rlaplace(nb_points)
  vect_u_i = runif(nb_points)
  h_i = dnorm(vect_x_i) / my_dlaplace(vect_x_i)
  filtre <- which( vect_u_i <= h_i/maj )
  sum(filtre)
  return ( vect_x_i[filtre] )
}


vect_x_normaux = my_rnorm(nb_points, maj)
truehist(vect_x_normaux)
x_vect = seq(-4, 4, len=100)
lines(x_vect, dnorm(x_vect), col="red" )

## ------------------------------------------------------------------------
#Comparaison des taux de rejets prévu et observé:
print( 1 - length(vect_x_normaux)/nb_points)
print( 1 - 1/maj )

## ----message=FALSE-------------------------------------------------------
my_dprop = function(scal, delta) #Ca génère la nouvelle position
{
  start = scal - delta
  end = scal + delta
  
  return ( runif(1, start, end) )
}

my_rprop = function (n) { return ( replicate(n, dprop()) ) }

my_dtarget = function (scal)
{
  return ( 0.2*dnorm(scal, -3, 2) + 0.5*dnorm(scal) + 0.3*dnorm(scal, 5, 3) )
}

my_rtarget = function (nb_steps)
{
  x_i = 10 #On s'en fout, point de départ
  vect_x_i = c(x_i, rep(0, 100))
  delta = 2
  
  for( i in 2:(nb_steps+1) )
  {
    y_i = my_dprop(x_i, delta)
    proba_decision = min( 1, my_dtarget(y_i)/my_dtarget(x_i) )
    u_decision = runif(1)
    
    if (u_decision < proba_decision) {  x_i = y_i }
    vect_x_i[i] = x_i
  }
  
  return ( vect_x_i )
}

## ------------------------------------------------------------------------
nb_steps = 10000
vect_target = my_rtarget(nb_steps)

truehist( vect_target )

x_vect = seq(-10, 15, len=nb_steps)
lines( x_vect, my_dtarget(x_vect), col="red" )

## ----message=FALSE-------------------------------------------------------
my_rbeta = function(theta) { return ( rbeta(1, 1/(1-theta), 2) ) }

my_dbeta_condition = function (theta, theta_prim)
{
  return ( dbeta(theta, 1/(1-theta_prim), 2) )
}

calc_proba_decision = function (theta, theta_prim)
{
  vraiss_theta = dbinom(70, 100, theta)
  vraiss_theta_prim = dbinom(70, 100, theta_prim)
  
  numerat = vraiss_theta_prim * my_dbeta_condition(theta, theta_prim)
  denominat = vraiss_theta * my_dbeta_condition(theta_prim, theta)
  
  return (numerat/denominat)
}

#Comme theta est sur [0;1], pi(theta) = 1 = pi(theta_prim) ??

MCMC_beta_mutation = function (nb_steps)
{
  theta = runif(1)
  vec_theta = c( theta, rep(0, nb_steps) )
  size_ech = 100
  
  for (i in 2:(nb_steps+1))
  {
    theta_prim = min( 1, my_rbeta(theta) )
    proba_decision = min( 1, calc_proba_decision(theta, theta_prim) )

    if (runif(1) < proba_decision) { theta = theta_prim }
    vec_theta[i] = theta
  }
  
  return (vec_theta)
}

## ----message=FALSE-------------------------------------------------------
simul_conjointe = function (nb_points, freq_mut_obs)
{
  vec_theta = rep(0, nb_points)
  compt_pts = 1
  
  while(compt_pts < nb_points)
  {
    theta = runif(1)
    size_ech = 100
    nb_mut_simul = rbinom(1, size_ech, theta)
    
    if ( nb_mut_simul == (freq_mut_obs*size_ech) )
    {
      vec_theta[compt_pts] = theta
      compt_pts = compt_pts + 1
    }
  }
  
  return (vec_theta[1:(compt_pts)])
}

simul_conj_one = function (freq_mut_obs)
{
  #na.omit(replicate(1000000, simul_conj_one)), met 3 secondes de plus que
  #l'autre
  
  theta = runif(1)
  size_ech = 100
  
  if ( rbinom(1, size_ech, theta) == (size_ech*freq_mut_obs) )
    return (theta)
  return(NA)
}

## ----message=FALSE-------------------------------------------------------
vec_theta_MCMC = MCMC_beta_mutation(10000)
vec_theta_bourrin = simul_conjointe(10000, 0.7)

## ----message=FALSE-------------------------------------------------------
#Par résolution analytique: pi(theta/x) --> loi bêta (x + 1, n − x + 1)

graphics.off()
par( mfrow=c(1, 2) )
truehist(vec_theta_MCMC, xlim=c(0, 1))
curve(dbeta(x, 70+1, 30+1), from=0, to=1, add=T, xname="x", col="red")
truehist(vec_theta_bourrin, xlim=c(0, 1))
curve(dbeta(x, 70+1, 30+1), from=0, to=1, add=T, xname="x", col="red")

## ----message=FALSE-------------------------------------------------------
meanS = paste("Moy MCMC =", mean(vec_theta_MCMC), 
              "| Moy bourrin =", mean(vec_theta_bourrin))
varS = paste("Variance MCMC =", var(vec_theta_MCMC), 
              "| Variance bourrin =", var(vec_theta_bourrin))
print( paste(meanS, varS, sep='\n') )

## ----message=FALSE-------------------------------------------------------
quantile(vec_theta_MCMC, probs=seq(0, 1, 0.025))

## ----message=FALSE-------------------------------------------------------
credible_interval = function(theta_obs, vec_theta_bayes, pas)
{
  flag = T
  nb_val_theta = length(vec_theta_bayes)
  i = 1
  
  while (flag)
  {
    up_born = theta_obs + i * pas
    down_born = theta_obs - i * pas
    nb_inside = sum((vec_theta_bayes > down_born) & (vec_theta_bayes < up_born))
    
    if ( nb_inside/nb_val_theta > 0.95 )
      flag = F
    
    i = i + 1
  }
  
  return ( c(down_born, up_born) )
}

## ----message=FALSE-------------------------------------------------------
credible_interval(0.7, vec_theta_MCMC, 0.001)

## ----message=FALSE-------------------------------------------------------
credible_interval(0.7, vec_theta_bourrin, 0.001)

