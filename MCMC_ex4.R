
## EXERCICE 4 ##

n=100
x=70

##Poster
f<-function(){
  return(runif(1))
}

my_dtarget<-function(teta){
  dunif(teta)* dbinom(70,100,teta)
}

dprop<-function(y,delta){
  return(dbeta(1,y+1,n-y+1))
}


rprop<-function(y,delta){
  return(rbeta(1,y+1,n-y+1))
}


delta=1.5
##discuter du delta pour la qualité de la chaine
res=NA
i=0
my_rtarget<-function(n,delta,res,i) {
  while(length(res) < n) {
    y=rprop(i,delta)
    p=min(1,(f()*dprop(i,delta)) / (f()*dprop(y,delta)))
    print(p)
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



