library(doParallel)
library(foreach)
detectCores()

cl <- makeCluster(4)
# Register cluster
registerDoParallel(cl)
#Find out how many

Nit=50000
ptm <- proc.time()
MCMC <- foreach(i=1:4, .combine=cbind,.export=c("Densevalm22")) %dopar% {
  ypo=matrix(0,nrow=nrow(wq)+9,ncol=Nit)
  
  t_old=as.matrix(t_m)
  Dens<-Densevalm22(t_old,RC)
  p_old=Dens$p
  ypo_old=Dens$ypo
  
  for(j in 1:Nit){
    t_new=t_old+solve(t(LH),rnorm(9,0,1))
    Densnew<-Densevalm22(t_new,RC)
    ypo_new=Densnew$ypo
    p_new=Densnew$p
    logR=p_new-p_old
    
    if (logR>log(runif(1))){
      t_old=t_new
      p_old=p_new
      ypo_old=ypo_new
      
    }
    ypo[,j]=rbind(ypo_old,t_old)
  }
  
  seq=seq(2000,Nit,5)
  ypo=ypo[,seq]
  
  return(ypo)
}
ypo=head(MCMC,nrow(MCMC)-9)
t=tail(MCMC,9)
proc.time()-ptm

