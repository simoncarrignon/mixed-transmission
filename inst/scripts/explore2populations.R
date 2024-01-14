devtools::load_all(".")
z=2
neutraltraitsParam=initNeutralTraitsPathways(z = z)
neutraltraitsParam$post[,"i"]=1
neutraltraitsParam$s=c(0,1)
traitsid=paste0("t",1:z)
km=0
ki=z
G=2
initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=1,km=1,n=3),coordinates=random2Dgrid(K=ki+km,Gx=G),G=G,sizes=150,plot=T)
initcomus$occupation[initcomus$occupation==0]=1
communities=unlist(lapply(seq_along(initcomus$size),function(i)rep(i,initcomus$size[i])))
data(population)
population=population[population[,"community"]%in% c(1,2),]
population[,traitsid]=0
for(i in 1:z){population[population[,"community"]==i,traitsid[i]]=1}
knitr::kable(aggregate(population[,traitsid],by=list(population[,"community"]),sum))
population=population[,-c(12,13)]
tstep=10000
initcomus$size=unname(table(population[,"community"]))

a=Sys.time()
quickV=modelVector(K=ki, m=1, b=0.2, r=0.1, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus","popsize"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = 150)
print(Sys.time()-a)

tstep=10000
rhos=c(0,.5,1)
repli=30
library(parallel)
cl <- makeCluster(30,type="FORK",outfile="log.txt")
rhodepSBBIB=lapply(rhos,function(rho){
           neutraltraitsParam$post[,]=0
           neutraltraitsParam$pre[,]=0
           neutraltraitsParam$post[,"i"]=1
           allinlaw=parSapply(cl,1:repli,function(r)
                              {
                                  set.seed(as.numeric(Sys.time())+r)
                                  rho=rhos[1]
                                  quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=rho, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("traitsumary","finalpop","finalcomus","popsize"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = 150,vidfile=NULL)
                                  pop=quickV$population
                                  do.call("rbind",quickV$traitsumary)/quickV$popsize
                              },simplify=F)
           neutraltraitsParam$post[,"i"]=0
           neutraltraitsParam$post[,"h"]=1
           allposthoriz=parSapply(cl,1:repli,function(r){
                                  set.seed(as.numeric(Sys.time())+r)
                                      quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=rho, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("traitsumary","finalpop","finalcomus","popsize"),logging=c("done","visu"),ma=1,traitsid=paste0("t",1:z),F_Th = 150,vidfile=NULL,beta=0)
                                      pop=quickV$population
                                     do.call("rbind",quickV$traitsumary)/quickV$popsize
                              },simplify=F)


           neutraltraitsParam$post[,"h"]=0
           neutraltraitsParam$pre[,"h"]=1
           allprehoriz=parSapply(cl,1:repli,function(r){
                                  set.seed(as.numeric(Sys.time())+r)
                                     quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=rho, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("traitsumary","finalpop","finalcomus","popsize"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = 150,vidfile=NULL)
                                     pop=quickV$population
                                     do.call("rbind",quickV$traitsumary)/quickV$popsize
                              },simplify=F)
           neutraltraitsParam$pre[,"h"]=0
           neutraltraitsParam$post[,"o"]=1
           allpostoblic=parSapply(cl,1:repli,function(r){
                                  set.seed(as.numeric(Sys.time())+r)
                                     quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=rho, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("traitsumary","finalpop","finalcomus","popsize"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = 150,vidfile=NULL)
                                     pop=quickV$population
                                     do.call("rbind",quickV$traitsumary)/quickV$popsize
                              },simplify=F)
           neutraltraitsParam$pre[,"o"]=1
           allprepostoblic=parSapply(cl,1:repli,function(r){
                                  set.seed(as.numeric(Sys.time())+r)
                                     quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=rho, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("traitsumary","finalpop","finalcomus","popsize"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = 150,vidfile=NULL)
                                     pop=quickV$population
                                     do.call("rbind",quickV$traitsumary)/quickV$popsize
                              },simplify=F)
           list(allinlaw=allinlaw,allposthoriz=allposthoriz,allprehoriz=allprehoriz,allprepostoblic=allprepostoblic,allpostoblic=allpostoblic)
}
)

stopCluster(cl)
saveRDS(file="10k.RDS",rhodepSBBIB)
