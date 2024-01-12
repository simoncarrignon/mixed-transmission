
devtools::load_all(".")
N=800
Th=400 #fission threshold
ki <- 3
km <- 2
K  <-  ki+km
m=1 #proba marriage
b=.1 #HG birth rate
r=.05 #farmer scale param
rho=.5 #marriage rule
d=0.001
maturity=0
endrepro=20
tstep=20*10
generation.threshold = 20
z=5



neutraltraitsParam=generatePathways(z = z)
neutraltraitsParam$s=c(0,1,0,1,0)
neutraltraitsParam$pre[,"v"]=c(1,1,1,1,1) #this needs to be always like this otherwise there are NA traits
neutraltraitsParam$pre[,"o"]=c(0,0,0,1,1)
neutraltraitsParam$pre[,"h"]=c(0,0,1,1,1)
neutraltraitsParam$post[,"i"]=c(1,1,0,0,0)
neutraltraitsParam$post[,"o"]=c(0,0,0,1,1)
neutraltraitsParam$post[,"h"]=c(0,0,1,1,1)



pos=random2Dgrid(K=K,Gx=10)
a=initAdaptiveTraits(ki=ki,km=km,n=10)
initcomus=initialiseCommunities(traits=a,coordinates=pos)
initcomus$size=rep(N/K,K)
plot(initcomus$coordinates,pch=21,bg=apply(initcomus$adaptivetraits,1,mean)+1,cex=log(initcomus$size))

communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(N,age="random",community = communities),initNeutralTraits(N,z))

quickV=modelVector( K =K, m=m, b=b, r=r, rho=rho, d=d, maturity=maturity, endrepro=60, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=generation.threshold, logging=c("time","visu"),ma=1,traitsid=paste0("t",1:z),out=c("finalpop","finalcomus","popsize"),beta=0)
plot(quickV$popsize)

tstep=800
##Stable:
#library(parallel)
#
#par(mfrow=c(1,4))
#for(bt in seq(2,2.3,length.out=4)){
#    cl=makeCluster(10,"FORK")
#    allsizes=parLapply(cl,1:40,function(i){set.seed(as.numeric(Sys.time())+i);
#                       modelVector( m=1, b=.02*bt, r=0, rho=rho, d=0.02, maturity=0, endrepro=1000, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam, logging=c("time","demo"),ma=1,traitsid=paste0("t",1:z))$popsize
#})
#    stopCluster(cl)
#
#    plot(1,1,type="n",xlim=c(0,tstep),ylim=c(0,800),main=bquote(b==.(bt)*d))
#    sapply(allsizes,lines)
#}
#stopCluster(cl)
z=sample(2:20,1)

neutraltraitsParam=generatePathways(z = z)
neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$s=rbinom(z,1,.5)
traitsid=paste0("t",1:z)
km=sample(1:5,1)
ki=sample(1:5,1)
G=sample(4:8,1)
initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=ki,km=km,n=20),coordinates=random2Dgrid(K=ki+km,Gx=G),G=G,sizes=sample(10:100,1))
communities=unlist(lapply(seq_along(initcomus$size),function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(n=sum(initcomus$size),age="random",community = communities),initNeutralTraits(sum(initcomus$size),z))
quickV=modelVector(K=K, m=1, b=0.08, r=0.01, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus"),logging=c("visu","fission","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 80)
population=quickV$population
initcomus=quickV$finalcomus
quickV=modelVector(K=K, m=1, b=0.08, r=0.01, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus"),logging=c("visu","fission","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 80)
population=quickV$population
initcomus=quickV$finalcomus
quickV=modelVector(K=K, m=1, b=0.08, r=0.01, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus"),logging=c("visu","fission","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 80)
