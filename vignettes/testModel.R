
devtools::load_all(".")
N=200
Th=400 #fission threshold
ki <- 1
km <- 0
K  <-  ki+km
m=1 #proba marriage
b=.2 #HG birth rate
r=.05 #farmer scale param
rho=.5 #marriage rule
d=0.001
maturity=0
endrepro=20
tstep=20*10
z=5



neutraltraitsParam=initNeutralTraitsPathways(z = z)
neutraltraitsParam$s=c(0,0,1,1)
neutraltraitsParam$pre[,"v"]=c(1,1,1,0,0)



pos=random2Dgrid(K=K,Gx=100)
a=initAdaptiveTraits(ki=ki,km=km)
initcomus=initialiseCommunities(traits=a,coordinates=pos)
initcomus$size=rep(N/K,K)
plot(initcomus$coordinates,pch=21,bg=apply(initcomus$adaptivetraits,1,mean)+1,cex=log(initcomus$size))

communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(N,age="random",community = communities),initNeutralTraits(N,z))

quickV=modelVector( K =K, m=m, b=b, r=r, rho=rho, d=d, maturity=maturity, endrepro=60, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam, logging=c("time"),ma=1,traitsid=paste0("t",1:z))
plot(quickV$popsize)

tstep=800
##Stable:
library(parallel)

par(mfrow=c(1,4))
for(bt in seq(2,2.3,length.out=4)){
    cl=makeCluster(10,"FORK")
    allsizes=parLapply(cl,1:40,function(i){set.seed(as.numeric(Sys.time())+i);
                       modelVector( m=1, b=.02*bt, r=0, rho=rho, d=0.02, maturity=0, endrepro=1000, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam, logging=c("time","demo"),ma=1,traitsid=paste0("t",1:z))$popsize
})
    stopCluster(cl)

    plot(1,1,type="n",xlim=c(0,tstep),ylim=c(0,800),main=bquote(b==.(bt)*d))
    sapply(allsizes,lines)
}
