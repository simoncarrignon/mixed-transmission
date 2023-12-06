neutraltraitsParam=initNeutralTraitsPathways(z = 2)
neutraltraitsParam$s=c(0,1)
neutraltraitsParam$pre[,"v"]=c(0,0) #this needs to be always like this otherwise there are NA traits
neutraltraitsParam$pre[,"o"]=c(0,0)
neutraltraitsParam$pre[,"h"]=c(1,1)
neutraltraitsParam$post[,"i"]=c(0,0)
neutraltraitsParam$post[,"o"]=c(0,0)
neutraltraitsParam$post[,"h"]=c(0,0)

population=cbind(newpop(10,community = rep(0,10)),initNeutralTraits(10,2))

population[1:5,"sex"]=0
population[6:10,"sex"]=1
population[1:5,paste0("t",1:2)]=t(replicate(5,c(0,1)))
population[6:10,paste0("t",1:2)]=t(replicate(5,c(1,0)))
population[,"community"]=sample(1:2,size=10,replace=T)

social.learning(x = population,when="pre",threshold = 10,pathways = neutraltraitsParam)
p,os=random2Dgrid(K=K,Gx=100)
a=initAdaptiveTraits(ki=ki,km=km)
initcomus=initialiseCommunities(traits=a,coordinates=pos)
initcomus$size=rep(N/K,K)
plot(initcomus$coordinates,pch=21,bg=apply(initcomus$adaptivetraits,1,mean)+1,cex=log(initcomus$size))

communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))

quickV=modelVector( K =K, m=m, b=b, r=r, rho=rho, d=d, maturity=maturity, endrepro=60, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=generation.threshold, logging=c("time"),ma=1,traitsid=paste0("t",1:z))
plot(quickV$popsize)



