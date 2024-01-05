
z=10
neutraltraitsParam=initNeutralTraitsPathways(z = z)
traitsid=paste0("t",1:z)
percomu=50
K=6
km=2
ki=4
N=K*percomu
pos=random2Dgrid(K=K,Gx=100)
a=initAdaptiveTraits(ki=ki,km=km,n=10)
initcomus=initialiseCommunities(traits=a,coordinates=pos)
initcomus$size=rep(percomu,K)
communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(N,age="random",community = communities),initNeutralTraits(N,z))

neutraltraitsParam$post[,"o"]=0
neutraltraitsParam$post[,"i"]=0
neutraltraitsParam$post[,"h"]=0
neutraltraitsParam$pre[,"o"]=0
neutraltraitsParam$pre[,"v"]=0
neutraltraitsParam$pre[,"h"]=0
neutraltraitsParam$pre[1,"h"]=1
neutraltraitsParam$pre[5:10,"h"]=1
neutraltraitsParam$post[4,"i"]=1
neutraltraitsParam$post[5,"o"]=1
neutraltraitsParam$pre[2,"h"]=1
neutraltraitsParam$pre[3,"o"]=1

population[population[,"sex"]==1,"t2"]=0
population[population[,"sex"]==1,"t3"]=0
population[population[,"sex"]==0,"t4"]=0
population[population[,"sex"]==0,"t5"]=0
population[population[,"sex"]==1 & population[,"community"]==4,"t2"]=1
population[population[,"sex"]==1 & population[,"community"]==3,"t3"]=1
population[population[,"sex"]==0 & population[,"community"]==4,"t4"]=1
population[population[,"sex"]==0 & population[,"community"]==3,"t5"]=1

quickV=modelVector(K=K, m=1, b=0.07, r=0.005, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=150,tp=neutraltraitsParam,age.threshold=sample(100,1), out=c("finalpop","weddings","migrsum","popsumary"),logging=c("visu" ,"done","time"),ma=1,traitsid=paste0("t",1:z),beta=0)

