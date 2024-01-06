z=4
neutraltraitsParam=initNeutralTraitsPathways(z = z)
neutraltraitsParam$post[,"i"]=1
neutraltraitsParam$s=rep(.5,z)
traitsid=paste0("t",1:z)
km=0
ki=z
G=2
initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=ki,km=km,n=1),coordinates=matrix(c(1,1,2,2,1,2,1,2),nrow=4,ncol=2),G=G,sizes=150,plot=T)
population=cbind(newpop(n=sum(initcomus$size),age="random",community = rep(1:4,150)),initNeutralTraits(sum(initcomus$size),z))
population[,traitsid]=0
tstep=1000
set.seed(1234)
quickV=modelVector(K=ki, m=1, b=0.2, r=0.01, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=tstep, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus","popsize"),logging=c("time"),ma=1,traitsid=paste0("t",1:z),F_Th = 150)

population=quickV$population

population[population[,"cid"]!= -1 ,"cid"] = as.numeric(as.factor(as.character(population[population[,"cid"]!= -1 ,"cid"])))
population[,"fid"] = as.numeric(as.factor(as.character(population[ ,"fid"])))
ids = as.numeric(as.factor(as.character(population[ ,"id"])))
names(ids)=population[,"id"]
population[,"id"] = as.numeric(as.factor(as.character(population[ ,"id"])))
population[,"partner"]=as.numeric(ids[as.character(population[,"partner"])])
population[is.na(population[,"partner"]),"partner"]=-1
initcomus$size=as.numeric(table(population[,"community"]))

usethis::use_data(population)
usethis::use_data(initcomus)

