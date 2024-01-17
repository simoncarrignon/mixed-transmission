
generateInitialPopulation <- function(km,ki,percomu,z,G,na,F_Th){
    neutraltraitsParam=generatePathways(z = z)
    traitsid=paste0("t",1:z)
    K=km+ki
    pos=random2Dgrid(K=K,Gx=G)
    a=initAdaptiveTraits(ki=ki,km=km,n=na)
    initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=F,sizes=percomu)
    initcomus$occupation[,]=1
    communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
    population=cbind(newpop(K*percomu,age="random",community = communities),generateTraitsMatrix(K*50,z))
    newtestres=modelVector(K=K, m=1, b=0.8, r=0, rho=.5, d=0.04, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop"),logging=c("time"),ma=1,traitsid=paste0("t",1:z),F_Th = F_Th)
    table(newtestres$population[,"community"])
    population=newtestres$population
    rmid=population[population[,"fid"]==-1,"id"]
    population=population[population[,"fid"]!=-1,]
    population=population[!(population[,"partner"]%in% rmid),]
    initcomus$size=unname(table(population[,"community"]))
    population=resetIds(population)
    population
}

