
bs=runif(100,.18,.3)
initcomus$size=unname(table(population[,"community"]))
initrun=modelVector(K=K, m=1, b=0.212, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=170, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop"),logging=c("done","visu"),ma=1,traitsid=paste0("t",1:z),F_Th = 100,popcapsize=)

table(initrun$population[,"community"]) 
population=resetIds(test$population)

initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=F,sizes=unname(table(test$population[,"community"])))
test=modelVector(K=K, m=1, b=0.18, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop"),logging=c("done","visu"),ma=1,traitsid=paste0("t",1:z),F_Th = 100)

    cl <- makeCluster(10,type="FORK",logfile="log.txt")
    newtestres=parSapply(cl,bs,function(b)modelVector(K=K, m=1, b=b, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = NULL)$popsize)

stopCluster(cl)


    table(newtestres$population[,"community"])
    population=newtestres$population
    rmid=population[population[,"fid"]==-1,"id"]
    population=population[population[,"fid"]!=-1,]
    population=population[!(population[,"partner"]%in% rmid),]
    initcomus$size=unname(table(population[,"community"]))
    population=resetIds(population)
    population



    
randomages=sample(1:85,5000,replace=T)
mortality=c(0.15 0.01 0.01 0.02 0.05 1.00)
while(length(randomages)>500)
randomages=randomages[!ageDeath(randomages,m=mortality)]

##Neutral Neutral
ki=10
km=0
neutraltraitsParam=generatePathways(z = z)
traitsid=paste0("t",1:z)
K=km+ki
pos=random2Dgrid(K=K,Gx=G)
percomu=50
a=initAdaptiveTraits(ki=ki,km=km,n=3)
initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=T,sizes=percomu)
initcomus$occupation[,]=1
communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
agescat=c(0,5,18,40,65,85)
agdis=sample(seq_along(agescat),size=K*percomu,prob=1-mortality,replace=T)
getagdis=sapply(agdis,function(i)sample(agescat[i]:agescat[i+1],1))
population=cbind(newpop(length(getagdis),age=getagdis,community = sample(1:K,size=length(getagdis),replace=T)),generateTraitsMatrix(length(getagdis),z))
initrun=modelVector(K=K, m=1, b=0.42, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop","repros","finalcomus"),logging=c("done","visu","fission"),ma=1,traitsid=paste0("t",1:z),F_Th = 75,popcapsize=20*70,fracfiss=.25)
population=resetIds(initrun$population)
initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=T,sizes=unname(initrun$finalcomus$size))
realrun=modelVector(K=K, m=1, b=0.2, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop","repros"),logging=c("done","visu","fission"),ma=1,traitsid=paste0("t",1:z),F_Th = 100,popcapsize=25*150,fracfiss=.5)
initcomus$size=unname(table(population[,"community"]))


bs=runif(200,.18,.25)
cl <- makeCluster(10,type="FORK",outfile="log.txt")
birthRexp=parSapply(cl,bs,function(b)modelVector(K=K, m=1, b=b, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop","repros"),logging=c("done"),ma=1,traitsid=paste0("t",1:z),F_Th = 100,fracfiss=.5))
stopCluster(cl)
