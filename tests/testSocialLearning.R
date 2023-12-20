devtools::load_all(".")
z = 2
neutraltraitsParam=initNeutralTraitsPathways(z)
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

z = 10
neutraltraitsParam=initNeutralTraitsPathways(z)
neutraltraitsParam$s=c(0,0,0,0,0,1,1,1,1,1)
neutraltraitsParam$pre[,"v"]=rep(c(0,0),5) #this needs to be always like this otherwise there are NA traits
neutraltraitsParam$pre[,"o"]=rep(c(0,0),5)
neutraltraitsParam$pre[,"h"]=rep(c(0,1),5)

population=cbind(newpop(200,community = rep(0,200)),initNeutralTraits(200,z))

population[1:100,"sex"]=0
population[101:200,"sex"]=1
population[1:100,paste0("t",1:5)]=t(replicate(5,c(0,1)))
population[101:200,paste0("t",6:10)]=t(replicate(5,c(1,0)))
population[,"community"]=sample(1:2,size=10,replace=T)
traitsid=paste0("t",1:z)
x=population
age.pool=x[,"age"]==0
horiz=which(neutraltraitsParam$pre[,"h"]==1)
traits=x[age.pool,traitsid[horiz]]
subpop=x[age.pool,]

microbenchmark::microbenchmark(apply(traits,2,function(i)tapply(i,subpop[,"community"],FUN=getRatio)),aggregate(traits,list(subpop[,"community"]),FUN=getRatio))


z = 4
neutraltraitsParam=initNeutralTraitsPathways(z)
neutraltraitsParam$pre[,"v"]=c(0,0,0,0)
neutraltraitsParam$pre[,"o"]=rep(0,4)
neutraltraitsParam$pre[,"h"]=c(0,1,1,1)
neutraltraitsParam$s=c(0,1,0,0)
traitsid=paste0("t",1:z)
population=cbind(newpop(200,community = rep(0,200)),initNeutralTraits(200,z))
population[,"t1"]=rbinom(200,1,prob=.6)
population[,"t2"]=1
population[population[,"sex"]==1,"t2"]=1
population[,"t3"]=0
population[population[,"sex"]==0,"t3"]=1
population[,"t4"]=rbinom(200,1,prob=.5)
population[,"community"]=sample(1:2,size=200,replace=T)
population[1:10,"community"]=3 
population[1:100,"community"]=1
population[101:200,"community"]=2
population[101:200,"t2"]=0
population[1:100,"t3"]=0
population[1:100,"t4"]=sample(0:1,size=100,replace=T)
tochange=population
for(i in 1:10){
    print(i)
    tochange=social.learning(population,when="pre",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)
}
aggregate(tochange[,traitsid],by=list(tochange[,"sex"]),FUN=getRatio)

population[population[,"sex"]==0,"t2"]=0
population[population[,"sex"]==1,"t2"]=1
population[population[,"sex"]==0,"t3"]=1
population[population[,"sex"]==1,"t3"]=0
population[population[,"sex"]==0,"t4"]=1
population[population[,"sex"]==1,"t4"]=0
population[,"t1"]=0
population[1,"t1"]=1
neutraltraitsParam$s=c(0,1,.5,0)
tochange=population
for(i in 1:10){
    tochange=social.learning(population,when="pre",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)
}
a=aggregate(tochange[,traitsid],by=list(tochange[,"sex"]),FUN=getRatio)
#should be true
a[population[1,"sex"] + 1,"t1"]==(1/sum(population[,"sex"]==population[1,"sex"]))

neutraltraitsParam$s=c(0,1,.5,0)
tochange=population
for(i in 1:10){
    print(i)
    tochange=social.learning(population,when="pre",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)
}
aggregate(tochange[,traitsid],by=list(tochange[,"sex"]),FUN=getRatio)

tochange=population
neutraltraitsParam$post[,"o"]=rep(0,4)
neutraltraitsParam$post[,"h"]=c(1,1,1,1)
for(i in 1:10){
    print(i)
    tochange=social.learning(population,when="pre",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)
}



randomSocialLearning <- function(tochange,pathways,ttimes=100){
    for(i in 1:ttimes){
        pathways$post[,"o"]=rbinom(4,1,.5)
        pathways$post[,"h"]=rbinom(4,1,.5)
        pathways$pre[,"o"]=rbinom(4,1,.5)
        pathways$pre[,"h"]=rbinom(4,1,.5)
        pathways$s=rbinom(4,1,.5)
        print(i)
        tochange[,"age"]= sample(0:50,200,replace=T)
        new=social.learning( x=tochange , when="pre" , pathways=pathways , threshold=10 , traitsid=traitsid)
        if(is.null(dim(new)))stop()
        tochange=new
        print(aggregate(tochange[,traitsid],by=list(tochange[,"sex"]),FUN=getRatio))
    }
}
exploreSocialLearing <- function(tochange,pathways,ttimes=100){
    for(i in 1:ttimes){
        print(i)
        tochange=social.learning( x=tochange , when="pre" , pathways=pathways , threshold=10 , traitsid=traitsid)
        print(aggregate(tochange[,traitsid],by=list(tochange[,"sex"]),FUN=getRatio))
    }
}

randomSocialLearning(population,neutraltraitsParam,ttimes=100)
aggregate(population[,traitsid],by=list(population[,"sex"]),FUN=getRatio)
as=aggregate(population[,traitsid],by=list(population[,"sex"]),FUN=getRatio)
bs=exploreSocialLearing(population,neutraltraitsParam,ttimes=10)
bs[1,2]==as[1,2]
