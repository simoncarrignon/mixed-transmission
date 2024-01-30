
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
mortality=c(0.15,0.01,0.01,0.02,0.05,1.00)
while(length(randomages)>500)
randomages=randomages[!ageDeath(randomages,m=mortality)]

##Neutral Neutral
ki=10
km=0
z=2
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

devtools::load_all()
ki=10
km=0
G=10
z=2
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
mortality=c(0.15,0.01,0.01,0.02,0.05,1)
agdis=sample(seq_along(agescat),size=K*percomu,prob=1-mortality,replace=T)
getagdis=sapply(agdis,function(i)sample(agescat[i]:agescat[i+1],1))
population=cbind(newpop(length(getagdis),age=getagdis,community = sample(1:K,size=length(getagdis),replace=T)),generateTraitsMatrix(length(getagdis),z))
initrun=modelVector(K=K, m=1, b=0.42, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop","repros","finalcomus"),logging=c("done","","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 75,popcapsize=20*70,fracfiss=.25)
population=resetIds(initrun$population)
population=paperpopulation
initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=T,sizes=unname(table(population[,"community"])))


initcomus$adaptivetraits[c(1,2),]=1
z=36
fullpathways=generatePathways(z = z)

pw=1
for(sb in c(0,.5,1)){
    print(pw)
    for(pre in c("v","h","o")){
        fullpathways$pre[pw:(pw+3),pre]=1
        fullpathways$s[pw:(pw+3)]=sb
        pw=pw+1
        print(pw)
        fullpathways$s[pw]=sb
        for(post in c("h","o","i")){
            fullpathways$post[pw,post]=1
            print(pw)
            fullpathways$s[pw]=sb
            pw=pw+1
        }
    }
}

expname=paste0("BonusExploBig2")
dir.create(expname)
traitsid=paste0("t",1:z)
alltraits=generateTraitsMatrix(nrow(population),z)
population=cbind(population[,-c(10,11)],alltraits)
beta=-1
bonus=runif(1000,0.0005,0.02)
beta=runif(1000,-10,0.2)
                                  electedpop=sample(nrow(initcomus$adaptivetraits),2)
electedpop=1:2
                                  initcomus$adaptivetraits[,]=0
                                  population[,traitsid]=0
                                  population[population[,"community"]==electedpop[1],traitsid]=1
                                  population[population[,"community"]==electedpop[2],traitsid]=1
                                  initcomus$adaptivetraits[,]=0
                                  initcomus$adaptivetraits[electedpop,]=1

cl<-makeCluster(60,type="FORK",outfile=file.path(expname,"log.txt"))
allpopsizesonly=parSapply(cl,1:1000,function(b){
                              set.seed(as.numeric(Sys.time())+b)
                              tryCatch({
                                  a=Sys.time()
                                  singlesimu=modelVector(K=K, m=1, b=0.216, r=bonus[b], rho=1, d=mortality, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=fullpathways,age.threshold=20, out=c("popsize","comusize"),logging=c("done"),ma=1,traitsid=traitsid,F_Th=100,testdebug=F,fracfiss=.5,beta=beta[b])
                                  print(Sys.time()-a)
                                  full=which(lengths(singlesimu$comusize)==100)
                                  pop=singlesimu$popsize
                                  end=ifelse(length(full)==0,length(pop),full)
                                  pop=pop[1:end]
                                  slope=lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2]
                                  effect=mean((pop[-1]-pop[-end])/pop[-end])
                                  saveRDS(file=file.path(expname,paste0("singlesimu_s_",b,".RDS")),singlesimu)
                                  c(slope=slope,effect=effect,beta=beta[b],bonus=bonus[b])
                              },error=function(e){ print("problem ======");print(e)})
})
stopCluster(cl)

saveRDS(file="ExploringBonusOnGrowth_1000replicate_big2.RDS",allpopsizesonly)
readRDS(file="ExploringBonusOnGrowth_60replicate_small.RDS",allpopsizesonly)

png(file="SlopeVsEffect.png",height=850,width=850,pointsize=22)
plot(allpopsizesonly["effect",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear fit" ,xlab="")
mtext(expression(frac(N[t+1]-N[t],N[t])),1,4,)
dev.off()

png(file="SlopeVsEffect.png",height=850,width=850,pointsize=22)
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="bonus for 1 traits")
dev.off()
