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
initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=F,sizes=percomu)
initcomus$occupation[,]=1
communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
agescat=c(0,5,18,40,65,85)
mortality=c(0.15,0.01,0.01,0.02,0.05,1)
agdis=sample(seq_along(agescat),size=K*percomu,prob=1-mortality,replace=T)
getagdis=sapply(agdis,function(i)sample(agescat[i]:agescat[i+1],1))
population=cbind(newpop(length(getagdis),age=getagdis,community = sample(1:K,size=length(getagdis),replace=T)),generateTraitsMatrix(length(getagdis),z))
initrun=modelVector(K=K, m=1, b=0.42, r=0, rho=.5, d=c(0.15,0.01,0.01,0.02,0.05,1), maturity=18, endrepro=65, population=population, comus=initcomus, tstep=500, tp=generatePathways(z=z),age.threshold=20, out=c("popsize","finalpop","repros","finalcomus"),logging=c("done","","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 75,popcapsize=20*70,fracfiss=.25)
population=resetIds(initrun$population)
initcomus=initialiseCommunities(ki=ki,km=km,traits=a,coordinates=pos,G=G,plot=T,sizes=unname(table(population[,"community"])))


initcomus$adaptivetraits[c(1,2),]=1
z=45
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
        for(tr in c(.9,1)){
            for(post in c("h","o")){
                fullpathways$post[pw,post]=1
                print(pw)
                fullpathways$s[pw]=sb
                fullpathways$tr[pw]=tr
                pw=pw+1
            }
        }
    }
}
traitsid=paste0("t",1:z)
alltraits=generateTraitsMatrix(nrow(population),z,initval=0)
population=cbind(population[,1:9],alltraits)
electedpop=sample(nrow(initcomus$adaptivetraits),2)
electedpop=1:2
initcomus$adaptivetraits[,]=0
population[,traitsid]=0
population[population[,"community"]==electedpop[1],traitsid]=1
population[population[,"community"]==electedpop[2],traitsid]=1
initcomus$adaptivetraits[,]=0
initcomus$adaptivetraits[electedpop,]=1
initcomus$strat[electedpop]=1
for(rho in c(0)){
    for(beta in c(-10,0,.1)){
        for(bonus in c(0,1,3)){
            expname=paste0("NewPW_invertSex_TraitTraj_StratTraj_10t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
            dir.create(expname)
            cl<-makeCluster(30,type="FORK",outfile=file.path(expname,"log.txt"))
            allpopsizesonly=parSapply(cl,1:90,function(b){
                                          set.seed(as.numeric(Sys.time())+b)
                                          tryCatch({
                                              singlesimu=modelVector(K=K, m=1, b=0.216, r=0.005*bonus, rho=rho, d=mortality, maturity=18, endrepro=45, population=population, comus=initcomus, tstep=500, tp=fullpathways,age.threshold=20, out=c("popsize","finalpop","finalcomus","traitsumary","comusize","traitpercomu"),logging=c("done"),ma=.67,traitsid=traitsid,F_Th=100,testdebug=F,fracfiss=.5,beta=beta)
                                              singlesimu$popsize
                                              id=b
                                              while(file.exists(file.path(expname, paste0("singlesimu_s_", id, ".RDS")))) id <- id + 1
                                              saveRDS(file=file.path(expname,paste0("singlesimu_s_",id,".RDS")),singlesimu)

                                          },error=function(e){ print("problem ======");print(e)})
})
            stopCluster(cl)
            saveRDS(file=file.path(expname,"neutralTraits_longer.RDS"),allpopsizesonly)
        }
    }
}

