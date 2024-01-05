expl=c()
expl=matrix(0,nrow=60,ncol=3)
colnames(expl)=c("sex","t1","t2")
expl[1:30,"sex"]=1
expl[1:30,"t1"]=1
testthat::expect_true(sum(drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,1)))==0)
expl[1:15,"sex"]=0
newdis=apply(replicate(100,drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,1))),1,sum)/100

 #30 t1:1 both sexes
testthat::expect_equal(sum(expl[,"t1"]==1),30)
testthat::expect_equal(sum(expl[expl[,"sex"]==0,"t1"]==1),15) #15 female with t1 is 1
testthat::expect_equal(sum(expl[,"sex"]==0),45) #45 female total. 
#Thus looking at bewdis should be 15/45 for the firs, 0 the other
testthat::expect_identical(newdis[2],0)
cat(paste("should be close to:",round(15/45,digit=2)," => ",newdis[1]))
testthat::expect_gt(newdis[1],0.1)

expl[expl[,"sex"]==1,"t2"]=1 #all sex 1 will get 1 at t2
expl[expl[,"sex"]==1,"t2"][1]=0 #except one. we have 15, so 14/15 for t2 (with sexbiases c(0,1)
newdis=apply(replicate(1000,drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,1))),1,sum)/1000
cat(paste("should be close to (14/15)=.93=>",newdis[2]))

newdis=apply(replicate(1000,drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,.5))),1,sum)/1000
cat(paste("should be close to (14/15)/2=.46=> ",newdis[2]))

#popex
#social.learning(x = popex,pathways=neutraltraitsParam,when="pre",traitsid=c("t1","t2"),threshold=15)


##using drawFromPool for vertical transmission
ntraits=5
prts=matrix(0,nrow=2,ncol=ntraits)
prts[1,1]=1
prts[2,2]=1
prts[2,3]=1
prts[2,4]=1
prts[1,5]=1

testthat::expect_identical(drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=rep(1,ntraits)),prts[1,])
testthat::expect_identical(drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=rep(0,ntraits)),prts[2,])

allres=do.call("rbind",replicate(100,drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=rep(.5,ntraits)),simplify=F))
cat(paste("all number should be close to .5 => ", paste(apply(allres,2,sum)/100,collapse=",")))
# should be 50/50

biases=rep(.5,ntraits)
biases[3]=0
allres=do.call("rbind",replicate(100,drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=biases),simplify=F))
#should be true:
testthat::expect_identical((apply(allres,2,sum)/100)[3],prts[2,3])

allres=do.call("rbind",replicate(100,drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=c(.1,.9)),simplify=F))

#apply(allres,2,sum)/1000


oneind=c(1,0,0,0,1)
testthat::expect_identical(drawFromPool(pool.traits=oneind,pool.sex=1,sexbiases=c(1,1,0,0,1)),oneind)
prts[1,]
pool.sex=c(1,0)[1]
sexbiases=1


#load("data/pool.ex.Rbin")
#testthat::expect_identical(sapply(pools,function(pool)drawFromPool(pool.traits=pool[,2,drop=F],pool.sex=pool[,1,drop=F],sexbiases=0)),c(0,0,0))
