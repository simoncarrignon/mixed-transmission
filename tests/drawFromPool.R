devtools::load_all(".")


popex
social.learning(popex,
expl=c()
expl=matrix(0,nrow=60,ncol=3)
colnames(expl)=c("sex","t1","t2")
expl[1:30,"sex"]=1
expl[1:30,"t1"]=1
sum(drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,1)))==0
expl[1:15,"sex"]=0
newdis=apply(replicate(1000,drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,1))),1,sum)/1000

sum(expl[,"t1"]==1) #30 t1:1 both sexes
sum(expl[expl[,"sex"]==0,"t1"]==1) #15 female with t1 is 1
sum(expl[,"sex"]==0) #45 female total. 
#Thus looking at bewdis should be 15/45 for the firs, 0 the other
print(15/45)
newdis

expl[expl[,"sex"]==1,"t2"]=1 #all sex 1 will get 1 at t2
expl[expl[,"sex"]==1,"t2"][1]=0 #except one. we have 15, so 14/15 for t2 (with sexbiases c(0,1)
newdis=apply(replicate(1000,drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,1))),1,sum)/1000
#newdis[2] hsould be close to 14/15=0.933

newdis=apply(replicate(1000,drawFromPool(pool.traits=expl[,2:3],pool.sex=expl[,1],sexbiases=c(0,.5))),1,sum)/1000
newdis[2]# hsould be close to 14/15=0.933

social.learning(x = popex,pathways=neutraltraitsParam,when="pre",traitsid=c("t1","t2"),threshold=15)


##using drawFromPool for vertical transmission
ntraits=5
prts=matrix(0,nrow=2,ncol=ntraits)
prts
prts[1,1]=1
prts[2,2]=1
prts[2,3]=1
prts[2,4]=1
prts[1,5]=1
prts

drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=rep(1,ntraits))==prts[1,]
drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=rep(0,ntraits))==prts[2,]

allres=do.call("rbind",replicate(1000,drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=rep(.5,ntraits)),simplify=F))
apply(allres,2,sum)/1000 
# should be 50/50

biases=rep(.5,ntraits)
biases[3]=0
allres=do.call("rbind",replicate(1000,drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=biases),simplify=F))
#should be true:
(apply(allres,2,sum)/1000)[3] == prts[2,3]

allres=do.call("rbind",replicate(1000,drawFromPool(pool.traits=prts,pool.sex=c(1,0),sexbiases=c(.1,.9)),simplify=F))

apply(allres,2,sum)/1000 


oneind=c(1,0,0,0,1)
drawFromPool(pool.traits=oneind,pool.sex=1,sexbiases=c(1,1,0,0,1))
prts[1,]
pool.sex=c(1,0)[1]
sexbiases=1
