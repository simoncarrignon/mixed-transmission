##This script run a simulation with similar paramaters that the simulations ran for the paper but log the number of migrants at each time steps.

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


singlesimu=modelVector(K=K, m=1, b=0.26, r=0.005*1, rho=0, d=mortality, maturity=18, endrepro=45, population=population, comus=initcomus, tstep=200, tp=fullpathways,age.threshold=20, out=c("popsize","finalpop","finalcomus","traitsumary","comusize","traitpercomu","popfull","migrantscount","comufull"),logging=c("done","time","pairing"),ma=.67,traitsid=traitsid,F_Th=100,testdebug=F,fracfiss=.5,beta=-10,popcapsize=6000,up=3)



# Proportion of migrants over the whole population per time step
allmigrants=sapply(3:length(singlesimu$comufull),function(t){
           prev=singlesimu$comufull[[t-1]]$migrantscount
           new=singlesimu$comufull[[t]]$migrantscount[1:dim(prev)[1],1:dim(prev)[2]]
           apply(new-prev,2,sum)/singlesimu$comufull[[t]]$size[1:dim(prev)[1]]
})

# Raw number of migrants per time step
allmigrantsCount=sapply(3:length(singlesimu$comufull),function(t){
           prev=singlesimu$comufull[[t-1]]$migrantscount
           new=singlesimu$comufull[[t]]$migrantscount[1:dim(prev)[1],1:dim(prev)[2]]
           apply(new-prev,2,sum)
})

axis(1)


pdf("migrantcount.pdf")
par(mfrow=c(2,2),mar=c(0,5,5,0))
stats=sapply(singlesimu$comusize,quantile)
plot(stats[3,],lwd=3,col="red",ann=F,axes=F,type="l",ylim=range(stats[c(1,5),]))
lines(stats[2,],lwd=1,col="red",lty=2)
lines(stats[4,],lwd=1,col="red",lty=2)
axis(2,col="red",col.ticks = "red",col.axis="red")
mtext("communities size",2,2.5,col="red")

mtext("Migration and communities",3,2,at=200,cex=1.4 )
box()
par(mar=c(0,0,5,5))
plot(singlesimu$popsize,lwd=3,col="blue",ann=F,axes=F,type="l")
axis(4,col="blue",col.ticks = "blue",col.axis="blue")
mtext("Total Population Size",4,2.5 ,col="blue")
box()
par(mar=c(5,5,0,0))
plot(lengths(singlesimu$comusize),lwd=3,col="darkgreen",xlab="Time",ylab="",axes=F,type="l")
axis(2,col="darkgreen",col.ticks = "darkgreen",col.axis="darkgreen")
mtext("Number of Communities",2,2.5,col="darkgreen")
axis(1,col="darkgreen",col.ticks = "darkgreen",col.axis="darkgreen")
box()
par(mar=c(5,0,0,5))
# we plot eahc point separatly, adding some nois to make them visible
plot(cbind(rep(1:length(allmigrantsCount),lengths(allmigrantsCount))+runif(length(allmigrantsCount),-.01,.01),unlist(allmigrantsCount)+runif(length(allmigrantsCount),-.25,.25)),pch=20,col=adjustcolor(1,.2),cex=.5,xlab="Time",ylab="",bty="n",axes=F,range=0,lty=1,lwd=.1)
mtext("Number of migrants per community",4,2.5)
axis(1)
axis(4)
box()

dev.off()
