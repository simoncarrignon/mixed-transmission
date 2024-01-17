allpopsizesonly=readRDS("pposizeLargerTh.RDS")

allpopsizesonly=readRDS("NeutrEvoLSingleBR5k/neutralTraits_longer.RDS")
par(mfrow=c(1,2))
boxplot(lengths(allpopsiz)~bs,ylab="simu length vs birth rate ",xlab="birth rate",main="extinctions")
finished=allpopsizesonly[lengths(allpopsizesonly)==2000]
finsize=sapply(finished,"[",2000)
boxplot(finsize~round(bs[lengths(allpopsizesonly)==2000],digit=2),ylab="population size at the end of simulation for simulation who finised",xlab="birth rate ",main="pop at end without extinctions")
abline(h=nrow(population),col="red")
text("init pop. size",y=nrow(population)+10,x=1,col="red",pos=3)

allfinal=sapply(allpopsizesonly[lengths(allpopsizesonly)>2],function(i)i[length(i)])
boxplot(allfinal~bs[lengths(allpopsizesonly)>2],ylab="population size at the end of simulation for simulation who finised",xlab="birth rate ",main="pop at end ")
abline(h=nrow(population),col="red")


clg=sort(unique(round(bs,digit=3))) 
gpes=cut(bs,breaks=clg,include.lowest = T)

par(mfrow=c(1,2))
boxplot(finsize~round(bs[lengths(allpopsizesonly)==2000],digit=3),ylab="population size at the end of simulation for simulation who finised",xlab="birth rate ",main="pop at end without extinctions")
abline(h=nrow(population),col="red")
text("init pop. size",y=nrow(population)+10,x=1,col="red",pos=3)
plot(1,1,xlim=c(0,1000),ylim=range(0,1000),type="n",main="population size throught time (median over 200 replicates",ylab="# individuals",xlab="time")
cols=rev(heat.colors(length(levels(gpes))))
cols=rainbow(length(levels(gpes)))
for(gpe in seq_along(levels(gpes))){
    subset=allpopsizesonly[ gpes == levels(gpes)[gpe]]
    compressedts=sapply(1:1000,function(t)sapply(subset[lengths(subset)>2],function(i)i[t]))
    getquant=apply(compressedts,2,quantile,na.rm=T)
    lines(getquant[3,],lwd=2,col=cols[gpe])
    #lines(getquant[2,],lwd=1,col=cols[gpe])
    #lines(getquant[4,],lwd=1,col=cols[gpe])
}

legend("top",lwd=2,col=cols,legend=clg[-1],title="birth rate")
par(mfrow=c(2,3))
cols=rainbow(length(levels(gpes)))
for(gpe in seq_along(levels(gpes))){
    plot(1,1,xlim=c(0,10000),ylim=range(0,1400),type="n",main=paste0("pop, size wrt time b=",clg[-1][gpe]),ylab="# individuals",xlab="time")
    subset=allpopsizesonly[ gpes == levels(gpes)[gpe]]
    sapply(subset,function(i)try(lines(i,col=adjustcolor(cols[gpe],.5),lwd=1)))
}

legend("top",lwd=2,col=cols,legend=clg[-1])


plot(tapply( lengths(allpopsizesonly)!=2000,gpes,sum)/table(gpes) ,type="l",lwd=5)


plot(1,1,xlim=c(0,10000),ylim=range(0,800),type="n",main="population size throught time (median over 200 replicates",ylab="# individuals",xlab="time")
    compressedts=sapply(1:10000,function(t)sapply(allpopsizesonly[lengths(allpopsizesonly)>2],function(i)i[t]))
    getquant=apply(compressedts,2,quantile,na.rm=T)
    lines(getquant[3,],lwd=2,col=cols[gpe])
    #lines(getquant[2,],lwd=1,col=cols[gpe])
    #lines(getquant[4,],lwd=1,col=cols[gpe])
}

expname="NeutrEvoLSingleBR5kRHO_1"

´#neutral
allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
traitsextinct=sapply(allsingle.exp,function(expfname){
           one=readRDS(expfname)
           apply(one$traitsumary/one$popsize,2,function(trait)suppressWarnings(min(which(trait==0))))
})
traitsextinct[is.infinite(traitsextinct)]=2000
cols=1:3
names(cols)=c(0,.5,1)
traitcount=t(traitsextinct)
colnames(traitcount)=rep(c("-","h","o","i"),6)
boxplot(traitcount,col=cols[as.character(neutraltraitsParam$s)])
arrows(x0=seq(1,24,4),y0=rep(2100,6),x1=seq(1,24,4)+3,y1=rep(2100,6),lwd=2,angle=90,code=3,length=.1)
text(((seq(1,24,4)+3)+seq(1,24,4))/2,2150,c("v","h","o"),)
text(0,2200,"pre-marrital",pos=2)
text(0,-100,"post-marrital",pos=2)

´#neutral 3 SEX BIAS
expname="NeutrEvoLSingleBR5kRHO_1_3SEX"
allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
traitsextinct=sapply(allsingle.exp,function(expfname){
           one=readRDS(expfname)
           apply(one$traitsumary/one$popsize,2,function(trait)suppressWarnings(min(which(trait==0))))
})
traitsextinct[is.infinite(traitsextinct)]=1199
traitcount=t(traitsextinct)
colnames(traitcount)=rep(c("-","h","o","i"),9)

popsizes=sapply(allsingle.exp,function(expfname){
           one=readRDS(expfname)
           one$popsize
})
popsizes=apply(popsizes,1,quantile)
adaptraits=sapply(allsingle.exp,function(expfname){
           one=readRDS(expfname)
           sapply(2:length(one$popsize),function(i)apply(one$finalcomus$adaptivetraits[as.numeric(names(one$popsumary[[i]]$community)),]*as.vector(one$popsumary[[i]]$community),2,sum)/one$popsize[i])
})
adaptraits=apply(adaptraits,1,quantile)
par(mfrow=c(1,2))
plot(1,1,type="n",ylim=range(popsizes[c(2,4),]),xlim=c(1,1200),log="y",xlab="time",ylab="pop size",main= "population size for neutral scenario")
lines(popsizes[3,],lwd=2)
lines(popsizes[2,],lwd=1)
lines(popsizes[4,],lwd=1)
legend("topleft",lwd=c(2,1),legend=c("median","75% HDR"))

cols=1:3
names(cols)=c(0,.5,1)

boxplot(traitcount,col=cols[as.character(neutraltraitsParam$s)],ylab="time to extinction")
mtext(side=1,line=3,text="time to extinction with rho=1",cex=1.5)
par(xpd=NA)
arrows(x0=seq(1,36,4),y0=rep(1300,9),x1=seq(1,36,4)+3,y1=rep(1300,9),lwd=2,angle=90,code=3,length=.1)
text(((seq(1,36,4)+3)+seq(1,36,4))/2,1330,c("v","h","o"),)
text(0,1320,"pre-marital\n social learning",pos=2)
text(0,-50,"post-marital\n social learnin",pos=2)
legend("bottomleft",fill=1:3,legend=c(0,.5,1),title="sex bias")


dev.new()
expname="AdapEvoLSingleBR5kRHO_1_3SEX"
#adaptivw
allsingle.exp.ada <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
traitsextinct.ada=sapply(allsingle.exp.ada,function(expfname){
           one=readRDS(expfname)
           apply(one$traitsumary/one$popsize,2,function(trait)suppressWarnings(min(which(trait==0))))
})
traitsextinct.ada[is.infinite(traitsextinct.ada)]=500
traitcount.ada=t(traitsextinct.ada)
colnames(traitcount.ada)=rep(c("-","h","o","i"),9)

traitsfreq.ada=sapply(allsingle.exp.ada,function(expfname){
           one=readRDS(expfname)
           end=length(one$popsize)
           one$traitsumary[end,]/one$popsize[end]
})
traitfreq.ada=t(traitsfreq.ada)
colnames(traitfreq.ada)=rep(c("-","h","o","i"),9)

popsizes.ada=sapply(allsingle.exp.ada,function(expfname){
           one=readRDS(expfname)
           one$popsize
})
popsizes.ada=apply(popsizes.ada,1,quantile)
adaptraits.ada=sapply(allsingle.exp.ada,function(expfname){
           one=readRDS(expfname)
           try(sapply(2:length(one$popsize),function(i)apply(one$finalcomus$adaptivetraits[as.numeric(names(one$popsumary[[i]]$community)),]*as.vector(one$popsumary[[i]]$community),2,sum)/one$popsize[i]))[1,]
})
adaptraits.ada=apply(adaptraits.ada,1,quantile)

par(mfrow=c(1,2))

cols=1:3
names(cols)=c(0,.5,1)

boxplot(traitcount.ada,col=cols[as.character(neutraltraitsParam$s)],ylab="time to extinction")
mtext(side=1,line=3,text="time to extinction with rho=1",cex=1.5)
par(xpd=NA)
arrows(x0=seq(1,36,4),y0=rep(530,9),x1=seq(1,36,4)+3,y1=rep(530,9),lwd=2,angle=90,code=3,length=.1)
text(((seq(1,36,4)+3)+seq(1,36,4))/2,535,c("v","h","o"),)
text(0,535,"pre-marital\n social learning",pos=2)
text(0,-5,"post-marital\n social learnin",pos=2)
legend("bottomleft",fill=1:3,legend=c(0,.5,1),title="sex bias")

par(xpd=F)
boxplot(traitfreq.ada,col=cols[as.character(neutraltraitsParam$s)],ylab="% of population",ylim=c(0,.2))
mtext(side=1,line=3,text="frequencies at the end with rho=1",cex=1.5)
par(xpd=NA)
arrows(x0=seq(1,36,4),y0=rep(.21,9),x1=seq(1,36,4)+3,y1=rep(.21,9),lwd=2,angle=90,code=3,length=.1)
text(((seq(1,36,4)+3)+seq(1,36,4))/2,.212,c("v","h","o"),)
text(0,.21,"pre-marital\n social learning",pos=2)
text(0,-.01,"post-marital\n social learnin",pos=2)
legend("topright",fill=1:3,legend=c(0,.5,1),title="sex bias")

plot(1,1,type="n",ylim=range(popsizes.ada[c(2,4),]),xlim=c(1,500),log="y",xlab="time",ylab="pop size",main= "population size for adaptive scenario")
lines(popsizes.ada[3,],lwd=2)
lines(popsizes.ada[2,],lwd=1)
lines(popsizes.ada[4,],lwd=1)
legend("topleft",lwd=c(2,1),legend=c("median","75% HDR"))

plot(1,1,type="n",ylim=range(adaptraits.ada[c(2,4),]),xlim=c(1,500),log="y",xlab="time",ylab="%",main= "frequency of a_1 in the population")
lines(adaptraits.ada[3,],lwd=2)
lines(adaptraits.ada[2,],lwd=1)
lines(adaptraits.ada[4,],lwd=1)
legend("bottomright",lwd=c(2,1),legend=c("median","75% HDR"))
