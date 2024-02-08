allpopsizesonly=readRDS("pposizeLargerTh.RDS")
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
boxplot(traitcount,col=cols[as.character(fullpathways$s)])
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

boxplot(traitcount,col=cols[as.character(fullpathways$s)],ylab="time to fixation of 0")
mtext(side=1,line=3,text="time to fixation of 0 with rho=1",cex=1.5)
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

boxplot(traitcount.ada,col=cols[as.character(fullpathways$s)],ylab="time to extinction")
mtext(side=1,line=3,text="time to extinction with rho=1",cex=1.5)
par(xpd=NA)
arrows(x0=seq(1,36,4),y0=rep(530,9),x1=seq(1,36,4)+3,y1=rep(530,9),lwd=2,angle=90,code=3,length=.1)
text(((seq(1,36,4)+3)+seq(1,36,4))/2,535,c("v","h","o"),)
text(0,535,"pre-marital\n social learning",pos=2)
text(0,-5,"post-marital\n social learnin",pos=2)
legend("bottomleft",fill=1:3,legend=c(0,.5,1),title="sex bias")

par(xpd=F)
boxplot(traitfreq.ada,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,.2))
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




expnames=c("NeutrEvoLSingleBR500kRHO_1_G10","AdapEvoLSingleBR500kRHO_1_G10",paste0("AdapEvoLSingleBR500kRHO_1_G10_bonus_",2:3))
expnames=c("NeutrEvoLSingleBR500kRHO_1_G10",paste0("AdapEvoLSingleBR500kRHO_1_G10_bonus_",1:3,"_beta-1"))



for(expname in expnames[3]){
    #adaptivw
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsextinct=sapply(allsingle.exp,function(expfname){
                             one=readRDS(expfname)
                             apply(one$traitsumary/one$popsize,2,function(trait)suppressWarnings(min(which(trait==0))))
})
    traitsextinct[is.infinite(traitsextinct)]=500
    traitcount=t(traitsextinct)
    colnames(traitcount)=rep(c("-","h","o","i"),9)

    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          end=length(one$popsize)
                          one$traitsumary[end,]/one$popsize[end]
})
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(c("-","h","o","i"),9)

    popsizes=sapply(allsingle.exp,function(expfname){
                        one=readRDS(expfname)
                        one$popsize
})
    popsizes=apply(popsizes,1,quantile)
    adaptraits=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          try(sapply(2:length(one$popsize),function(i)apply(one$finalcomus$adaptivetraits[as.numeric(names(one$popsumary[[i]]$community)),]*as.vector(one$popsumary[[i]]$community),2,sum)/one$popsize[i]))[1,]
})
    adaptraits=apply(adaptraits,1,quantile)

    pdf(file=paste0("neutral_traits_",expname,".pdf"),width=18)
    par(mfrow=c(1,2))

    cols=1:3
    names(cols)=c(0,.5,1)

    boxplot(traitcount,col=cols[as.character(fullpathways$s)],ylab="time to fixation 0")
    mtext(side=1,line=3,text="time to fixation of 0 with rho=1",cex=1.5)
    par(xpd=NA)
    arrows(x0=seq(1,36,4),y0=rep(530,9),x1=seq(1,36,4)+3,y1=rep(530,9),lwd=2,angle=90,code=3,length=.1)
    text(((seq(1,36,4)+3)+seq(1,36,4))/2,535,c("v","h","o"),)
    text(0,535,"pre-marital\n social learning",pos=2)
    text(0,-5,"post-marital\n social learnin",pos=2)
    legend("bottomleft",fill=1:3,legend=c(0,.5,1),title="sex bias")

par(mfrow=c(1,2))
    par(xpd=F)
    boxplot(traitfreq,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,1))
    bns=as.numeric(sub(".*_","",expname))
    bns=ifelse(is.na(bns),bns,1)
    mtext(side=1,line=3,text=paste0("frequencies at the end with rho=1, beta 0 base br=0.216 max bonus=",0.005*2),cex=1.5)
    par(xpd=NA)
    arrows(x0=seq(1,36,4),y0=rep(max(traitfreq),9),x1=seq(1,36,4)+3,y1=rep(max(traitfreq),9),lwd=2,angle=90,code=3,length=.1)
    text(((seq(1,36,4)+3)+seq(1,36,4))/2,max(traitfreq),c("v","h","o"),pos=3)
    text(0,max(traitfreq),"pre-marital\n social learning",pos=2)
    text(0,-.1,"post-marital\n social learnin",pos=2)
    legend("topright",fill=1:3,legend=c(0,.5,1),title="sex bias")
    dev.off()

    pdf(file=paste0("posize_",expname,".pdf"),width=10)
    par(mfrow=c(1,2))

    plot(1,1,type="n",ylim=range(popsizes[c(2,4),]),xlim=c(1,500),log="y",xlab="time",ylab="pop size",main= "population size for adaptive scenario")
    lines(popsizes[3,],lwd=2)
    lines(popsizes[2,],lwd=1)
    lines(popsizes[4,],lwd=1)
    legend("topleft",lwd=c(2,1),legend=c("median","75% HDR"))

    plot(1,1,type="n",ylim=range(adaptraits[c(2,4),]),xlim=c(1,500),log="y",xlab="time",ylab="%",main= "frequency of a_1 in the population")
    lines(adaptraits[3,],lwd=2)
    lines(adaptraits[2,],lwd=1)
    lines(adaptraits[4,],lwd=1)
    legend("bottomright",lwd=c(2,1),legend=c("median","75% HDR"))
    dev.off()
}

expnames=c("NeutrEvoLSingleBR500kRHO_1_G10","AdapEvoLSingleBR500kRHO_1_G10_bonus_2","AdapEvoLSingleBR500kRHO_1_G10_bonus_2_beta-1")
names(expnames)=c("neutral","beta 0","beta -1")

three.expe=sapply(expnames,function(expname){
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          end=length(one$popsize)
                          one$traitsumary[end,]/one$popsize[end]
})
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(c("-","h","o","i"),9)

    popsizes=sapply(allsingle.exp,function(expfname){
                        one=readRDS(expfname)
                        one$popsize
})
list(popsizes,traitfreq)
})


par(mfrow=c(3,4))
for(beta in c(-10,0,0.1)){
for(bonus in 0:3){
    expname=paste0("TraitTraj_500t_RHO_1_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          end=length(one$popsize)
                          one$traitsumary[end,]/one$popsize[end]
    })
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(c("-","h","o","i"),9)
    cols=1:3
    names(cols)=c(0,.5,1)
    par(xpd=F)
    boxplot(traitfreq,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,1),main=paste0("rho:1, br:0.216 bonus:",0.005*bonus,", beta:",beta))
    bns=as.numeric(sub(".*_","",expname))
    bns=ifelse(is.na(bns),bns,1)
    #mtext(side=1,line=3,text=paste0("freq. at end rho=1, br=0.216 maxbonus=",0.005*bonus,", beta ",beta),cex=1.5)
    par(xpd=NA)
    arrows(x0=seq(1,36,4),y0=rep(max(traitfreq),9),x1=seq(1,36,4)+3,y1=rep(max(traitfreq),9),lwd=2,angle=90,code=3,length=.1)
    text(((seq(1,36,4)+3)+seq(1,36,4))/2,max(traitfreq),c("v","h","o"),pos=3)
    text(0,max(traitfreq),"pre-marital\n social learning",pos=2)
    text(0,-.1,"post-marital\n social learnin",pos=2)
    legend("topright",fill=1:3,legend=c(0,.5,1),title="sex bias")
}
}

for(rho in c(0.5,1)){
pdf(file=paste0("rho_",rho,".pdf"),width=27,height=18)
par(mfrow=c(3,4))
for(beta in c(-10,0,0.1)){
for(bonus in 0:3){
    expname=paste0("TraitTraj_500t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          end=length(one$popsize)
                          one$traitsumary[end,]/one$popsize[end]
    })
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(c("-","h","o","i"),9)
    cols=1:3
    names(cols)=c(0,.5,1)
    par(xpd=F)
    boxplot(traitfreq,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,1),main=paste0("rho:",rho,", br:0.216 bonus:",0.005*bonus,", beta:",beta))
    bns=as.numeric(sub(".*_","",expname))
    bns=ifelse(is.na(bns),bns,1)
    #mtext(side=1,line=3,text=paste0("freq. at end rho=1, br=0.216 maxbonus=",0.005*bonus,", beta ",beta),cex=1.5)
    par(xpd=NA)
    arrows(x0=seq(1,36,4),y0=rep(max(traitfreq),9),x1=seq(1,36,4)+3,y1=rep(max(traitfreq),9),lwd=2,angle=90,code=3,length=.1)
    text(((seq(1,36,4)+3)+seq(1,36,4))/2,max(traitfreq),c("v","h","o"),pos=3)
    text(0,max(traitfreq),"pre-marital\n social learning",pos=2)
    text(0,-.1,"post-marital\n social learnin",pos=2)
    legend("topright",fill=1:3,legend=c(0,.5,1),title="sex bias")
}
}
dev.off()
}
#popsize:

allresults

getresults=c()
for(rho in c(0.5,1)){
    for(beta in c(-10,0,0.1)){
        for(bonus in 0:3){
            expname=paste0("TraitTraj_500t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
            allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
            for( expfname in allsingle.exp){
                one=readRDS(expfname)
                end=length(one$popsize)
                getresults=rbind(
                                 getresults,
                                 c(
                                   rho=rho,
                                   beta=beta,
                                   bonus=bonus,
                                   slope=lm(y~x,data=cbind.data.frame(y=log(one$popsize),x=(1:end)))$coefficients[2],
                                   popsize=one$popsize[c(200,300,400,500)],
                                   apply(one$finalcomus$adaptivetraits,2,sum)/one$popsize[end],
                                   t=t(one$traitsumary[end,]/one$popsize[end])
                                 )
                )
            }
        }
    }
}

allresutls=readRDS("matrixfinal")

boxplot(getresults[,"slope.x"]~getresults[,"bonus"],xlab="bonus (increament of 0.005 added to 0.216)",ylab="slope of log of curv fit")
boxplot(getresults[,"slope.x"]~getresults[,"beta"],xlab="adoption of traits)",ylab="slope of log of curv fit")
par(mfrow=c(1,2))
for(rho in  unique(getresults[,"rho"])){
    subset=getresults[getresults[,"rho"]==rho,]
    ul=boxplot(subset[,"slope.x"]~subset[,"bonus"]+subset[,"beta"],xaxt="n",xlab="",ylab="slope of log of curv fit",col=heat.colors(4),main=paste0("slope of curve with rho=",rho),ylim=range(getresults[,"slope.x"]))
    axis(1,at=c(2.5,6.5,10.5),label=c(-10,0,1))
    mtext(expression(beta),1,3)
    legend("bottomright",fill=heat.colors(4),legend=0.216+0.005*0:3,title="bonus")
}
for(rho in  unique(getresults[,"rho"])){
    subset=getresults[getresults[,"rho"]==rho,]
    ul=boxplot(subset[,"popsize4"]~subset[,"bonus"]+subset[,"beta"],xaxt="n",xlab="",ylab="popsize of log of curv fit",col=heat.colors(4),main=paste0("popsize of curve with rho=",rho),ylim=range(getresults[,"popsize4"]))
    axis(1,at=c(2.5,6.5,10.5),label=c(-10,0,1))
    mtext(expression(beta),1,3)
    legend("bottomright",fill=heat.colors(4),legend=0.216+0.005*0:3,title="bonus")
}


getalltraits=list()
for(rho in c(0.5,1)){
    for(beta in c(-10,0,0.1)){
        for(bonus in 0:3){
            expname=paste0("TraitTraj_500t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
            allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
            allexp=lapply(allsingle.exp,function(expfname){
                              one=readRDS(expfname)
                              end=length(one$popsize)
                              sapply(1:end,function(t)apply(one$traitpercomu[[t]],2,sum)/one$popsize[[t]])
                })
            allstats=lapply(1:39,function(at) apply(sapply(allexp,function(exp)exp[at,]),1,quantile))
            getalltraits[[length(getalltraits)+1]]=allstats[[37]][3,]
        }
    }
}

getalltraits= lapply(as.character(c(0.5,1)),function(rho){
                         lapply(as.character(c(0:3)),function(bonus){
                                    lapply(as.character(c(-10,0,0.1)),function(beta){
                                               print(paste("doing",rho,bonus,beta))
                                               expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
                                               allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
                                               allexp=lapply(allsingle.exp,function(expfname){
                                                                 one=readRDS(expfname)
                                                                 end=length(one$popsize)
                                                                 sapply(1:end,function(t)one$traitpercomu[[t]]/one$comusize[[t]])
                                                                 )
})
                                               alltrais=lapply(1:45,function(at) apply(sapply(allexp,function(exp)exp[at,]),1,quantile,na.rm=T))
})
                                 })
                })


rhos=c(0.5,1)
bonuses =0:3
betas=c(-10,0,0.1)
getallevents= lapply(as.character(rhos),function(rho){
                         lapply(as.character(bonuses),function(bonus){
                                    lapply(as.character(betas),function(beta){
                                               expname=paste0("TraitTraj_500t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
                                               allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
                                               cl<-makeCluster(4,type="FORK",outfile="log.txt")
                                               allexp=parLapply(cl,allsingle.exp,function(expfname){
                                               allexp=lapply(allsingle.exp,function(expfname){
                                                                    print(paste("doing",rho,bonus,beta,expfname))
                                                                    one=readRDS(expfname)
                                                                    ncom=max(sapply(one$traitpercomu,nrow))
                                                                    allevents=sapply(one$traitpercomu,function(i) sapply(1:ncom,function(c)sum(i[c,paste0("a",1:3)]>0)))
                                                                    apply(apply(allevents,1,function(u)(u[-1]-u[-ncol(allevents)])!=0),1,sum,na.rm=T)
})
                                               stopCluster(cl)
                                               apply(do.call("rbind",allexp),2,mean)

})
})
})

getallpopsize= lapply(as.character(rhos),function(rho){
                         lapply(as.character(bonuses),function(bonus){
                                    lapply(as.character(betas),function(beta){
                                               expname=paste0("TraitTraj_500t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
                                               allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
                                               cl<-makeCluster(8,type="FORK",outfile="log.txt")
                                               allexp=parSapply(cl,allsingle.exp,function(expfname){
                                                                    print(paste("doing",rho,bonus,beta,expfname))
                                                                    readRDS(expfname)$popsize
})
                                               stopCluster(cl)
                                               apply(allexp,1,mean)

})
})
})

par(mfrow=c(2,4))
for(rho in 1:2){
for(bonus in 1:4){
plot(1,1,type="n",xlim=c(0,500),ylim=c(0.2,.7),main=paste("rho=",rhos[rho],",bonus=",bonuses[bonus]),xlab="time",ylab="frequency of a_1")
for(beta in 1:3){
    lines(getalltraits[[rho]][[bonus]][[beta]][[37]][3,],lwd=4,col=heat.colors(3)[beta])
    #lines(ex[[beta]][[37]][2,],lwd=1,col=heat.colors(3)[beta])
    #lines(ex[[beta]][[37]][4,],lwd=1,col=heat.colors(3)[beta])
}
legend("toplef",legend=c(-10,0,0.1),col=heat.colors(3),lwd=4,title=expression(beta))

}
}
dev.off()

png("freq_a1_vs_beta_full.png",pointsize=24,width=1400,height=900)
betas=c(-10,0,0.1)
par(mfrow=c(1,3))
for(beta in 1:3){
plot(1,1,type="n",xlim=c(0,500),ylim=c(0.15,.7),main=paste("frequency of a1 in population with beta=",betas[beta]),xlab="time",ylab="frequency of a_1")
    lines(ex[[beta]][[37]][3,],lwd=4,col=heat.colors(3)[beta])
    lines(ex[[beta]][[37]][2,],lwd=1,col=heat.colors(3)[beta])
    lines(ex[[beta]][[37]][4,],lwd=1,col=heat.colors(3)[beta])
}
legend("toplef",legend=c(-10,0,0.1),col=heat.colors(3),lwd=4,title=expression(beta))
dev.off()

betacol=rev(colorRampPalette(c("lightblue", "yellow", "red"))(3))
png(file="adoption_wrt_beta.png",pointsize=22,width=850,height=850)
par(mfrow=c(1,1))
plot(1,1,type="n",xlim=c(0,500),ylim=c(0,2),xlab="time",ylab="mean number of adoption")
for(rho in 1:2)
    for(bonus in 1:4)
        for(beta in 1:3) points(getallevents[[rho]][[bonus]][[beta]],cex=1.2,bg=adjustcolor(betacol[beta],.5),pch=20+bonus,lwd=.2)
legend("toplef",legend=c(-10,0,0.1),pt.bg=betacol,title=expression(beta),pch=22)
dev.off()

png(file="adoption_wrt_bonuses.png",pointsize=26,height=850,width=1200)
par(mfrow=c(1,3))
for(beta in 1:3){
    plot(1,1,type="n",xlim=c(0,500),ylim=c(0,2),xlab="time",ylab="mean number of adoption")
    for(bonus in 1:4) for(rho in 1:2) points(getallevents[[rho]][[bonus]][[beta]],cex=1.8,bg=adjustcolor(rev(rainbow(4))[bonus],.5),pch=20+rho,lwd=.2)
}
legend("toplef",legend=bonuses,pt.bg=rev(rainbow(length(bonuses))),title="bonus",pch=21,cex=1.2)
mtext("mean number of adoption events",3,-2,outer=T)
dev.off()

png(file="adoption_wrt_beta_normalised.png",pointsize=22,width=850,height=850)
plot(1,1,type="n",xlim=c(0,500),ylim=c(0,.0005),xlab="time",ylab="mean number of adoption")
for(rho in 1:2) for(bonus in 1:4) for(beta in 1:3) points(getallevents[[rho]][[bonus]][[beta]]/getallpopsize[[rho]][[bonus]][[beta]][-1],cex=1.2,bg=adjustcolor(betacol[beta],.5),pch=20+bonus,lwd=.2)
legend("toplef",legend=c(-10,0,0.1),pt.bg=betacol,title=expression(beta),pch=22)
mtext("mean number of adoption events normalized by population size",3,-2,outer=T)
dev.off()

png(file="adoption_wrt_bonuses_normalised.png",pointsize=26,height=850,width=1200)
par(mfrow=c(1,3))
for(beta in 1:3){
    plot(1,1,type="n",xlim=c(0,500),ylim=c(0,.0005),xlab="time",ylab="mean number of adoption")
    for(bonus in 1:4) for(rho in 1:2) points(getallevents[[rho]][[bonus]][[beta]]/getallpopsize[[rho]][[bonus]][[beta]][-1],cex=1.8,bg=adjustcolor(rev(rainbow(4))[bonus],.5),pch=20+rho,lwd=.2)
}
legend("topright",legend=bonuses,pt.bg=rev(rainbow(length(bonuses))),title="bonus",pch=21,cex=1.2)
mtext("mean number of adoption events normalized by population size",3,-2,outer=T)
dev.off()
    



library(RColorBrewer)
library(scales)

# Create a sample vector
# Define a diverging color palette
palette <- colorRampPalette(brewer.pal(11, "RdBu"))

# Get colors based on the values in v
# Rescale v to be between 0 and 1 for color mapping
colors <- palette(100)[rescale(allpopsizesonly["beta",], c(1, 100))]

plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="bonus for 1 traits",pch=21,bg=colors)
plot(allpopsizesonly["beta",],allpopsizesonly["",],ylab="slope of log transfrom linear " )

onesime=readRDS("exploAp/singlesimu_s_107.RDS")
highb=readRDS("exploAp/singlesimu_s_117.RDS")
lowb=readRDS("exploAp/singlesimu_s_31.RDS")
u=lowb$traitpercomu[[1]]
freqAdap <- function(expe){
    bytim=sapply(expe$traitpercomu,function(u){
                     totadap=apply(u[,paste0("a",1:3)],2,sum)
    })
    apply(bytim,1,"/",expe$popsize)
}

plot(bytim[3,]/lowb$popsize)

params=readRDS("exploAp/ExploringAdaptiveTratis_120replicate_small.RDS")

allpopsizesonly=readRDS("../../ExploringBonusOnGrowth_1000replicate_big2.RDS")
plot(expl["slope.x",],expl["effect",])

png(file="SlopeVsEffect.png",height=850,width=850,pointsize=22)
plot(allpopsizesonly["effect",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear fit" ,xlab="")
mtext(expression(frac(N[t+1]-N[t],N[t])),1,4,)
dev.off()

png(file="SlopeWRTbonus.png",height=850,width=850,pointsize=22)
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="bonus for 1 traits",pch=20,cex=2,col=adjustcolor("blue",.6))
dev.off()
plot(allpopsizesonly["beta",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="bonus for 1 traits",pch=20,cex=2,col=adjustcolor("blue",.6))

allpopsizesonly=readRDS("NewAges_ExploringBonusOnGrowth_1000_500ts.RDS")
allpopsizesonly=do.call("cbind",allpopsizesonly[lengths(allpopsizesonly)==4])
plot(expl["slope.x",],expl["effect",])

png(file="SlopeVsEffect.png",height=850,width=850,pointsize=22)
plot(allpopsizesonly["effect",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear fit" ,xlab="")
mtext(expression(frac(N[t+1]-N[t],N[t])),1,4,)
dev.off()

png(file="SlopeWRTbonus.png",height=850,width=850,pointsize=22)
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="bonus for 1 traits",pch=20,cex=2,col=adjustcolor(col_vector,.6))
dev.off()

cut(allpopsizesonly["bonus",],breaks=3)


# Assuming you have a vector of "bonus" values


# Define a custom color palette
par(mfrow=c(2,2))
bgpe=5
custom_colors <- rev(colorRampPalette(c("red", "white", "blue"))(bgpe))

allpopsizesonly=readRDS("NewAges_ExploringBonusOnGrowth_1000_500ts.RDS")
allpopsizesonly=do.call("cbind",allpopsizesonly[lengths(allpopsizesonly)==4])
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="beta for 1 traits",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["beta",],breaks=bgpe)],.6),main="age 18-45",ylim=c(-0.0025,0.012))

mydata=data.frame(beta=cut(allpopsizesonly["beta",],breaks=bgpe),bonus=cut(allpopsizesonly["bonus",],breaks=3),slope=allpopsizesonly["slope.x",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012))
allpopsizesonly=readRDS("../../ExploringBonusOnGrowth_1000replicate_big2.RDS")
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="beta for 1 traits",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["beta",],breaks=bgpe)],.6),main="age 18-65",ylim=c(-0.0025,0.012))

mydata=data.frame(beta=cut(allpopsizesonly["beta",],breaks=bgpe),bonus=cut(allpopsizesonly["bonus",],breaks=3),slope=allpopsizesonly["slope.x",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012))
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(allpopsizesonly["beta",],breaks=bgpe)))


cols=1:3
names(cols)=c(0,.5,1)
ltys=rep(unlist(lapply(c(1,2,3),rep,5)),3)
allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
traitsextinct=sapply(allsingle.exp,function(expfname){
           one=readRDS(allsingle.exp[1])
           alltypes=getCommuType(one)
           freqpercomu=lapply(1:length(one$comusize),function(t)one$traitpercomu[[t]]/one$comusize[[t]])
           par(mfrow=c(1,4))
           lapply(0:3,function(ct){
                      ctype=which(alltypes==ct)
                      alltraitmeanCtype=sapply(freqpercomu,function(freqpct)apply(freqpct[ctype,],2,mean,na.rm=T))
                      plot(0,1,type="n",ylim=c(0,1),xlim=c(0,500),main=paste(expname,"full hunter farmer"))
                      for(i in 1:45) lines(alltraitmeanCtype[i,], col=cols[as.character(fullpathways$s[i])],lty=ltys[i],lwd=4)
})

           aa=legend("topright",legend=names(cols),lwd=2,col=cols,title="sex",bty="n")
           legend(aa$rect$left,aa$rect$top-aa$rect$h,legend=c("v","h","o"),lwd=2,lty=1:3,col=1,title="pre-marital",bty="n")
           rep(cols,9)
           one$traitsextinct.ada
           apply(one$traitsumary/one$popsize,2,function(trait)suppressWarnings(min(which(trait==0))))
})


for(beta in c(-10,0)){
for(bonus in c(0,3)){
    dev.new()
par(mfrow=c(3,3),oma=c(2,3,4,0),mar=c(0,0,0,0))

expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_0_G10_bonus_",bonus,"_beta_",beta)
allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
totes=sample(seq_along(allsingle.exp),9)
for( i in totes){
    one=readRDS(allsingle.exp[i])
    alltypes=getCommuType(one)
    tyepTtime=sapply(1:500,function(t){
                         sizes_t=one$comusize[[t]]
                         table(factor(alltypes[1:length(sizes_t)],level=0:3))/length(sizes_t)
})

    cols=colorRampPalette(c("#006400","#FFD700"))(4)
    barplot(tyepTtime,border=NA,space=0,xlab="time",ylab="% communities",col=cols)#,main=paste0("One Simulation wiht param:\n",expname))
    par(new=T)
    #plot(lengths(one$comusize),axes=F,ann=F,type="l")
    axis(1)
}
    mtext(paste("rho","0","beta",beta,"bonus", bonus),3,1,outer=T,cex=3)
}
}

