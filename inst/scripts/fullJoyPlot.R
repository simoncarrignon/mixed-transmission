traitsel=1:45
sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)
allcols=unlist(lapply(sexcols,rep,15))

allresults=list()
for(beta in c(-10,0)){
    allresults[[as.character(beta)]]=list()
for(bonus in c(0,1,3)){

    expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    alltraitComu=lapply(allsingle.exp,function(expfname){
                            one=readRDS(expfname)
                            if(any(lengths(one$comusize)==100))
                                end=min(which(lengths(one$comusize)==100))
                            else
                                end=length(one$popsize)
                            one$traitpercomu[[end]]/one$comusize[[end]]
})
    allresults[[as.character(beta)]][[as.character(bonus)]]= lapply(seq_along(traitsel),function(ti) unlist(lapply(alltraitComu,function(singex)singex[,ti])))
    }
}

alldensities=lapply(allresults,function(beta)lapply(beta,function(bonus)lapply(bonus,function(traits){
                                                                                 y=density(traits,na.rm=T)$y
                                                                                 x=c(0,density(traits,na.rm=T,from=0,to=1,bw=0.1)$x,1)
                                                                                 return(cbind(x,c(0,y/max(y),0)))
})))

par(mar=c(0,5,0,0),xpd=NA)
sep_ba=c(0,.4)
plot(1,1,type="n",xlim=c(1,10.2),ylim=c(0,40),axes=F,ann=F)
xs=c()
xs=c(xs,.7+tr/30+alldensities[[1]][[1]][[1]][1,1])
for(tr in length(alldensities[[1]][[1]]):1){
    polygon(.7+tr/30+alldensities[[1]][[1]][[tr]][,1],tr/1.2+alldensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],1),lwd=.6)
}
for(ba in 1:length(alldensities)){
    for(bo in 2:length(alldensities[[ba]])){
        xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/30+alldensities[[ba]][[bo]][[1]][1,1])
        for(tr in length(alldensities[[ba]][[bo]]):1){
            polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/30+alldensities[[ba]][[bo]][[tr]][,1],tr/1.2+alldensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],1),lwd=.6)
        }
    }
}
pathways=c(
           "c1","c2_(90%)","c2_(100%)","c3_(90%)","c3_(100%)",
           "c4","c5_(90%)","c5_(100%)","c6_(90%)","c6_(100%)",
           "c7","c8_(90%)","c8_(100%)","c9_(90%)","c9_(100%)")
text(x=rep(0,45)+1:45/30,y=(1:45)/1.2+.2,label=rep(pathways,3),las=2,cex=.7)
text(y=0,x=xs+.5,label=c(0,0.005,0.005,0.015,0.015))
text(y=0,x=0,label="bonus:")
text(y=40,x=45/30,label=expression(beta))
text(y=40,x=45/30+xs[c(2,4)]+1.2,label=c(-10,0))
mtext("Both sex ; grid is full",3,-1,outer=T)

dev.new()


allresults_noF=list()
for(beta in c(-10,0)){
    allresults_noF[[as.character(beta)]]=list()
for(bonus in c(0,1,3)){

    expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    alltraitComu=lapply(allsingle.exp,function(expfname){
                            one=readRDS(expfname)
                            end=length(one$popsize)
                            print(end)
                            one$traitpercomu[[end]]/one$comusize[[end]]
})
    allresults_noF[[as.character(beta)]][[as.character(bonus)]]= lapply(seq_along(traitsel),function(ti) unlist(lapply(alltraitComu,function(singex)singex[,ti])))
    }
}

alldensities=lapply(allresults_noF,function(beta)lapply(beta,function(bonus)lapply(bonus,function(traits){
                                                                                 y=density(traits,na.rm=T)$y
                                                                                 x=c(0,density(traits,na.rm=T,from=0,to=1,bw=0.1)$x,1)
                                                                                 return(cbind(x,c(0,y/max(y),0)))
})))

par(mar=c(0,5,0,0),xpd=NA)
sep_ba=c(0,.4)
plot(1,1,type="n",xlim=c(1,10.2),ylim=c(0,40),axes=F,ann=F)
xs=c()
xs=c(xs,.7+tr/30+alldensities[[1]][[1]][[1]][1,1])
for(tr in length(alldensities[[1]][[1]]):1){
    polygon(.7+tr/30+alldensities[[1]][[1]][[tr]][,1],tr/1.2+alldensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],1),lwd=.6)
}
for(ba in 1:length(alldensities)){
    for(bo in 2:length(alldensities[[ba]])){
        xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/30+alldensities[[ba]][[bo]][[1]][1,1])
        for(tr in length(alldensities[[ba]][[bo]]):1){
            polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/30+alldensities[[ba]][[bo]][[tr]][,1],tr/1.2+alldensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],1),lwd=.6)
        }
    }
}
pathways=c(
           "c1","c2_(90%)","c2_(100%)","c3_(90%)","c3_(100%)",
           "c4","c5_(90%)","c5_(100%)","c6_(90%)","c6_(100%)",
           "c7","c8_(90%)","c8_(100%)","c9_(90%)","c9_(100%)")
text(x=rep(0,45)+1:45/30,y=(1:45)/1.2+.2,label=rep(pathways,3),las=2,cex=.7)
text(y=0,x=xs+.5,label=c(0,0.005,0.005,0.015,0.015))
text(y=0,x=0,label="bonus:")
text(y=40,x=45/30,label=expression(beta))
text(y=40,x=45/30+xs[c(2,4)]+1.2,label=c(-10,0))
mtext("After 500 time step",3,-1,outer=T)
dev.new()



allresults_noF=list()
for(beta in c(-10,0)){
    allresults_noF[[as.character(beta)]]=list()
for(bonus in c(0,1,3)){

    expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    alltraitComu=lapply(allsingle.exp,function(expfname){
                            one=readRDS(expfname)
                            population=one$population[one$population[,"sex"]==0,]
                            ratio=(aggregate(population[,paste0("t",1:45)],by=list(factor(population[,"community"],levels=seq_along(one$comusize[[500]]))),FUN=sum,drop=F)[,-1])/ as.numeric(table(factor(population[,"community"],levels=seq_along(one$comusize[[500]]))))
                            print(range(ratio,na.rm=T))
                                  ratio
})
    allresults_noF[[as.character(beta)]][[as.character(bonus)]]= lapply(seq_along(traitsel),function(ti) unlist(lapply(alltraitComu,function(singex)singex[,ti])))
    }
}

alldensities=lapply(allresults_noF,function(beta)lapply(beta,function(bonus)lapply(bonus,function(traits){
                                                                                 y=density(traits,na.rm=T)$y
                                                                                 x=c(0,density(traits,na.rm=T,from=0,to=1,bw=0.1)$x,1)
                                                                                 return(cbind(x,c(0,y/max(y),0)))
})))

par(mar=c(0,5,0,0),xpd=NA)
sep_ba=c(0,.4)
plot(1,1,type="n",xlim=c(1,10.2),ylim=c(0,40),axes=F,ann=F)
xs=c()
xs=c(xs,.7+tr/30+alldensities[[1]][[1]][[1]][1,1])
for(tr in length(alldensities[[1]][[1]]):1){
    polygon(.7+tr/30+alldensities[[1]][[1]][[tr]][,1],tr/1.2+alldensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],1),lwd=.6)
}
for(ba in 1:length(alldensities)){
    for(bo in 2:length(alldensities[[ba]])){
        xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/30+alldensities[[ba]][[bo]][[1]][1,1])
        for(tr in length(alldensities[[ba]][[bo]]):1){
            polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/30+alldensities[[ba]][[bo]][[tr]][,1],tr/1.2+alldensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],1),lwd=.6)
        }
    }
}
pathways=c(
           "c1","c2_(90%)","c2_(100%)","c3_(90%)","c3_(100%)",
           "c4","c5_(90%)","c5_(100%)","c6_(90%)","c6_(100%)",
           "c7","c8_(90%)","c8_(100%)","c9_(90%)","c9_(100%)")
text(x=rep(0,45)+1:45/30,y=(1:45)/1.2+.2,label=rep(pathways,3),las=2,cex=.7)
text(y=0,x=xs+.5,label=c(0,0.005,0.005,0.015,0.015))
text(y=0,x=0,label="bonus:")
text(y=40,x=45/30,label=expression(beta))
text(y=40,x=45/30+xs[c(2,4)]+1.2,label=c(-10,0))
mtext("No Female (after 500 ts)",3,-1,outer=T)


