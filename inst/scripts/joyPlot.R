traitsel=c(1,2,3,16,17,18,31,32,33)
sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)
allcols=unlist(lapply(sexcols,rep,3))
par(mfrow=c(length(traitsel),1),oma=c(2,0,2,0))
par(mar=c(0,1,.5,1))

allresults_reduced=list()
for(beta in c(-10,0)){
    allresults_reduced[[as.character(beta)]]=list()
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
    allresults_reduced[[as.character(beta)]][[as.character(bonus)]]= lapply(traitsel,function(ti) unlist(lapply(alltraitComu,function(singex)singex[,ti])))
    }
}

alldensities=lapply(allresults_reduced,lapply,lapply,function(traits){
                        y=density(traits,na.rm=T)$y
                        x=c(0,density(traits,na.rm=T,from=0,to=1,bw=0.1)$x,1)
                        return(cbind(x,c(0,y/max(y),0)))
})

par(mar=c(0,5,0,0),xpd=NA)
sep_ba=c(0,.4)
plot(1,1,type="n",xlim=c(1,12),ylim=c(0,8),axes=F,ann=F)
angle=5
yadj=1.5
xs=c()
xs=c(xs,.7+tr/angle+alldensities[[1]][[1]][[1]][1,1])
for(tr in length(alldensities[[1]][[1]]):1){
    polygon(.7+tr/angle+alldensities[[1]][[1]][[tr]][,1],tr/yadj+alldensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],.8),lwd=.6)
                na=range(.7+tr/angle+alldensities[[1]][[1]][[tr]][,1])
                arrows(x0=na[1],y0=tr/yadj,x1=(na[1]+na[2])/2,y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
                arrows(x0=(na[1]+na[2])/2,y0=tr/yadj,x1=na[2],y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
                text(c(0,0.5,1),x=c(na[1],(na[2]+na[1])/2,na[2]),y=tr/yadj+c(0,0,0)+0.01,cex=.4,pos=1)
}
for(ba in 1:length(alldensities)){
    for(bo in 2:length(alldensities[[ba]])){
        xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.6+tr/angle+alldensities[[ba]][[bo]][[1]][1,1])
        for(tr in length(alldensities[[ba]][[bo]]):1){
            polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/angle+alldensities[[ba]][[bo]][[tr]][,1],tr/yadj+alldensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],.8),lwd=.6)
                na=range(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr/angle+alldensities[[ba]][[bo]][[tr]][,1])
                arrows(x0=na[1],y0=tr/yadj,x1=(na[1]+na[2])/2,y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
                arrows(x0=(na[1]+na[2])/2,y0=tr/yadj,x1=na[2],y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
                text(c(0,0.5,1),x=c(na[1],(na[2]+na[1])/2,na[2]),y=tr/yadj+c(0,0,0)+0.01,cex=.4,pos=1)
        }
    }
}
pathways=rep(c( "c1","c2_(90%)","c2_(100%)"),3)
text(x=rep(0,length(pathways))+1:length(pathways)/angle,y=(1:length(pathways))/yadj+.2,label=pathways,las=2,cex=.7)
text(y=0,x=xs+.5,label=c(0,0.005,0.015,0.005,0.015))
text(y=0,x=0,label="bonus:")
text(y=7.5,x=length(pathways)/angle*2,label=expression(beta))
text(y=7.5,x=length(pathways)/angle+xs[c(2,4)]+.8,label=c(-10,0))



allresults_noF=list()
for(beta in c(-10,0)){
    allresults_noF[[as.character(beta)]]=list()
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
    allresults_noF[[as.character(beta)]][[as.character(bonus)]]= lapply(seq_along(traitsel),function(ti) unlist(lapply(alltraitComu,function(singex)singex[,ti])))
    }
}
alldensities=lapply(allresults,function(beta)lapply(beta,function(bonus)lapply(bonus,function(traits){
                                                                                 y=density(traits,na.rm=T)$y
                                                                                 x=c(0,density(traits,na.rm=T,from=0,to=1,bw=0.1)$x,1)
                                                                                 return(cbind(x,c(0,y/max(y),0)))
})))

