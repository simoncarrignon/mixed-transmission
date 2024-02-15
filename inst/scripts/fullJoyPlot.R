traitsel=1:45

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

#saveRDS(file="allresults.RDS",allresults)
#saveRDS(file="alldensities.RDS",alldensities)


allresults_noFM=list()

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

alldensities_noF=lapply(allresults_noF,function(beta)lapply(beta,function(bonus)lapply(bonus,function(traits){
                                                                                 y=density(traits,na.rm=T)$y
                                                                                 x=c(0,density(traits,na.rm=T,from=0,to=1,bw=0.1)$x,1)
                                                                                 return(cbind(x,c(0,y/max(y),0)))
})))

#saveRDS(file="allresults_noF.RDS",allresults_noF)
#saveRDS(file="alldensities_noF.RDS",alldensities_noF)




joyplot(alldensities_noF,pathways,main="No Female (after 500 ts)",angle=10,yadj=2)
joyplot(alldensities,pathways,main="When grid full (both sex)",angle=50,yadj=4)
pathwaysnames=c(
           "c1","c2_(90%)","c2_(100%)","c3_(90%)","c3_(100%)",
           "c4","c5_(90%)","c5_(100%)","c6_(90%)","c6_(100%)",
           "c7","c8_(90%)","c8_(100%)","c9_(90%)","c9_(100%)")

joyplot <- function(listdensities,pathwaysnames=pathwaysnames,main="",angle=25,yadj=2,sep_ba=c(0,.4),alpha=0.8,baselines=F,miniaxes=F){


    tot_traits=length(listdensities[[1]][[1]])
    sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
    names(sexcols)=c(0,.5,1)
    allcols=unlist(lapply(sexcols,rep,tot_traits/length(sexcols)))
    par(mar=c(0,0,0,0),oma=c(3,3,3,0),xpd=NA)
    

    xadj=ifelse(angle==0,0,1/angle)
    ylims=c(0,tot_traits/yadj+5/yadj)
    xlims=c(0,10+(1+tot_traits)*xadj)
    plot(1,1,type="n",xlim=xlims,ylim=ylims,axes=F,ann=F)
    if(baselines) abline(h=(1:tot_traits)/yadj,lwd=.4,lty=2)
    xs=c()
    xs=c(xs,.7+xadj+listdensities[[1]][[1]][[1]][1,1])
    for(tr in length(listdensities[[1]][[1]]):1){
        polygon(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1],tr/yadj+listdensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.6)
        if(miniaxes){
            na=range(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1])
            arrows(x0=na[1],y0=tr/yadj,x1=(na[1]+na[2])/2,y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
            arrows(x0=(na[1]+na[2])/2,y0=tr/yadj,x1=na[2],y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
            text(c(0,0.5,1),x=c(na[1],(na[2]+na[1])/2,na[2]),y=tr/yadj+c(0,0,0)+0.2,cex=.4,pos=1)
        }
    }
    for(ba in 1:length(listdensities)){
        for(bo in 2:length(listdensities[[ba]])){
            xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.6+tr*xadj+listdensities[[ba]][[bo]][[1]][1,1])
            for(tr in length(listdensities[[ba]][[bo]]):1){
                polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr*xadj+listdensities[[ba]][[bo]][[tr]][,1],tr/yadj+listdensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.6)
            }
        }
    }
    text(x=rep(0,tot_traits)+1:tot_traits*xadj,y=(1:tot_traits)/yadj+.1,label=rep(pathwaysnames,3),las=2,cex=.7)
    text(y=0,x=xs+.5,label=c(0,0.005,0.015,0.005,0.015))
    text(y=0,x=0,label="bonus:")
    text(y=max(ylims)-2/yadj,x=tot_traits*xadj,label=expression(beta))
    text(y=max(ylims)-2/yadj,x=tot_traits*xadj+xs[c(2,4)]+1.2,label=c(-10,0))
    mtext(main,3,-1,outer=T)

}

