    sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
    names(sexcols)=c(0,.5,1)


pathways=readRDS("inst/scripts/fullpatways.RDS")
    for(beta in c(-10,0)){

png(paste0("tracking_traits_beta",beta,".png"),width=1000,height=1600,pointsize=20)
par(mfrow=c(3,2),mar=c(1,1,1,1),oma=c(3,3,3,3),xpd=T)
for(bonus in c(0,1,3)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
for(type in c(0,1)){
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allTypeI=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        neut_time=one$traitpercomu[[t]][,paste0("t",1:45)]
                                        neut_time[ one$finalcomus$strat[1:length(sizes_t)]==type,]/sizes_t[one$finalcomus$strat[1:length(sizes_t)]==type]

})
})
        stopCluster(cl)
plot(1,1,xlim=c(0,500),ylim=c(0,1),type="n",main=paste("type",as.roman(type+1)))
ltys=rep(1:3,3)
names(ltys)=c(1,3,5,16,18,20,31,33,35)
for(tri in seq_along(pathways$s)[c(1,3,5,16,18,20,31,33,35)]){
        #lapply(lapply(allTypeI,function(exp)sapply(exp[tri,],mean,na.rm=T)),lines,lwd=2,col=adjustcolor(sexcols[as.character(pathways$s[tri])],.2))
        allval=lapply(allTypeI,function(exp)sapply(exp[tri,],mean,na.rm=T))
        quantil=sapply(lapply(1:500,function(ts)sapply(allval,"[",ts)),quantile)
        lines(quantil[3,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=4,lty=ltys[as.character(tri)])
        lines(quantil[2,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=1)
        lines(quantil[4,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=1)
}
if(type==0)legend("topleft",legend=c("c1","c2 (100%)","c3 (100%)","s=0","s=0.5","s=1"),lwd=2,lty=c(1:3,1,1,1),ncol=2,col=c(rep(1,3),sexcols))

    if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
    if(type==1) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}

}
}
dev.off()
}


pathways=readRDS("inst/scripts/fullpatways.RDS")
    for(beta in c(-10,0)){

png(paste0("tracking_traits_beta",beta,".png"),width=1000,height=1600,pointsize=20)
par(mfrow=c(3,2),mar=c(1,1,1,1),oma=c(3,3,3,3),xpd=T)
for(bonus in c(0,1,3)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
for(type in c(0,1)){
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allTypeI=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        neut_time=one$traitpercomu[[t]][,paste0("t",1:45)]
                                        neut_time[ one$finalcomus$strat[1:length(sizes_t)]==type,]/sizes_t[one$finalcomus$strat[1:length(sizes_t)]==type]

})
})
        stopCluster(cl)


}
}



for(prt in c(90)){
    if(prt==90)traitsel=c(1,2,4,16,17,19,31,32,34)
    else traitsel=c(1,3,5,16,18,20,31,33,35)
    for(beta in c(-10,0)){

        png(paste0("tracking_traits_",prt,"_beta",beta,".png"),width=1000,height=1600,pointsize=20)
        par(mfrow=c(3,2),mar=c(1,1,1,1),oma=c(3,3,3,3),xpd=T)
        for(bonus in c(0,1,3)){
            expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
            allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
            for(type in c(0,1)){
                cl<-makeCluster(6,type="FORK",outfile="log.txt")
                allTypeI=parLapply(cl,allsingle.exp,function(totes){
                                       one=readRDS(totes)
                                       sapply(1:500,function(t){
                                                  sizes_t=one$comusize[[t]]
                                                  neut_time=one$traitpercomu[[t]][,paste0("t",1:45)]
                                                  neut_time[ one$finalcomus$strat[1:length(sizes_t)]==type,]/sizes_t[one$finalcomus$strat[1:length(sizes_t)]==type]

})
})
                stopCluster(cl)
                plot(1,1,xlim=c(0,500),ylim=c(0,1),type="n",main=paste("type",as.roman(type+1)))
                ltys=rep(1:3,3)
                names(ltys)=traitsel
                for(tri in seq_along(pathways$s)[traitsel]){
                    #lapply(lapply(allTypeI,function(exp)sapply(exp[tri,],mean,na.rm=T)),lines,lwd=2,col=adjustcolor(sexcols[as.character(pathways$s[tri])],.2))
                    allval=lapply(allTypeI,function(exp)sapply(exp[tri,],mean,na.rm=T))
                    quantil=sapply(lapply(1:500,function(ts)sapply(allval,"[",ts)),quantile)
                    lines(quantil[3,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=4,lty=ltys[as.character(tri)])
                    lines(quantil[2,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=1,lty=ltys[as.character(tri)])
                    lines(quantil[4,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=1,lty=ltys[as.character(tri)])
                }
                if(prt==90){ if(type==0)legend("topleft",legend=c("c1","c2 (90%)","c3 (90%)","s=0","s=0.5","s=1"),lwd=2,lty=c(1:3,1,1,1),ncol=2,col=c(rep(1,3),sexcols))}
                else{ if(type==0)legend("topleft",legend=c("c1","c2 (100%)","c3 (100%)","s=0","s=0.5","s=1"),lwd=2,lty=c(1:3,1,1,1),ncol=2,col=c(rep(1,3),sexcols))}

                if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
                if(type==1) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}

            }
        }
        dev.off()
    }
}



traitsel=c(1:5,16:20,31:35)
betas=c(-10,0)
names(betas)=betas
bonuses= c(0,1,3)
names(bonuses)=bonuses
allsexes=lapply(betas,function(beta){
           lapply(bonuses,function(bonus){
                      expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
                      allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
                      sapply(c(0,1),function(type){
                                 cl<-makeCluster(6,type="FORK",outfile="log.txt")
                                 allTypeI=parLapply(cl,allsingle.exp,function(totes){
                                                        one=readRDS(totes)
                                                        end=getSimFull(one)
                                                        sapply(1:500,function(t){
                                                                   sizes_t=one$comusize[[t]]
                                                                   neut_time=one$traitpercomu[[t]][,paste0("t",traitsel)]
                                                                   round(neut_time[ one$finalcomus$strat[1:length(sizes_t)]==type,]/sizes_t[one$finalcomus$strat[1:length(sizes_t)]==type],digit=2)

    })
    })
                                 stopCluster(cl)
                                 lapply(seq_along(traitsel),function(traits)sapply(1:500,function(timestep)apply(sapply(allTypeI,function(ex)sapply(ex[traits,timestep],function(tt)table(cut(tt,breaks=seq(0,1,0.05),include.lowest=T)))),1,sum)))
    })
})
})


traitsel=c(1:5,16:20,31:35)
betas=c(-10,0)
names(betas)=betas
bonuses= c(0,1,3)
names(bonuses)=bonuses
allend=lapply(betas,function(beta){
                        lapply(bonuses,function(bonus){
                                   expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
                                   allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
                                   sapply(c(0,1),function(type){
                                              sapply(allsingle.exp,function(totes){
                                                         one=readRDS(totes)
                                                         end=getSimFull(one)
    })
    })
})
})

saveRDS(file="alldamnedres.RDS",alldamnedres)

pathways=readRDS("inst/scripts/fullpatways.RDS")
    for(beta in c(-10,0)){

png(paste0("tracking_traits_beta",beta,".png"),width=1000,height=1600,pointsize=20)
par(mfrow=c(3,2),mar=c(1,1,1,1),oma=c(3,3,3,3),xpd=T)
for(bonus in c(0,1,3)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
for(type in c(0,1)){
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allTypeI=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        neut_time=one$traitpercomu[[t]][,paste0("t",1:45)]
                                        neut_time[ one$finalcomus$strat[1:length(sizes_t)]==type,]/sizes_t[one$finalcomus$strat[1:length(sizes_t)]==type]

})
})
        stopCluster(cl)


}
}



for(prt in c(90)){
    if(prt==90)traitsel=c(1,2,4,16,17,19,31,32,34)
    else traitsel=c(1,3,5,16,18,20,31,33,35)
    for(beta in c(-10,0)){

        png(paste0("tracking_traits_",prt,"_beta",beta,".png"),width=1000,height=1600,pointsize=20)
        par(mfrow=c(3,2),mar=c(1,1,1,1),oma=c(3,3,3,3),xpd=T)
        for(bonus in c(0,1,3)){
            expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
            allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
            for(type in c(0,1)){
                cl<-makeCluster(6,type="FORK",outfile="log.txt")
                allTypeI=parLapply(cl,allsingle.exp,function(totes){
                                       one=readRDS(totes)
                                       sapply(1:500,function(t){
                                                  sizes_t=one$comusize[[t]]
                                                  neut_time=one$traitpercomu[[t]][,paste0("t",1:45)]
                                                  neut_time[ one$finalcomus$strat[1:length(sizes_t)]==type,]/sizes_t[one$finalcomus$strat[1:length(sizes_t)]==type]

})
})
                stopCluster(cl)
                plot(1,1,xlim=c(0,500),ylim=c(0,1),type="n",main=paste("type",as.roman(type+1)))
                ltys=rep(1:3,3)
                names(ltys)=traitsel
                for(tri in seq_along(pathways$s)[traitsel]){
                    #lapply(lapply(allTypeI,function(exp)sapply(exp[tri,],mean,na.rm=T)),lines,lwd=2,col=adjustcolor(sexcols[as.character(pathways$s[tri])],.2))
                    allval=lapply(allTypeI,function(exp)sapply(exp[tri,],mean,na.rm=T))
                    quantil=sapply(lapply(1:500,function(ts)sapply(allval,"[",ts)),quantile)
                    lines(quantil[3,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=4,lty=ltys[as.character(tri)])
                    lines(quantil[2,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=1,lty=ltys[as.character(tri)])
                    lines(quantil[4,] ,col=adjustcolor(sexcols[as.character(pathways$s[tri])],1),lwd=1,lty=ltys[as.character(tri)])
                }
                if(prt==90){ if(type==0)legend("topleft",legend=c("c1","c2 (90%)","c3 (90%)","s=0","s=0.5","s=1"),lwd=2,lty=c(1:3,1,1,1),ncol=2,col=c(rep(1,3),sexcols))}
                else{ if(type==0)legend("topleft",legend=c("c1","c2 (100%)","c3 (100%)","s=0","s=0.5","s=1"),lwd=2,lty=c(1:3,1,1,1),ncol=2,col=c(rep(1,3),sexcols))}

                if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
                if(type==1) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}

            }
        }
        dev.off()
    }
}
