cols4=colorRampPalette(c("#006400","#FFD700"))(4)
cols3=colorRampPalette(c("#006400","#FFD700"))(3)



par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(3,3,3,3),xpd=T)

for(bonus in c(0,1,3)){
    for(beta in c(-10,0,)){
        expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allrep=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             alltypes=getCommuType(one)
                             #alltypes[alltypes==2]=1
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        table(factor(alltypes[1:length(sizes_t)],level=c(0,1,2,3)))#/length(sizes_t)
})
})
        stopCluster(cl)

        allstrats=sapply(1:4,function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
        #allmedian=sapply(allstrats,function(i)i[3,])
    barplot(t(allstrats),border=NA,space=0,xlab="time",ylab="mean number of community with each strats",col=cols4,ann=F,axes=F,ylim=c(0,100))#,main=paste0("One Simulation wiht param:\n",expname))
    if(beta==-10){ axis(2,cex=.8) ; mtext("mean number of communities",2,1.8,cex=.8)}
    if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
    if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
    if(beta==0) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}
}}


#4 STRATS relative frequencies
par(mfrow=c(3,2),mar=c(0,0,0,0),oma=c(3,3,3,3),xpd=T)

for(bonus in c(0,1,3)){
    for(beta in c(-10,0)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allrep=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             alltypes=getCommuType(one)
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                        alltypes=apply(ada_time/sizes_t,1,sum)
                                        table(factor(alltypes[1:length(sizes_t)],level=c(0,1,2,3)))/length(sizes_t)

})
})
        stopCluster(cl)

        allstrats=sapply(1:4,function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
        #allmedian=sapply(allstrats,function(i)i[3,])
    barplot(t(allstrats),border=NA,space=0,xlab="time",ylab="mean number of community with each strats",col=cols4,ann=F,axes=F)#,main=paste0("One Simulation wiht param:\n",expname))
    if(beta==-10){ axis(2,cex=.8) ; mtext("mean number of communities",2,1.8,cex=.8)}
    if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
    if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
    if(beta==0) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}
}}


#4 STRATS counts
par(mfrow=c(3,2),mar=c(0,0,0,0),oma=c(3,3,3,3),xpd=T)

for(bonus in c(0,1,3)){
    for(beta in c(-10,0)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allrep=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             alltypes=getCommuType(one)
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                        alltypes=apply(ada_time/sizes_t,1,sum)
                                        table(factor(alltypes[1:length(sizes_t)],level=c(0,1,2,3)))

})
})
        stopCluster(cl)

        allstrats=sapply(1:4,function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
        #allmedian=sapply(allstrats,function(i)i[3,])
    barplot(t(allstrats),border=NA,space=0,xlab="time",ylab="mean number of community with each strats",col=cols4,ann=F,axes=F)#,main=paste0("One Simulation wiht param:\n",expname))
    if(beta==-10){ axis(2,cex=.8) ; mtext("mean number of communities",2,1.8,cex=.8)}
    if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
    if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
    if(beta==0) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}
}}

#FIGURE 1

prop=F
singleEx=T
theclass=T
for(rep in 1:50){
png(paste0("strat_and_type_",rep,".png"),width=1000,height=1000,pointsize=24)
par(mfrow=c(3,4),mar=c(0,0,1,0),oma=c(3,4,4,3),xpd=T)

for(bonus in c(0,1,3)){

    if(!prop)ylim=c(0,100)
    else ylim=c(0,1)
    if(threclass)stratclass=c(0,1,2,3)
    else stratclass=c(0,1,3)
    colstrat=colorRampPalette(c("#006400","#FFD700"))(length(stratclass))
    for(beta in c(-10,0)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        twotypes=c()
        for(type in c(0,1)){
            if(singleEx){
                one=readRDS(sample(allsingle.exp,1))
                end=getSimFull(one)
                print(end)
                single=sapply(1:end,function(t){
                                  sizes_t=one$comusize[[t]]
                                  ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                  alltypes=apply(ada_time/sizes_t,1,sum)
                                  alltypes=alltypes[ one$finalcomus$strat[1:length(sizes_t)]==type]
                                  if(prop) table(factor(alltypes,level=stratclass))/sum(one$finalcomus$strat[1:length(sizes_t)]==type)
                                  else table(factor(alltypes,level=stratclass))

})
                allstrats=t(single)
ylab="number of communities"
            }
            else{
                cl<-makeCluster(6,type="FORK",outfile="log.txt")
                allrep=parLapply(cl,allsingle.exp,function(totes){
                                     one=readRDS(totes)
                                     end=getSimFull(one)
                                     print(end)
                                     ylab="mean number of communities"
                                     sapply(round(seq(1,end,length.out=50)),function(t){
                                                sizes_t=one$comusize[[t]]
                                                ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                                alltypes=apply(ada_time/sizes_t,1,sum)
                                                if(threclass) alltypes[alltypes==2]=1
                                                alltypes=alltypes[ one$finalcomus$strat[1:length(sizes_t)]==type]
                                                if(prop) table(factor(alltypes,level=stratclass))/sum(one$finalcomus$strat[1:length(sizes_t)]==type)
                                                else table(factor(alltypes,level=stratclass))
})
})
                stopCluster(cl)
                
                allstrats=sapply(seq_along(stratclass),function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
            }
            #allmedian=sapply(allstrats,function(i)i[3,])
            twotypes=cbind(twotypes,allstrats)
            barplot(t(allstrats),border=NA,space=0,xlab="time",ylab="mean number of community with each strats",col=colstrat,density=c(30,30,NA),ann=F,axes=F,ylim=ylim)#,main=paste0("One Simulation wiht param:\n",expname))
            if(beta==-10 && type==0){ axis(2,cex=.8) ; mtext(ylab,2,1.8,cex=.8)}
            if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
            if(bonus==0 && type ==0) { mtext(bquote(beta==.(beta)),3,2,at=ifelse(beta==-10,.25,.75),cex=.9,outer=T)}
            if(bonus==0 ) { mtext(paste("Type", as.roman(type+1)),3,1,cex=.8)}
            if(beta==0 && type==1) { mtext(bquote(f==.(bonus*0.005)),4,1,cex=.8)}
        }
    }
}
dev.off()
}









#difference of ratio
for(beta in c(-10,0)){

    par(mfrow=c(3,2),mar=c(0,0,0,0),oma=c(3,3,3,3),xpd=T)
    for(bonus in c(0,1,3)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allrep=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             ttime=lapply(1:500,function(t){
                                              sizes_t=one$comusize[[t]]
                                              ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                              allstrats=apply(ada_time/sizes_t,1,sum)
                                              table(factor(allstrats,levels=stratclass),one$finalcomus$strat[1:length(allstrats)])

})
                             allrat=sapply(ttime,function(u)(t(u)/apply(u,2,sum)))
                             rbind(allrat[1,]-apply(allrat[2:4,],2,sum),apply(allrat[5:7,],2,sum)-allrat[8,])
})
        stopCluster(cl)
        #allmedian=sapply(allstrats,function(i)i[3,])
        barplot(t(allstrats),border=NA,space=0,xlab="time",ylab="mean number of community with each strats",col=cols4,ann=F,axes=F,ylim=c(0,100))#,main=paste0("One Simulation wiht param:\n",expname))
        if(beta==-10 && type= 0){ axis(2,cex=.8) ; mtext("mean number of communities",2,1.8,cex=.8)}
        if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
        if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
        if(beta==0 && type==1) { mtext(bquote(f==.(bonus*0.005)),4,1,cex=.8)}
    }
}

par(mfrow=c(3,2),mar=c(0,0,0,0),oma=c(3,3,5,3),xpd=T)
for(bonus in c(0,1,3)){
    if(!prop)ylim=c(0,100)
    else ylim=c(0,1)
    if(threclass)stratclass=c(0,1,2,3)
    else stratclass=c(0,1,3)
    for(beta in c(-10,0)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        twotypes=c()
        for(type in c(0,1)){
            if(singleEx){
                one=readRDS(sample(allsingle.exp,1))
                end=getSimFull(one)
                print(end)
                single=sapply(1:end,function(t){
                                  sizes_t=one$comusize[[t]]
                                  ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                  alltypes=apply(ada_time/sizes_t,1,sum)
                                  alltypes=alltypes[ one$finalcomus$strat[1:length(sizes_t)]==type]
                                  if(prop) table(factor(alltypes,level=stratclass))/sum(one$finalcomus$strat[1:length(sizes_t)]==type)
                                  else table(factor(alltypes,level=stratclass))

})
                allstrats=t(single)
                ylab="number of communities"
            }
            else{
                cl<-makeCluster(6,type="FORK",outfile="log.txt")
                allrep=parLapply(cl,allsingle.exp,function(totes){
                                     one=readRDS(totes)
                                     end=getSimFull(one)
                                     print(end)
                                     ylab="mean number of communities"
                                     sapply(round(seq(1,end,length.out=150)),function(t){
                                                sizes_t=one$comusize[[t]]
                                                ada_time=one$traitpercomu[[t]][,c("a1","a2","a3")]
                                                alltypes=apply(ada_time/sizes_t,1,sum)
                                                if(threclass) alltypes[alltypes==2]=1
                                                alltypes=alltypes[ one$finalcomus$strat[1:length(sizes_t)]==type]
                                                #if(prop) table(factor(alltypes,level=stratclass))/sum(one$finalcomus$strat[1:length(sizes_t)]==type)
                                                if(prop) table(factor(alltypes,level=stratclass))/sum(one$comusize[[t]]>0)
                                                else table(factor(alltypes,level=stratclass))
})
})
                stopCluster(cl)

                allstrats=sapply(seq_along(stratclass),function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
            }
            twotypes=cbind(twotypes,allstrats)
        }
        colstrat=colorRampPalette(c("#006400","#FFD700"))(length(stratclass))
        colstrat=rep(colstrat,2)
        if(prop){
            twotypes=twotypes/apply(twotypes,1,sum)
            ylim=c(0,1)
            ylab="proportion of comunity"
        }
        else{
            ylab="number of comunity"
        }
#,density=c(NA,20,20,20,20,NA)
        colstrat[2:(length(colstrat)-1)]=adjustcolor(colstrat[2:(length(colstrat)-1)],.6)
        barplot(t(twotypes),border=NA,space=0,xlab="time",ylab="mean number of community with each strats",col=colstrat,density=c(NA,40,40,40,40,NA),angle=c(NA,-35,-45,45,55,NA),ann=F,axes=F,ylim=ylim)
        if(beta==-10){ axis(2,cex=.8) ; mtext(ylab,2,1.8,cex=.8)}
        if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
        if(bonus==0 ) { mtext(bquote(beta==.(beta)),3,2,at=ifelse(beta==-10,.25,.75),cex=.9,outer=T)}
        if(beta==0 ) { mtext(bquote(f==.(bonus*0.005)),4,1,cex=.8)}
    }
}
