cols4=colorRampPalette(c("#006400","#FFD700"))(4)
cols3=colorRampPalette(c("#006400","#FFD700"))(3)



par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(3,3,3,3),xpd=T)

for(bonus in c(0,1,3)){
    for(beta in c(-10,0,0.1)){
        expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allrep=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             alltypes=getCommuType(one)
                             #alltypes[alltypes==2]=1
                             sapply(1:500,function(t){
                                        sizes_t=one$comusize[[t]]
                                        table(factor(alltypes[1:length(sizes_t)],level=c(0,1,2,3)))/length(sizes_t)
})
})
        stopCluster(cl)

        allstrats=sapply(1:4,function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
        #allmedian=sapply(allstrats,function(i)i[3,])
    barplot(t(allstrats),border=NA,space=0,xlab="time",ylab="mean proportion of each strats",col=cols4,ann=F,axes=F)#,main=paste0("One Simulation wiht param:\n",expname))
    if(beta==-10){ axis(2,cex=.8) ; mtext("mean proportion of each strats",2,1.8,cex=.8)}
    if(bonus==3) { axis(1,cex=.8) ; mtext("time",1,1.8,cex=.8)}
    if(bonus==0) { mtext(bquote(beta==.(beta)),3,1,cex=.8)}
    if(beta==0) { mtext(bquote(bonus==.(bonus)),4,1,cex=.8)}
}}
