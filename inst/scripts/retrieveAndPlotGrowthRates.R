params=readRDS("NewAges_ExploringBonusOnGrowth_5000_500ts_params.RDS")
bonus=params[,"bonus"]
beta=params[,"beta"]
allpopsizesonly=sapply(1:nrow(params),function(expr){
 expfname=paste0("BonusExploNewAges_5000_500ts/singlesimu_s_",expr,".RDS")
if(file.exists(expfname)){
    singlesimu=readRDS(expfname)
    full=which(lengths(singlesimu$comusize)==100)
    pop=singlesimu$popsize
    end=ifelse(length(full)==0,length(pop),full)
    pop=pop[1:end]
    slope=lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2]
    effect=mean((pop[-1]-pop[-end])/pop[-end])
    c(slope=slope,effect=effect,beta=beta[expr],bonus=bonus[expr])
}
else{ c(NULL,NULL,NULL,NULL) }
})
allpopsizesonly=do.call(cbind,allpopsizesonly)

png(file="SlopeVsEffect.png",height=850,width=850,pointsize=22)
points(allpopsizesonly["bonus",]/0.67 +0.216/0.67,allpopsizesonly["effect",],ylab="slope of log transfrom linear fit" ,xlab="",col="")
mtext(expression(frac(N[t+1]-N[t],N[t])),1,4,)
dev.off()

mydata=data.frame(beta=cut(allpopsizesonly["beta",],breaks=bgpe),bonus=cut(allpopsizesonly["bonus",],breaks=3),slope=allpopsizesonly["slope.x",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012))
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(allpopsizesonly["beta",],breaks=bgpe)))


allpopsizesonly1000=readRDS("NewAges_ExploringBonusOnGrowth_1000_500ts.RDS")
allpopsizesonly1000=do.call("cbind",allpopsizesonly1000[lengths(allpopsizesonly1000)==4])
allpopsizesonly=cbind(allpopsizesonly,allpopsizesonly1000)

bgpe=4
custom_colors <- rev(colorRampPalette(c("red", "white", "blue"))(bgpe))


plot(allpopsizesonly["beta",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear fit" ,xlab="beta for 1 traits",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["bonus",],breaks=bgpe)],.4),main="age 18-45",ylim=c(-0.0025,0.012))
mtext(expression(frac(N[t+1]-N[t],N[t])),2,2,)


par(mfrow=c(1,2))
plot(bonusbeta["bonus",],bonusbeta["slope.x",],ylab="slope of log transfrom linear " ,xlab="f",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(bonusbeta["beta",],breaks=bgpe,dig.lab=2,ordered_result=T)],.6),main="Effective Growth Rate wrt f & beta",ylim=c(0.002,0.013),lwd=.4)


mydata=data.frame(beta=cut(bonusbeta["beta",],breaks=bgpe,dig.lab=2,ordered_result=T),bonus=cut(bonusbeta["bonus",],breaks=4,dig.lab=2,ordered_result=T),slope=bonusbeta["effect",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors,xaxt="n",ylab="slope of log transfrom linear fit",xlab="f",outline=T,ylim=c(0.002,0.013))
axis(1,at=(seq(1,16,4)+seq(4,16,4))/2,label=levels(cut(c(0,round(allpopsizesonly["bonus",],digit=2)),breaks=4,dig.lab=2,ordered_result=T)))
levels(cut(c(0,round(allpopsizesonly["bonus",],digit=2)),breaks=4,dig.lab=2,ordered_result=T))


aa=boxplot(mydata$slope ~ mydata$bonus * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012),ylab="")
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(allpopsizesonly["beta",],breaks=bgpe)))



#epxlore GROWTH RATE PROPERLY (NOT BONUS)
expname=paste0("NewAges_ExploringBirthrateOnGrowth_5000_FS")
params=readRDS(paste0(expname,"_params.RDS"))
birthrate=params[,1]
allpopsizesonly=sapply(1:nrow(params),function(expr){
expfname=paste0(expname,"/singlesimu_s_",expr,".RDS")
if(file.exists(expfname)){
    singlesimu=readRDS(expfname)
    full=which(lengths(singlesimu$comusize)==100)
    pop=singlesimu$popsize
    end=ifelse(length(full)==0,length(pop),full)
    pop=pop[1:end]
    slope=tryCatch(lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2],error=function(e)NA)
    effect=mean((pop[-1]-pop[-end])/pop[-end])
    c(slope=slope,effect=effect,br=birthrate[expr])
}
else{ c(NULL,NULL,NULL,NULL) }
})

allpopsizesonly=do.call("cbind",allpopsizesonly)
par(mar=c(4,6,4,0))
plot(allpopsizesonly["br",]/0.67,allpopsizesonly["effect",],ylim=c(-0.025,0.025),xlim=c(0.2,0.5),xlab="birth rate",ylab=expression(frac(N[t+1]-N[t],N[t])),main="Population Growth wrt base birth rate")

##ECPLORE BONUS AND BETAjj
set.seed(1234)
birtrate=runif(5000,0.0005,0.02)                                                                                   
bonus=runif(5000,0,0.02)                                                                                            
beta=runif(5000,-10,0.2)                                                                                            
params=cbind(bonus,beta)

expname=paste0("BonusExploNewAges_5000_FS_poppaper")
bonusbeta=sapply(1:nrow(params),function(expr){
 expfname=paste0(expname,"/singlesimu_s_",expr,".RDS")
if(file.exists(expfname)){
    singlesimu=readRDS(expfname)
    full=which(lengths(singlesimu$comusize)==100)
    pop=singlesimu$popsize
    end=ifelse(length(full)==0,length(pop),full)
    pop=pop[1:end]
    slope=lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2]
    effect=mean((pop[-1]-pop[-end])/pop[-end])
    c(slope=slope,effect=effect,beta=beta[expr],bonus=bonus[expr])
}
else{ c(NULL,NULL,NULL,NULL) }
})
bonusbeta=do.call(cbind,bonusbeta)
expname=paste0("BonusExploNewAges_5000_FS_poppaper2")
bonusbeta2=sapply(1:nrow(params),function(expr){
 expfname=paste0(expname,"/singlesimu_s_",expr,".RDS")
if(file.exists(expfname)){
    singlesimu=readRDS(expfname)
    full=which(lengths(singlesimu$comusize)==100)
    pop=singlesimu$popsize
    end=ifelse(length(full)==0,length(pop),full)
    pop=pop[1:end]
    slope=lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2]
    effect=mean((pop[-1]-pop[-end])/pop[-end])
    c(slope=slope,effect=effect,beta=beta[expr],bonus=bonus[expr])
}
else{ c(NULL,NULL,NULL,NULL) }
})
bonusbeta2=do.call(cbind,bonusbeta2)
bonusbeta=cbind(bonusbeta2,bonusbeta)

png(file="growth_rate_wrt_b_f.png",height=850,width=1450,pointsize=22,type="cairo")
par(mfrow=c(1,2))
par(mar=c(4,6,4,0))
allpopsizesonlyadj=allpopsizesonly[,(allpopsizesonly["br",]/.67)>0.25]
plot(allpopsizesonlyadj["br",]/0.67,allpopsizesonlyadj["effect",],ylim=c(-0.02,0.025),xlim=c(0.25,0.5),xlab="birth rate",ylab=expression(frac(N[t+1]-N[t],N[t])),main="Population Growth wrt base birth rate")
#,xlab="f",ylab=expression(abline(v=0.322,col="red")
abline(v=0.3224,col="black",lwd=3)
abline(v=0.3224,col="green",lwd=1.5)
par(mar=c(4,3,4,1))
plot(bonusbeta["bonus",],bonusbeta["effect",],ylab="" ,xlab="f",pch=21,main="effect of f on growth rate\n(base birthrate=0.322)",bg=adjustcolor(custom_colors[cut(bonusbeta["beta",],breaks=bgpe,dig.lab=2,ordered_result=T)],.6))
abline(v=c(0,0.005,0.015),col="black",lwd=2.5)
abline(v=c(0,0.005,0.015),col="green",lwd=1)
legend("bottomright",fill=custom_colors,title="beta",legend=levels(cut(bonusbeta["beta",],breaks=bgpe)),bty="n",cex=.8)
dev.off()

mydata=data.frame(beta=cut(bonusbeta["beta",],breaks=bgpe),bonus=cut(bonusbeta["bonus",],breaks=3),slope=bonusbeta["slope.x",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012))
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(bonusbeta["beta",],breaks=bgpe)))


allpopsizesonly1000=readRDS("NewAges_ExploringBonusOnGrowth_1000_500ts.RDS")
allpopsizesonly1000=do.call("cbind",allpopsizesonly1000[lengths(allpopsizesonly1000)==4])
allpopsizesonly=cbind(allpopsizesonly,allpopsizesonly1000)

bgpe=4
custom_colors <- rev(colorRampPalette(c("red", "white", "blue"))(bgpe))


plot(allpopsizesonly["beta",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear fit" ,xlab="beta for 1 traits",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["bonus",],breaks=bgpe)],.4),main="age 18-45",ylim=c(-0.0025,0.012))

par(mfrow=c(1,2))
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="f",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["beta",],breaks=bgpe,dig.lab=2,ordered_result=T)],.6),main="Effective Growth Rate wrt f & beta",ylim=c(-0.0025,0.012),lwd=.4)


mydata=data.frame(beta=cut(allpopsizesonly["beta",],breaks=bgpe,dig.lab=2,ordered_result=T),bonus=cut(allpopsizesonly["bonus",],breaks=4,dig.lab=2,ordered_result=T),slope=allpopsizesonly["effect",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors,xaxt="n",ylab="slope of log transfrom linear fit",xlab="f",outline=T,ylim=c(0.001,0.011))
axis(1,at=(seq(1,16,4)+seq(4,16,4))/2,label=levels(cut(c(0,round(allpopsizesonly["bonus",],digit=2)),breaks=4,dig.lab=2,ordered_result=T)))
levels(cut(c(0,round(allpopsizesonly["bonus",],digit=2)),breaks=4,dig.lab=2,ordered_result=T))


aa=boxplot(mydata$slope ~ mydata$bonus * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012),ylab="")
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(allpopsizesonly["beta",],breaks=bgpe)))
