##ECPLORE BONUS AND BETAjj
devtools::load_all()

#Left Panel GROWTH RATE only (NO BONUS)
expname=here::here("simulations",paste0("NewAges_ExploringBirthrateOnGrowth_5000_FS"))
params=readRDS(paste0(expname,"_params.RDS"))
birthrate=params[,1]
allpopsizesonly=sapply(1:nrow(params),function(expr){
expfname=paste0(expname,"/singlesimu_s_",expr,".RDS")
if(file.exists(expfname)){
    singlesimu=readRDS(expfname)
    pop=singlesimu$popsize
    end=getSimFull(singlesimu)
    pop=pop[1:end]
    slope=tryCatch(lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2],error=function(e)NA)
    effect=mean((pop[-1]-pop[-end])/pop[-end])
    c(slope=slope,effect=effect,br=birthrate[expr])
}
else{ c(NULL,NULL,NULL) }
})

##this 1000 are more simulatino, available on throughton, should be dl
allpopsizesonly1000=readRDS(here::here("simulations","NewAges_ExploringBonusOnGrowth_1000_500ts.RDS"))
allpopsizesonly1000=do.call("cbind",allpopsizesonly1000[lengths(allpopsizesonly1000)==4])
allpopsizesonly=cbind(allpopsizesonly,allpopsizesonly1000)
setwd(file.path(here::here(),"simulations"))
prefixexp="BonusExploNewAges_5000_FS_poppaper2"
params=readRDS(paste0(prefixexp,"_params.RDS"))
expname=prefixexp
bonusbeta=sapply(1:nrow(params),function(expr){
                     expfname=paste0(expname,"/singlesimu_s_",expr,".RDS")
                     if(file.exists(expfname)){
                         singlesimu=readRDS(expfname)
                         pop=singlesimu$popsize
                         end=getSimFull(singlesimu)
                         pop=pop[1:end]
                         slope=lm(y~x,data=cbind.data.frame(y=log(pop),x=seq_along(pop)))$coefficient[2]
                         effect=mean((pop[-1]-pop[-end])/pop[-end])
                         c(slope=slope,effect=effect,params[expr,"beta"],params[expr,"bonus"])
                     }
                     else{ c(NULL,NULL,NULL,NULL) }
})
bonusbeta=do.call(cbind,bonusbeta) # concat all 

bgpe=10
custom_colors <- rev(colorRampPalette(c("red", "white", "blue"))(bgpe))


png(file="growth_rate_wrt_b_f.png",height=850,width=1450,pointsize=22,type="cairo")
par(mfrow=c(1,2))
par(mar=c(4,6,4,0))
allpopsizesonlyadj=allpopsizesonly[,(allpopsizesonly["br",]/.67)>0.25]
plot(allpopsizesonlyadj["br",]/0.67,allpopsizesonlyadj["effect",],ylim=c(-0.02,0.025),xlim=c(0.25,0.5),xlab="birth rate",ylab=expression(frac(N[t+1]-N[t],N[t])),main="")
#,xlab="f",ylab=expression(abline(v=0.322,col="red")
abline(v=0.3224,col="black",lwd=3)
abline(v=0.3224,col="green",lwd=1.5)
par(mar=c(4,3,4,1))
plot(bonusbeta["bonus",],bonusbeta["effect",],ylab="" ,xlab="f",pch=21,main="effect of f on growth rate\n(base birthrate=0.322)",bg=adjustcolor(custom_colors[cut(bonusbeta["beta",],breaks=bgpe,dig.lab=2,ordered_result=T)],.6))
abline(v=c(0,0.005,0.015),col="black",lwd=2.5)
abline(v=c(0,0.005,0.015),col="green",lwd=1)
legend("bottomright",fill=custom_colors,title="beta",legend=levels(cut(bonusbeta["beta",],breaks=bgpe)),bty="n",cex=.8)
dev.off()


