setwd(here::here())

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

par(mar=c(4,6,4,0))
plot(allpopsizesonly["br",]/0.67,allpopsizesonly["effect",],ylim=c(-0.025,0.025),xlim=c(0.2,0.5),xlab="birth rate",ylab=expression(frac(N[t+1]-N[t],N[t])),main="Population Growth wrt base birth rate")

##Right Panel, BONUS AND BETA
devtools::load_all()
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

bgpe=5
custom_colors <- rev(colorRampPalette(c("red", "white", "blue"))(bgpe))

### We will now cut by beeta and bonus to see if beta can get an effect after some level 
mydata=data.frame(beta=cut(bonusbeta["beta",],breaks=10),bonus=cut(bonusbeta["bonus",],breaks=10),slope=bonusbeta["slope.x",])
aa=boxplot(mydata$slope ~ mydata$bonus * mydata$beta,col=custom_colors ,ylim=c(-0.0025,0.012))
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(bonusbeta["beta",],breaks=bgpe)))



plot(allpopsizesonly["beta",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear fit" ,xlab="beta for 1 traits",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["bonus",],breaks=bgpe)],.4),main="age 18-45",ylim=c(-0.0025,0.012))

par(mfrow=c(1,2))
plot(allpopsizesonly["bonus",],allpopsizesonly["slope.x",],ylab="slope of log transfrom linear " ,xlab="f",pch=21,cex=2,bg=adjustcolor(custom_colors[cut(allpopsizesonly["beta",],breaks=bgpe,dig.lab=2,ordered_result=T)],.6),main="Effective Growth Rate wrt f & beta",ylim=c(-0.0025,0.012),lwd=.4)


mydata=data.frame(beta=cut(allpopsizesonly["beta",],breaks=bgpe,dig.lab=2,ordered_result=T),bonus=cut(allpopsizesonly["bonus",],breaks=4,dig.lab=2,ordered_result=T),slope=allpopsizesonly["effect",])
aa=boxplot(mydata$slope ~ mydata$beta * mydata$bonus,col=custom_colors,xaxt="n",ylab="slope of log transfrom linear fit",xlab="f",outline=T,ylim=c(0.001,0.011))
axis(1,at=(seq(1,16,4)+seq(4,16,4))/2,label=levels(cut(c(0,round(allpopsizesonly["bonus",],digit=2)),breaks=4,dig.lab=2,ordered_result=T)))
levels(cut(c(0,round(allpopsizesonly["bonus",],digit=2)),breaks=4,dig.lab=2,ordered_result=T))


aa=boxplot(mydata$slope ~ mydata$bonus * mydata$bonus,col=custom_colors ,ylim=c(-0.0025,0.012),ylab="")
legend("topleft",fill=custom_colors,title="beta",legend=levels(cut(allpopsizesonly["beta",],breaks=bgpe)))


bb=bonusbeta[,bonusbeta["beta",]>-2]
cuts=seq(-2,.2,length.out=10+1)
cutsBns=seq(0,.02,length.out=4+1)
mydata=data.frame(beta=cut(bb["beta",],breaks=cuts,dig.lab=2,ordered_result=T),bonus=cut(bb["bonus",],breaks=4,dig.lab=2,ordered_result=T),slope=bb["slope.x",])

pdf("growthVsBetaF.pdf")
aa=boxplot(mydata$slope ~ mydata$bonus * mydata$beta,las=3,xlab="",col=rev(colorRampPalette(c("red", "white", "blue"))(20)),xaxt="n",outline = F,ylim=c(0.003,0.013))

tickvalue <- 1:80
num_groups <- 4
group_size <- length(tickvalue) / num_groups
mid_indices <- seq(from = group_size/2, by = group_size, length.out = num_groups)
middle_values <- (tickvalue[floor(mid_indices)] + tickvalue[ceiling(mid_indices)]) / 2


lbl=paste0("[",cutsBns[-length(cutsBns)],";",cutsBns[-1],")")
mtext(1,1,at=middle_values,text = lbl)
mtext(1,2,text = "f")
legend("bottomright",fill=rev(colorRampPalette(c("red", "white", "blue"))(20)),legend=round(cuts[-1],digit=2),bty="n",title=expression(beta),cex=.8,ncol=3)

dev.off()

plot(10^bonusbeta["beta",],bonusbeta["effect",],ylab="" ,xlab="f",pch=21,main="effect of f on growth rate\n(base birthrate=0.322)",bg=adjustcolor(custom_colors[cut(bonusbeta["bonus",],breaks=bgpe,dig.lab=2,ordered_result=T)],.6))
