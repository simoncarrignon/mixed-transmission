allpopsizesonly=readRDS("pposizeLargerTh.RDS")
z=36
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

