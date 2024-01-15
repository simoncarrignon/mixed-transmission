allpopsizesonly=readRDS("pposizeLargerTh.RDS")
par(mfrow=c(1,2))
boxplot(lengths(allpopsiz)~bs,ylab="simu length vs birth rate ",xlab="birth rate",main="extinctions")
finished=allpopsizesonly[lengths(allpopsizesonly)==2000]
finsize=sapply(finished,"[",2000)
boxplot(finsize~round(bs[lengths(allpopsizesonly)==2000],digit=2),ylab="population size at the end of simulation for simulation who finised",xlab="birth rate ",main="pop at end without extinctions")
abline(h=nrow(population),col="red")
text("init pop. size",y=nrow(population)+10,x=1,col="red",pos=3)

allfinal=sapply(allpopsizesonly[lengths(allpopsizesonly)>2],function(i)i[length(i)])
boxplot(allfinal~bs[lengths(allpopsizesonly)>2],ylab="population size at the end of simulation for simulation who finised",xlab="birth rate ",main="pop at end ")
abline(h=nrow(population),col="red")


clg=sort(unique(round(bs,digit=3))) 
gpes=cut(bs,breaks=clg,include.lowest = T)

par(mfrow=c(1,2))
boxplot(finsize~round(bs[lengths(allpopsizesonly)==2000],digit=3),ylab="population size at the end of simulation for simulation who finised",xlab="birth rate ",main="pop at end without extinctions")
abline(h=nrow(population),col="red")
text("init pop. size",y=nrow(population)+10,x=1,col="red",pos=3)
plot(1,1,xlim=c(0,1000),ylim=range(0,1000),type="n",main="population size throught time (median over 200 replicates",ylab="# individuals",xlab="time")
cols=rev(heat.colors(length(levels(gpes))))
cols=rainbow(length(levels(gpes)))
for(gpe in seq_along(levels(gpes))){
    subset=allpopsizesonly[ gpes == levels(gpes)[gpe]]
    compressedts=sapply(1:1000,function(t)sapply(subset[lengths(subset)>2],function(i)i[t]))
    getquant=apply(compressedts,2,quantile,na.rm=T)
    lines(getquant[3,],lwd=2,col=cols[gpe])
    #lines(getquant[2,],lwd=1,col=cols[gpe])
    #lines(getquant[4,],lwd=1,col=cols[gpe])
}

legend("top",lwd=2,col=cols,legend=clg[-1],title="birth rate")
par(mfrow=c(2,3))
cols=rainbow(length(levels(gpes)))
for(gpe in seq_along(levels(gpes))){
    plot(1,1,xlim=c(0,10000),ylim=range(0,1400),type="n",main=paste0("pop, size wrt time b=",clg[-1][gpe]),ylab="# individuals",xlab="time")
    subset=allpopsizesonly[ gpes == levels(gpes)[gpe]]
    sapply(subset,function(i)try(lines(i,col=adjustcolor(cols[gpe],.5),lwd=1)))
}

legend("top",lwd=2,col=cols,legend=clg[-1])


plot(tapply( lengths(allpopsizesonly)!=2000,gpes,sum)/table(gpes) ,type="l",lwd=5)


plot(1,1,xlim=c(0,10000),ylim=range(0,800),type="n",main="population size throught time (median over 200 replicates",ylab="# individuals",xlab="time")
    compressedts=sapply(1:10000,function(t)sapply(allpopsizesonly[lengths(allpopsizesonly)>2],function(i)i[t]))
    getquant=apply(compressedts,2,quantile,na.rm=T)
    lines(getquant[3,],lwd=2,col=cols[gpe])
    #lines(getquant[2,],lwd=1,col=cols[gpe])
    #lines(getquant[4,],lwd=1,col=cols[gpe])
}
