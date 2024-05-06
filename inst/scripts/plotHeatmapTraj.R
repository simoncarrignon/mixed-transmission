sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)
allsexcols=unlist(lapply(sexcols,rep,5))
traits=1:15
mat=matrix(traits,ncol=3,nrow=5)
traits=as.vector(t(mat))

uu=readRDS("alldamnedres.RDS")
for(logfreq in c(0,1)){
png(paste0("distribCommunitiesThroughtTime",ifelse(logfreq==0,"","logfreq"),".png"),width=1000,height=1600,pointsize=22,type="cairo")
par(mfrow=c(5,6),mar=c(1,0,0,1),oma=c(4,4,6,4))
beta=as.character(-10)
bonus=2
tmax=250
for(trait in traits){
#    for(bonus in as.character(c(0,1,3))){
        for(type in 1:2){
            datas=uu[[beta]][[bonus]][,type][[trait]][,tmax:1]
            if(logfreq)datas=apply(datas,2,function(i){new=log((i+1)/sum(i+1));new-log(1/sum(i+1))})
            #datas=apply(datas,2,function(i)(log(i+1)))
            image(datas,ylab=paste0("c",trait-10),xaxt="n",main=paste("Type",as.roman(type)),col=colorRampPalette(c("white",allsexcols[trait],"black"))(100)[1:70],yaxt="n")
            if(trait %in% traits[1:3]) mtext(paste("type",as.roman(type)),3,1)
            if(type == 1 & trait %in% c(1:5)) mtext(bquote(c[.(trait)]),cex=1.4,2,2)

        }
    }
#}
mtext(bquote(f*":"*.(bonus*0.005)*","*beta*":"*.(beta)*.(ifelse(logfreq==0,"",",log prop."))),3,3,outer=T,cex=2)
axis(1)
axis(4,labels=round(seq(tmax,1,length.out=5)),at=seq(0,1,length.out=5))
mtext("time step",4,3)
dev.off()
}

lapply(uu,function(bet)lapply(seq_along(bet[[3]][,1]),function(trait)for(i in 1:2)image((bet[[1]][,i][[trait]])[,200:1],ylab=paste0("c",trait-10),xaxt="n",main=paste("Type",as.roman(i)),col=colorRampPalette(c("white","#1E8F89"))(100),yaxt="n")))

