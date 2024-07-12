library(parallel)
sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)

devtools::load_all()
expname=paste0("BonusExploNewAges_11550_FS_poppaperNewRanges_rho0")
bonus=round(seq(0,0.02,length.out =11),digits=3)
beta=round(seq(-3,1,length.out=21),digits=3)
params= do.call("rbind",lapply(1:50,function(i)expand.grid(bonus,beta)))
allres=matrix(NA,nrow=length(beta),ncol=length(bonus))
colnames(allres)=as.character(bonus)
rownames(allres)=as.character(beta)

cl<-makeCluster(11,type="FORK",outfile="log.txt")
allallres=list()
allallcol=list()
allexpe=list(C1=c(1,16,31),
             C2=c(1,16,31)+1,
             C3=c(1,16,31)+2,
             C4=c(1,16,31)+3,
             C5=c(1,16,31)+4)
for(ne in names(allexpe)){
    for(traitsel in allexpe[[ne]]){
        allres[,]=NA
        for(be in beta){
            print(paste(ne,be,traitsel))
            cur=parLapply(cl,bonus,function(bo,be,traitsel){

                singlep=which(params[,1]==bo & params[,2]==be)
                uu=extractResults(expname,traitsel=traitsel,pathways=fullpathways,diffexp=T,params=singlep,log=T,type=T)[[2]]
            } ,be=be,traitsel=traitsel)
            allres[as.character(be),]=sapply(cur,function(i)mean(i[[1]],na.rm=T))
        }
        allallres[[paste0(ne,"_distribFBETA_pbias",fullpathways$s[traitsel])]]=allres
        allallcol[[paste0(ne,"_distribFBETA_pbias",fullpathways$s[traitsel])]]=sexcols[as.character(fullpathways$s[traitsel])]
    }
}
stopCluster(cl)

save(file="allmatricsTrait.RDS",allallres)

#png("A4_art.png",height=3508,width=2480,pointsize=60,bg="black",type="cairo")
png("A3_art.png",height=4961,width=3508,pointsize=90,bg="black",type="cairo")
par(mar=rep(.2,4),oma=c(1,1.5,2,1.5),mfrow=c(5,3),bg="black")
lapply(names(allallres),function(nalr){
           allres=allallres[[nalr]]
plot(1,1,xlim=range(beta)*.7,ylim=0.001+range(bonus)*.7,axes=F,ann=F)
.filled.contour(x=beta,y=bonus,z=allres,col = colorRampPalette(c("white",allallcol[[nalr]],"black"))(110)[1:101],levels=pretty(c(0.16,.62),100,xlim=range(beta),ylim=range(bonus)))#,main=paste(ne," pbias=",fullpathways$s[traitsel]),zlim=c(0.16,.62),ann=F,axes=F,key.axes=F)
box()
            })
mtext(3,0.5,outer=T,col="white",text="\\subsection{Impact of $f_i$ and FIXME} %find a better title",cex=.8,font=4,family = "Bookman" )
mtext(1,-0.5,outer=T,col="white",text="Carrignon, Crema, Kandler and Shennan 2024",cex=.3,font=4,family = "Bookman",adj=1)
dev.off()

