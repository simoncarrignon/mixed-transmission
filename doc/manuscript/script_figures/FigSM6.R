#This script is use to extract the information need to generate the heatmap of the % of neutral traits in the final population for all combination of f and $\beta$.
## getting back the data itself can be long and can be slighlty parallelized, we thus provide, aside  the folder 'BonusExploNewAges_11550_FS_poppaperNewRanges_rho0' with the raw data, a RDS file `allmatricsTrait.RDS` that store the results of the for loop

library(parallel)
sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)

devtools::load_all()
setwd(file.path(here::here(),"simulations"))
## data extraction, can be skipped to dataviz below
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

#save(file="allmatricsTrait.RDS",allallres)

allres=readRDS(file="allmatricsTrait.RDS")

#dataviz ================

traitsel=c(1,7,13)  #which traits will be plotted
sexcols=c('#EE5A45', '#D4D6B9','#1E8F89') #base color for each sex
names(sexcols)=c(0,.5,1)
sb=gsub(pattern="C.*pbias",replacement="",names(allres)) #use the name given in the previous step to find the traits id while remove the part that will be color-coded
tn=gsub("C","",gsub(pattern="_distrib.*",replacement="",names(allres)))


f=as.numeric(colnames(allres[[1]]))
betas=as.numeric(rownames(allres[[1]]))
i=1
mlay=matrix(ncol=4,nrow=3)
mlay[,]=1:length(mlay)
mlay=t(mlay)
cn=1:3

#cairo_pdf("FBETA_heatmap_3PW_3pbias.pdf",width=15,height=12,pointsize=12) #to reproduce the exact image in the papaer
layout(mlay,heights=c(1,1,1,.35))
par(mar=c(1,1,1,1),oma=c(2,7,4,0))
for(t in  traitsel){
    for(s in 0:2){
        plot(NA,ylim=range(f),xlim=range(betas),type="n",ann=F,axes=F)
        .filled.contour(x=betas,y=f,z=allres[[t+s]],col=colorRampPalette(c("white",sexcols[as.character(sb[t+s])],"black"))(120)[1:110],levels=seq(0.1,1,length.out=101))
        #image(x=betas,y=f,z=allres[[t+s]],col=colorRampPalette(c("white",sexcols[as.character(sb[t+s])],"black"))(120)[10:110],levels=pretty(range(unlist(allres)),100))
        if(s == 0){
            axis(2)
            mtext(2,2,text=expression(f))
            mtext(2,5,text=bquote(c[.(tn[t])]),cex=1.2,font=2)
        }
        if(t==traitsel[length(traitsel)]){
            axis(1)
            mtext(1,3,text=expression(beta))
        }
        if(t==1){
            mtext(3,1,text=bquote(p[bias]==.(sb[s+1])),cex=1.1,font=2)
        }

    }
}


for(l in 1:3){
    par(mar=c(2,4,4,4))
    m=par()$mar
    image(x=pretty(range(unlist(allres)),100),y=1,z=as.matrix(pretty(range(unlist(allres)),100)),col=colorRampPalette(c("white",sexcols[as.character(sb[l])],"black"))(120)[10:110],zlim=range(unlist(allres)),ann=F,axes=F,xlab=paste0("mean percentage of ",cn[l],"=1 by communities"))
    axis(1)
    mtext(1,3,text=paste0("mean percentage of $c_i=1$ by communities"))
}
#dev.off()


# horizontal layout, hasn't been used:
f=as.numeric(colnames(allres[[1]]))
betas=as.numeric(rownames(allres[[1]]))
i=1
mlay=matrix(ncol=4,nrow=3)
mlay[,]=1:length(mlay)
#mlay=t(mlay)
layout(mlay,width=c(1,1,1,.35))
par(mar=c(1,1,1,1),oma=c(4,6,6,0))
for(t in  traitsel){
    for(s in 0:2){
        plot(NA,ylim=range(f),xlim=range(betas),type="n",ann=F,axes=F)
        .filled.contour(x=betas,y=f,z=allres[[t+s]],col=colorRampPalette(c("white",sexcols[as.character(sb[t+s])],"black"))(100)[10:110],levels=pretty(range(unlist(allres)),100))
        if(s == 0){
            mtext(3,5,text=tn[t],cex=1.1,font=2)
        }
        if(s==2){
            axis(1)
            mtext(1,3,text=expression(beta))
        }
        if(t==1){
            axis(2)
            mtext(2,5,text=bquote(p[bias]==.(sb[l+1])),cex=1.1,font=2)
            mtext(2,2,text=expression(f))
        }

    }
}

cn=1:3
for(l in 1:3){
    m=par()$mar
    par(mar=c(2,2,2,2))
    image(y=pretty(range(unlist(allres)),100),x=1,z=t(as.matrix(pretty(range(unlist(allres)),100))),col=colorRampPalette(c("white",sexcols[as.character(sb[l])],"black"))(120)[10:110],zlim=range(unlist(allres)),ann=F,axes=F,xlab=paste0("mean percentage of ci=1 by communities"))
    axis(1)
    par(mar=m)
}
