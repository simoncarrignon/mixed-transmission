fullpathways=readRDS("inst/scripts/fullpatways.RDS")


pathwaysnames=c(
           "c1","c2_{p_{trans}=0.9}","c2","c3_{p_{trans}=0.9}","c3",
           "c4","c5_{p_{trans}=0.9}","c5","c6_{p_{trans}=0.9}","c6",
           "c7","c8_{p_{trans}=0.9}","c8","c9_{p_{trans}=0.9}","c9")

pathwaysnames=c(
           "c_1","c_{2},p_{transmit}=0.9","c_2","c_{3},p_{transmit}=0.9","c_3",
           "c_4","c_{5},p_{transmit}=0.9","c_5","c_{6},p_{transmit}=0.9","c_6",
           "c_7","c_{8},p_{transmit}=0.9","c_8","c_{9},p_{transmit}=0.9","c_9")




sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)

##### FIGURE 2 =====================
##mean when bonus is 0
rho=0
beta=-10
bonus=0
expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
traitsfreq=sapply(allsingle.exp,function(expfname){
                      one=readRDS(expfname)
                      end=getSimFull(one)
                      traitsid=paste0("t",seq_along(pathways$s))
                      tcount=c()
                      for(s in c(0,1)){
                          #traits biased toward one sex
                          single.bias=traitsid[pathways$s == s]
                          #population of this given sex
                          singlesex=population[population[,"sex"]==s,]
                          #count frquencies of traits for these given sex
                          tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                      }       
                      single.bias=traitsid[pathways$s == 0.5]
                      tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                      tcount[traitsid]
           })
meanfreq=median(traitsfreq[1,])


limited=c()
for(beta in c(-10,0)){
    for(bonus in c(1,3)){
        expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          population=NULL
                          end=getSimFull(one)
                          traitsid=paste0("t",seq_along(pathways$s))
                          tcounts=apply(population[,traitsid],2,sum)
                          scounts=table(population[,"sex"])
                          tcount=c()
                          for(s in c(0,1)){
                              #traits biased toward one sex
                              single.bias=traitsid[pathways$s == s]
                              #population of this given sex
                              singlesex=population[population[,"sex"]==s,]
                              #count frquencies of traits for these given sex
                              tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                          }       
                          single.bias=traitsid[pathways$s == 0.5]
                          tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                          tcount[traitsid]
    })
        traitfreq=t(traitsfreq)
        colnames(traitfreq)=rep(pathwaysnames,3)

        traitsel=c(1:5,16:20,31:35)

        limited=cbind(limited,traitfreq[,traitsel])
    }
}
uuu=boxplot(limited,outline=F,ann=F,axes=F,plot=F)

# Calculate positions with a gap after every 6 boxes
positions <- c()
gap_size <- 2  # Define the size of the gap
for (i in 1:ncol(limited)) {
 if (i %% 15 == 1 && i != 1){
    positions <- c(positions, positions[length(positions)] + gap_size + 1) # Add a gap before starting the next set
  } 
 else if (i %% 5 == 1 && i != 1){
    positions <- c(positions, positions[length(positions)] + .5 + 1) # Add a gap before starting the next set
  } else {
    positions <- c(positions, ifelse(i == 1, i, positions[length(positions)] + 1))
  }
}

png("Figure2.png",width=1200,height=600,type="cairo",pointsize=16)
mardef=par()$mar
mardef[1]=1.1
mardef[2]=mardef[2]+0.4
mardef[c(3,4)]=1.1
par(xpd=F,mar=mardef)
plot(1,1,xlim=range(positions),ylim=c(0,1),type="n",xaxt="n",ylab="proportion",xlab="")
#abline(v=positions,lwd=3,col=adjustcolor("grey",.3),lty=5)
segments(x0=positions,x1=positions,y0=-1,y1=0.75,lwd=3,col=adjustcolor("grey",.3),lty=5)
colnames(limited)=rep(pathwaysnames[1:5],12)
boxplot(at=positions,limited,col=sexcols[as.character(pathways$s[traitsel])],outline=F,ann=F,lwd=.6,ylim=c(0,1),xaxt="n",add=T,boxwex=.80,staplewex=.4,lty=1)
legend("topright",legend=names(sexcols),fill=sexcols,title="sex bias",bty="n",bg="white")
abline(h=meanfreq,lwd=2,lty=5,col=adjustcolor(1,.5))
par(xpd=NA)
betaspos=sapply(0:1,function(ie)positions[c((30*ie)+1,(30*ie)+30)])
bay=0.85
text(apply(betaspos,2,sum)/2,bay,label=c("-10","0"),pos=3)
text(x=positions[1],bay,label=expression(beta~":"),pos=3)
arrows(x0=betaspos[1,],y0=rep(bay,nrow(betaspos)),x1=betaspos[2,],y1=rep(bay,nrow(betaspos)),lwd=1,angle=90,code=3,length=.01)
boy=0.75
bonuspos=sapply(0:3,function(ie)positions[ie*15+c(1,15)])
text(label=rep(c(0.005,0.015),2),y=boy,x=apply(bonuspos,2,sum)/2,pos=3)
text(x=positions[1],boy,label="f:",pos=3)
arrows(x0=bonuspos[1,],y0=rep(boy,nrow(bonuspos)),x1=bonuspos[2,],y1=rep(boy,nrow(bonuspos)),lwd=1,angle=90,code=3,length=.01)
text(positions,y=par("usr")[3]-0.02,labels=TeX(lapply(colnames(limited),function(i)paste("$",i,"$"))),srt=0,cex=.8)
text(par("usr")[1],y=meanfreq,labels="median prop.\nwhen f=0",cex=.8,pos=2)
dev.off()
##### END FIGURE 2 =====================

##### SUPPL =====================


for(rho in c(0,0.5)){
    pdf(paste0("boxplot_poplevel_full_rho_",rho,".pdf"),height=12,width=8)
    par(mfrow=c(3,2))
    for(bonus in c(0,1,3)){
        for(beta in c(-10,0)){
            expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
            allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
            traitsfreq=sapply(allsingle.exp,function(expfname){
                                  one=readRDS(expfname)
                                  population=NULL
                                  if(is.null(one$popwhenfull))
                                      population=one$population
                                  else
                                      population=one$popwhenfull
                                  traitsid=paste0("t",seq_along(pathways$s))
                                  tcounts=apply(population[,traitsid],2,sum)
                                  scounts=table(population[,"sex"])
                                  tcount=c()
                                  for(s in c(0,1)){
                                      #traits biased toward one sex
                                      single.bias=traitsid[pathways$s == s]
                                      #population of this given sex
                                      singlesex=population[population[,"sex"]==s,]
                                      #count frquencies of traits for these given sex
                                      tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                                  }       
                                  single.bias=traitsid[pathways$s == 0.5]
                                  tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                                  tcount[traitsid]
    })
            traitfreq=t(traitsfreq)
            colnames(traitfreq)=rep(pathwaysnames,3)

            boxplot(traitfreq,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,1),main=paste0("rho:",rho,", br:0.216 bonus:",0.005*bonus,", beta:",beta),las=3,lwd=.5)

        }
    }
    dev.off()
}



#### LEGACY BOXPLOT
for(rho in c(0.5)){
pdf(file=paste0("rho_",rho,".pdf"),width=27,height=18)
par(mfrow=c(3,3))
for(beta in c(-10,0)){
for(bonus in c(0,1,3)){
    try(
        {
            exprefix="NewPW_TraitTraj_StratTraj_500ts_"

    expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          if(any(lengths(one$comusize)==100))
                              end=min(which(lengths(one$comusize)==100))
                          else
                              end=length(one$popsize)
                          one$traitsumary[end,]/one$popsize[end]
    })
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(c("c1","c2_0.9","c3_0.9","c2_1","c3_1"),9)
    cols=c('#EE5A45', '#D4D6B9','#1E8F89')
    names(cols)=c(0,.5,1)
    par(xpd=F)
    boxplot(traitfreq,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,1),main=paste0("rho:",rho,", br:0.216 bonus:",0.005*bonus,", beta:",beta),las=3)
    bns=as.numeric(sub(".*_","",expname))
    bns=ifelse(is.na(bns),bns,1)
    #mtext(side=1,line=3,text=paste0("freq. at end rho=1, br=0.216 maxbonus=",0.005*bonus,", beta ",beta),cex=1.5)
    par(xpd=NA)
    arrows(x0=seq(1,45,5),y0=rep(max(traitfreq),9),x1=seq(1,45,5)+4,y1=rep(max(traitfreq),9),lwd=2,angle=90,code=3,length=.1)
    text(((seq(1,45,5)+3)+seq(1,45,5))/2,max(traitfreq),c("v","h","o"),pos=3)
    text(0,max(traitfreq),"pre-marital\n social learning",pos=2)
    text(0,-.1,"post-marital\n social learning",pos=2)
    legend("topright",fill=cols,legend=names(cols),title="sex bias")
        }
    )
}
}
dev.off()
}
#popsize:
