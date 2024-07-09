## This script keep various function that have beeen use to test


library(latex2exp)
devtools::load_all()
setwd(file.path(here::here(),"simulations")) #move to the folder with all simulations' results

data(fullpathways) #load fullpathways
pathwaysnames=paste0("c_",1:15)

renametypes=c("A","B and C")

sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)

##### FIGURE 2 bis=====================
##mean when bonus is 0
rho=0
beta=-10
bonus=0
exprefix="NewPW_TraitTraj_StratTraj_500ts_"
expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)

# We run through all simulation for beta -10 and bonus =0 to get baseline
traitsfreq=sapply(allsingle.exp,function(expfname){
                      one=readRDS(expfname)
                      end=getSimFull(one)
                      population=NULL
                      if(is.null(one$popwhenfull))
                          population=one$population
                      else
                          population=one$popwhenfull
                      traitsid=paste0("t",seq_along(fullpathways$s))
                      tcount=c()
                      for(s in c(0,1)){
                          #traits biased toward one sex
                          single.bias=traitsid[fullpathways$s == s]
                          #population of this given sex
                          singlesex=population[population[,"sex"]==s,]
                          #count frquencies of traits for these given sex
                          tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                      }       
                      single.bias=traitsid[fullpathways$s == 0.5]
                      tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                      tcount[traitsid]
           })
meanfreq=median(traitsfreq[1,])


#We will not go through all results again, splitting the analysis for both type of communities 

bonuses=c(0,3)
allcounts=list()
for(ctype in c(1,2)){
limited=list()
for(beta in c(-10,0)){
    for(bonus in bonuses){
        expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
        print(expname)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          population=NULL
                          if(is.null(one$popwhenfull))
                              population=one$population
                          else
                              population=one$popwhenfull
                          end=getSimFull(one)
                          ctypes=apply(one$traitpercomu[[end]][,paste0("a",1:3)]/one$comusize[[end]],1,sum)
                          ctypes[is.nan(ctypes)]=NA
                          ctypes[ctypes %in% c(1,2,3)]=2
                          ctypes[ctypes == 0]=1

                          population=population[population[,"community"] %in% which(ctypes==ctype),,drop=F]
                          traitsid=paste0("t",seq_along(fullpathways$s))
                          tcounts=apply(population[,traitsid],2,sum)
                          scounts=table(population[,"sex"])
                          tcount=c()
                          for(s in c(0,1)){
                              #traits biased toward one sex
                              single.bias=traitsid[fullpathways$s == s]
                              #population of this given sex
                              singlesex=population[population[,"sex"]==s,]
                              #count frquencies of traits for these given sex
                              tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                          }       
                          single.bias=traitsid[fullpathways$s == 0.5]
                          tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                          tcount[traitsid]
    })
        traitfreq=t(traitsfreq)
        colnames(traitfreq)=rep(pathwaysnames,3)

        traitsel=c(1:5,16:20,31:35)

        limited=cbind(limited,traitfreq[,traitsel])
    }
}
limited=apply(limited,2,function(u)unlist(unname(u)))
uuu=boxplot(limited,outline=F,ann=F,axes=F,plot=F)
allcounts[[ctype]]=limited
}

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

pdf("Figure3_pertype_f0.pdf",width=14,height=8)
mardef=par()$mar
mardef[1]=.2
mardef[2]=mardef[2]+1
mardef[c(3,4)]=1.1
par(xpd=F,mar=mardef,mfrow=c(2,1))
ctype=1
plot(1,1,xlim=range(positions),ylim=c(0,1),type="n",xaxt="n",ylab=paste("average proportion of 1-variants neutral traits\nin communities of type",renametypes[ctype]),xlab="") #as.roman(ctype)
#abline(v=positions,lwd=3,col=adjustcolor("grey",.3),lty=5)
segments(x0=positions,x1=positions,y0=-1,y1=0.75,lwd=1,col=adjustcolor("grey",.3),lty=5)
colnames(allcounts[[1]])=rep(pathwaysnames[1:5],12)
boxplot(at=positions,allcounts[[1]],col=sexcols[as.character(fullpathways$s[traitsel])],outline=F,ann=F,lwd=.6,ylim=c(0,1),xaxt="n",add=T,boxwex=.80,staplewex=.4,lty=1)
#abline(h=meanfreq,lwd=2,lty=5,col=adjustcolor(1,.5))

text(labels=c("no resocialisation","horizontal transmission probability 0.9","horizontal transmission probability 1","oblique transmission probability 0.9","oblique transmission probability 1"),x=positions[1:5],y=.05,srt=90,cex=.7,adj=c(0,-.2),col=adjustcolor("black",.4),font=3)

#text(labels=c("no resocialisation",TeX("horizontal,$p_{trans}=0.9$"),"horizontal","oblique",TeX("oblique,$p_{trans}=0.9$")),x=positions[1:5]-.1,y=.05,srt=90,cex=.8,adj=0,col=adjustcolor("grey",.9),font=3)

par(xpd=NA)
betaspos=sapply(0:1,function(ie)positions[c((30*ie)+1,(30*ie)+30)])
bay=0.85
text(apply(betaspos,2,sum)/2,bay,label=c("-10","0"),pos=3)
text(x=positions[1],bay,label=expression(beta~":"),pos=3)
arrows(x0=betaspos[1,],y0=rep(bay,nrow(betaspos)),x1=betaspos[2,],y1=rep(bay,nrow(betaspos)),lwd=1,angle=90,code=3,length=.01)
boy=0.75
bonuspos=sapply(0:3,function(ie)positions[ie*15+c(1,15)])
text(label=rep(bonuses*0.005,2),y=boy,x=apply(bonuspos,2,sum)/2,pos=3)
text(x=positions[1],boy,label="f:",pos=3)
arrows(x0=bonuspos[1,],y0=rep(boy,nrow(bonuspos)),x1=bonuspos[2,],y1=rep(boy,nrow(bonuspos)),lwd=1,angle=90,code=3,length=.01)

mardef[3]=.2
mardef[1]=2

ctype=2
par(mar=mardef,xpd=F)
plot(1,1,xlim=range(positions),ylim=c(0,1),type="n",xaxt="n",ylab=paste("average proportion of 1-variants neutral traits\nin communities of type",renametypes[ctype]),xlab="")
#abline(v=positions,lwd=3,col=adjustcolor("grey",.3),lty=5)
segments(x0=positions,x1=positions,y0=-1,y1=2,lwd=1,col=adjustcolor("grey",.3),lty=5)
colnames(allcounts[[2]])=rep(pathwaysnames[1:5],12)
boxplot(at=positions,allcounts[[2]],col=sexcols[as.character(fullpathways$s[traitsel])],outline=F,ann=F,lwd=.6,ylim=c(0,1),xaxt="n",add=T,boxwex=.80,staplewex=.4,lty=1)
legend("topright",legend=rev(names(sexcols)),fill=rev(sexcols),title="sex bias",bty="n",bg="white")
#abline(h=meanfreq,lwd=2,lty=5,col=adjustcolor(1,.5))
par(xpd=NA)
text(positions,y=par("usr")[3]-0.05,labels=TeX(lapply(colnames(limited),function(i)paste("$",i,"$"))),srt=0,cex=.8)
#text(par("usr")[1],y=meanfreq,labels="median prop.\nwhen f=0",cex=.8,pos=2)
dev.off()
##### END FIGURE 2 =====================

##### SUPPL =====================


for(rho in c(0,0.5)){
    pdf(paste0("boxplot_poplevel_full_rho_",rho,".pdf"),height=12,width=15)
    par(mfrow=c(3,4))
    for(bonus in c(0,1,3)){
        for(beta in c(-10,0)){
            expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
            allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
            for(ctype in c(1,2)){
                traitsfreq=sapply(allsingle.exp,function(expfname){
                                      one=readRDS(expfname)
                                      population=NULL
                                      if(is.null(one$popwhenfull))
                                          population=one$population
                                      else
                                          population=one$popwhenfull
                                      end=getSimFull(one)
                                      ctypes=apply(one$traitpercomu[[end]][,paste0("a",1:3)]/one$comusize[[end]],1,sum)
                                      ctypes[is.nan(ctypes)]=NA
                                      ctypes[ctypes %in% c(1,2,3)]=2
                                      ctypes[ctypes == 0]=1

                                      population=population[population[,"community"] %in% which(ctypes==ctype),,drop=F]
                                      traitsid=paste0("t",seq_along(fullpathways$s))
                                      tcounts=apply(population[,traitsid],2,sum)
                                      scounts=table(population[,"sex"])
                                      tcount=c()
                                      for(s in c(0,1)){
                                          #traits biased toward one sex
                                          single.bias=traitsid[fullpathways$s == s]
                                          #population of this given sex
                                          singlesex=population[population[,"sex"]==s,]
                                          #count frquencies of traits for these given sex
                                          tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                                      }       
                                      single.bias=traitsid[fullpathways$s == 0.5]
                                      tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                                      tcount[traitsid]
    })

                traitsel=c(1:5,16:20,31:35)
                traitfreq=t(traitsfreq)[,traitsel]
                colnames(traitfreq)=rep(pathwaysnames[1:5],3)

                boxplot(traitfreq,col=sexcols[as.character(fullpathways$s)][traitsel],ylab="% of population",ylim=c(0,1),main=bquote(rho*":"*.(rho)*", br:0.322 f:"*.(0.005*bonus)*", "*beta*":"*.(beta)),las=3,lwd=.5)
                mtext(paste0("Communitity of Type ",renametypes[ctype]),3,0)
                par(xpd=T)
                #text(1:ncol(traitfreq),y=par("usr")[3]-0.02,labels=lapply(colnames(traitfreq),function(i)TeX(paste("$",i,"$"))),srt=0,cex=.8)
            }

        }
            legend("topright",legend=rev(names(sexcols)),fill=rev(sexcols),title="sex bias",bty="n",bg="white")
    }
    dev.off()
}


#### FIGURE COMPARE F

ctype=2
limited=list()
beta=0
for(bonus in c(0,1,3)){
    expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          population=NULL
                          if(is.null(one$popwhenfull))
                              population=one$population
                          else
                              population=one$popwhenfull
                          end=getSimFull(one)
                          ctypes=apply(one$traitpercomu[[end]][,paste0("a",1:3)]/one$comusize[[end]],1,sum)
                          ctypes[is.nan(ctypes)]=NA
                          ctypes[ctypes %in% c(1,2,3)]=2
                          ctypes[ctypes == 0]=1

                          population=population[population[,"community"] %in% which(ctypes==ctype),,drop=F]
                          traitsid=paste0("t",seq_along(fullpathways$s))
                          tcounts=apply(population[,traitsid],2,sum)
                          scounts=table(population[,"sex"])
                          tcount=c()
                          for(s in c(0,1)){
                              #traits biased toward one sex
                              single.bias=traitsid[fullpathways$s == s]
                              #population of this given sex
                              singlesex=population[population[,"sex"]==s,]
                              #count frquencies of traits for these given sex
                              tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                          }       
                          single.bias=traitsid[fullpathways$s == 0.5]
                          tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                          tcount[traitsid]
    })
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(pathwaysnames,3)

    traitsel=c(1:5,16:20,31:35)

    limited=cbind(limited,traitfreq[,traitsel])
}
limited=apply(limited,2,function(u)unlist(unname(u)))
sexcols[as.character(fullpathways$s)]
uuu=boxplot(limited,outline=F,ann=F,axes=F,plot=F)
boxplot(limited,col=sexcols[as.character(fullpathways$s)][traitsel],ylab="% of population",ylim=c(0,1),main=bquote(rho*":"*.(rho)*", br:0.322 f:"*.(0.005*bonus)*", "*beta*":"*.(beta)),las=3,lwd=.5)

par(mfrow=c(2,3))
boxplot(limited[,c(11,26,41)],col=sexcols[3],ylim=c(0,1))
boxplot(limited[,c(11,26,41)+1],col=sexcols[3],ylim=c(0,1))
boxplot(limited[,c(11,26,41)+2],col=sexcols[3],ylim=c(0,1))
boxplot(limited[,c(11,26,41)+3],col=sexcols[3],ylim=c(0,1))
boxplot(limited[,c(1,16,41)+4],col=sexcols[3],ylim=c(0,1))

dev.new()
par(mfrow=c(2,3))
boxplot(limited[,c(1,16,31)],col=sexcols[1],ylim=c(0,1))
boxplot(limited[,c(1,16,31)+1],col=sexcols[1],ylim=c(0,1))
boxplot(limited[,c(1,16,31)+2],col=sexcols[1],ylim=c(0,1))
boxplot(limited[,c(1,16,31)+3],col=sexcols[1],ylim=c(0,1))
boxplot(limited[,c(1,16,31)+4],col=sexcols[1],ylim=c(0,1))



#### using new simulations with Varying F and Beta
allcounts=list()
for(ctype in c(1,2)){
limited=list()
for(beta in c(0)){
    for(bonus in c(1,3)){
        expname=paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          population=NULL
                          if(is.null(one$popwhenfull))
                              population=one$population
                          else
                              population=one$popwhenfull
                          end=getSimFull(one)
                          ctypes=apply(one$traitpercomu[[end]][,paste0("a",1:3)]/one$comusize[[end]],1,sum)
                          ctypes[is.nan(ctypes)]=NA
                          ctypes[ctypes %in% c(1,2,3)]=2
                          ctypes[ctypes == 0]=1

                          population=population[population[,"community"] %in% which(ctypes==ctype),,drop=F]
                          traitsid=paste0("t",seq_along(fullpathways$s))
                          tcounts=apply(population[,traitsid],2,sum)
                          scounts=table(population[,"sex"])
                          tcount=c()
                          for(s in c(0,1)){
                              #traits biased toward one sex
                              single.bias=traitsid[fullpathways$s == s]
                              #population of this given sex
                              singlesex=population[population[,"sex"]==s,]
                              #count frquencies of traits for these given sex
                              tcount=c(tcount,apply(singlesex[,single.bias],2,sum)/nrow(singlesex))
                          }       
                          single.bias=traitsid[fullpathways$s == 0.5]
                          tcount=c(tcount,apply(population[,single.bias],2,sum)/nrow(population))
                          tcount[traitsid]
    })
        traitfreq=t(traitsfreq)
        colnames(traitfreq)=rep(pathwaysnames,3)

        traitsel=c(1:5,16:20,31:35)

        limited=cbind(limited,traitfreq[,traitsel])
    }
}
limited=apply(limited,2,function(u)unlist(unname(u)))
uuu=boxplot(limited,outline=F,ann=F,axes=F,plot=F)
allcounts[[ctype]]=limited
}

allc1pb1=cbind(allcounts[[1]][,c(11,26,41,56)],allcounts[[2]][,c(11,26,41,56)])
par(mar=c(8,5,1,1))
boxplot(allc1pb1,col=sexcols[3],ylim=c(0,.8) )
mtext(text=rep(c("f 0.005","f 0.015"),4),side=1,line=3,at=1:8)
mtext(text=rep(c("beta -10","beta 0"),2),side=1,line=4,at=c(1.5,3.5,5.5,7.5))
text(labels=c("type A","Type B/C"),x=c(2.5,6.5),y=0.6)
