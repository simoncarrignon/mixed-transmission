library(latex2exp)
devtools::load_all()
setwd(file.path(here::here(),"simulations")) #move to the folder with all simulations' results

data(fullpathways) #load fullpathways
pathwaysnames=paste0("c_",1:15)

renametypes=c("A","B & C")

sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)

### First we retrieve the results from all simulation which have been run  in separate folders depending on beta, bonus and rho (the marital rule) in the following folder:
exprefix="NewPW_TraitTraj_StratTraj_500ts_"

bonuses=c(0,3) #bonus to retrieve
rho=0
allcounts=list()
for(ctype in c(1,2)){  ## this is costly as we read the simulation results twice. Could should be optimzed
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
                                  #count nuber of a_i==1, probably overkill
                                  ctypes=apply(one$traitpercomu[[end]][,paste0("a",1:3)]/one$comusize[[end]],1,sum)
                                  #remove dead communities
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


cairo_pdf("Figure3_pertype_f0.pdf",width=14,height=8)

##keep original par() value and the adjust them for the Figure
mardef=par()$mar
mardef[1]=.2
mardef[2]=mardef[2]+1
mardef[c(3,4)]=1.1
par(xpd=F,mar=mardef,mfrow=c(2,1))
 
## graph second line for Type B&C
ctype=1
plot(1,1,xlim=range(positions),ylim=c(0,1),type="n",xaxt="n",ylab=paste("average proportion of 1-variants neutral traits\nin communities of type",renametypes[ctype]),xlab="") 
segments(x0=positions,x1=positions,y0=-1,y1=0.75,lwd=1,col=adjustcolor("grey",.3),lty=5)
colnames(allcounts[[1]])=rep(pathwaysnames[1:5],12)
boxplot(at=positions,allcounts[[1]],col=sexcols[as.character(fullpathways$s[traitsel])],outline=F,ann=F,lwd=.6,ylim=c(0,1),xaxt="n",add=T,boxwex=.80,staplewex=.4,lty=1)

##vertical grey explicit label
text(labels=c("no resocialisation","horizontal transmission probability 0.9","horizontal transmission probability 1","oblique transmission probability 0.9","oblique transmission probability 1"),x=positions[1:5],y=.05,srt=90,cex=.7,adj=c(0,-.2),col=adjustcolor("black",.4),font=3)
#text(labels=c(TeX("no resocialisation"),TeX("horizontal $p_{trans}=0.9$"),TeX("horizontal $p_{trans}=1$"),TeX("oblique $p_{trans}=1$"),TeX("oblique $p_{trans}=0.9$")),x=positions[1:5]-.1,y=.05,srt=90,cex=.8,adj=0,col=adjustcolor("grey",.9),font=3)

##horizontal arrow with label (beta,f)
par(xpd=NA)
betaspos=sapply(0:1,function(ie)positions[c((30*ie)+1,(30*ie)+30)])
bay=0.85
text(apply(betaspos,2,sum)/2,bay,label=c(expression("none ("*beta==-10*")"),expression("strong ("*beta==0*")")) ,pos=3,cex=.9)
text(x=positions[1],bay,label=expression("learning bias ("*beta*"):"),pos=3,cex=.7)
arrows(x0=betaspos[1,],y0=rep(bay,nrow(betaspos)),x1=betaspos[2,],y1=rep(bay,nrow(betaspos)),lwd=1,angle=90,code=3,length=.01)
boy=0.75
bonuspos=sapply(0:3,function(ie)positions[ie*15+c(1,15)])
text(label=paste0(c("none (f =","high (f ="),rep(bonuses*0.005,2),c(")",")")),y=boy,x=apply(bonuspos,2,sum)/2,pos=3,cex=.9)
text(x=positions[1],boy,label="fitness benefit (f):",pos=3,cex=.7)
arrows(x0=bonuspos[1,],y0=rep(boy,nrow(bonuspos)),x1=bonuspos[2,],y1=rep(boy,nrow(bonuspos)),lwd=1,angle=90,code=3,length=.01)

mardef[3]=.2
mardef[1]=2

## graph second line for Type B&C
ctype=2
par(mar=mardef,xpd=F)
plot(1,1,xlim=range(positions),ylim=c(0,1),type="n",xaxt="n",ylab=paste("average proportion of 1-variants neutral traits\nin communities of type",renametypes[ctype]),xlab="")
segments(x0=positions,x1=positions,y0=-1,y1=2,lwd=1,col=adjustcolor("grey",.3),lty=5)
colnames(allcounts[[2]])=rep(pathwaysnames[1:5],12)
boxplot(at=positions,allcounts[[2]],col=sexcols[as.character(fullpathways$s[traitsel])],outline=F,ann=F,lwd=.6,ylim=c(0,1),xaxt="n",add=T,boxwex=.80,staplewex=.4,lty=1)
legend("topright",legend=rev(names(sexcols)),fill=rev(sexcols),title="sex bias",bty="n",bg="white")
par(xpd=NA)
text(positions,y=par("usr")[3]-0.05,labels=TeX(lapply(colnames(limited),function(i)paste("$",i,"$"))),srt=0,cex=.8)
dev.off()
##### END FIGURE 3 =====================


