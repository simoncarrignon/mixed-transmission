## Script to generate all the simpler version of the Figure 2 added in the suplementary material (Fig. S7 and S8)
library(latex2exp)
devtools::load_all()
setwd(file.path(here::here(),"simulations")) #move to the folder with all simulations' results

data(fullpathways) #load fullpathways
pathwaysnames=paste0("c_",1:15)

renametypes=c("A","B and C")

sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)


exprefix="NewPW_TraitTraj_StratTraj_500ts_"

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

