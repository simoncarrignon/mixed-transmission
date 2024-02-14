

#using a the result of a simulation
#return the type of comu (0,1,2,3) 
getCommuType <- function(exp){
    comusize=exp$comusize
    ncomu=length(comusize[[length(comusize)]])
    sapply(1:ncomu,function(com){
               st=min(which(!is.na(sapply(comusize,function(size)size[com]))))
               sum(exp$traitpercomu[[st]][com,paste0("a",1:3)]/comusize[[st]][com])
    })
}

#using a the result of a simulation
#return the type of comu (0,1,2,3) 
getCountBySexAndCommu <- function(population,pathways){
    traitsid=paste0("t",seq_along(pathways$s))
    freqcom=c()
    wholecomus=sort(unique(population[,"community"])) 
    for(s in c(0,1)){
        single.bias=traitsid[pathways$s == s]
        singlesex=population[population[,"sex"]==s,]
        singlesex.comus=factor(singlesex[,"community"],levels=wholecomus)
        freqcom=cbind(freqcom,xtabs(singlesex[,single.bias] ~ singlesex.comus)/as.numeric(table(singlesex.comus)))
    }       
    single.bias=traitsid[pathways$s == 0.5]
    freqcom=cbind(freqcom,xtabs(population[,single.bias] ~ population[,"community"])/as.numeric(table(population[,"community"])))
    freqcom[,traitsid]
}

extractResults <- function(expname,pathways=pathways,traitsel=NULL){
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    alltraitComu=lapply(allsingle.exp,function(expfname){
                            print(expfname)
                            one=readRDS(expfname)
                            population=NULL
                            if(is.null(one$popwhenfull))
                                population=one$population
                            else
                                population=one$popwhenfull
                            getCountBySexAndCommu(population,pathways)
    })
    lapply(traitsel,function(ti) unlist(lapply(alltraitComu,function(singex)singex[,ti])))
}


getDensities <- function(traits,bw=0.05){
    if(is.null(traits))return(NULL)
    traits.dens=density(traits,na.rm=T,from=0,to=1,bw=bw)
    y=c(0,traits.dens$y,0)
    x=c(0,traits.dens$x,1)
    return(cbind(x,y/max(y)))
}
