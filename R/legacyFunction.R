ratioPerGroup  <-  function(traits,group){
        allratio=aggregate(traits,by=list(community=group),FUN=getRatio)
        rownames(allratio)=allratio[,1]
        allratio[,-1,drop=F]
}

generateSamples <- function(n,tfreq){
        sapply(tfreq,rbinom,size=1,n=n)
}

selectTraits <- function(pop,pool,traits){
    getRatio(traits)
}

##When commu, we need to match communities
copyTraitsByCommu <- function(wholepop,copy.from,copy.to,traitstocopy,tsb){
    traits=wholepop[copy.from,traitstocopy,drop=F]
    subpop=wholepop[copy.from,,drop=F]
    if(tsb==-1)
        pr=ratioPerGroup(traits,subpop[,"community"])
    else
        pr=ratioPerGroup(traits[ subpop[,"sex"] == tsb,],subpop[subpop[,"sex"] == tsb,"community"])
    apply(pr[as.character(wholepop[copy.to,"community"]),,drop=F],2,rbinom,n=length(copy.to),size=1)
}


copyTraits <- function(wholepop,copy.from,traitstocopy,tsb=tsb){
    if(tsb==-1)
        pr=apply(wholepop[as.logical(copy.from) ,traitstocopy,drop=F],2,getRatio)
    else
        pr=apply(wholepop[copy.from & wholepop[,"sex"] == tsb ,traitstocopy,drop=F],2,getRatio)
    rbinom(length(pr),size=1,prob=pr)
}
