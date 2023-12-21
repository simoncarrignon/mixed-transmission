tablePerSexAndGroup <- function(is.candidate,pop.group,pop.sex){
    comms=unique(pop.group)
    sapply(c(0,1),function(s)lapply(comms,function(i) which(is.candidate & (pop.group == i) & (pop.sex==s))))
}


matchingSingle <- function(population,maturity){
    potential=population[,"age"]>=maturity & population[,"partner"]<0
    if(sum(population)==0)return(NULL)
    comus=unique(population[potential,"community"])
    if(length(comus)==1)return(NULL)  ## only one community, no marriage allowed
    tpsg=tablePerSexAndGroup(potential,population[,"community"],population[,"sex"])
    if(is.null(dim(tpsg)))return(NULL)
    lcom=apply(tpsg,2,lengths)
    lc1=lcom[,1]
    lc2=lcom[,2]
    pairs=c()
    while(sum(lc1)>0 && sum(lc2)>0){
        candidate.comu=which(lc1>0)
        c1=ifelse(length(candidate.comu)==1,candidate.comu,sample(candidate.comu,1))
        candidate.ind=tpsg[,1][[c1]] #randomly chose an individual from this commu
        i1=ifelse(length(candidate.ind)==1,candidate.ind,sample(candidate.ind,1))
        tpsg[,1][[c1]]=tpsg[,1][[c1]][tpsg[,1][[c1]]!=i1] #remove this individual from the pool
        candidate.comu=which(lc2>0)
        candidate.comu=candidate.comu[candidate.comu!=c1]
        if(length(candidate.comu)>0){
            c2=ifelse(length(candidate.comu)==1,candidate.comu,sample(candidate.comu,1))
            candidate.ind=tpsg[,2][[c2]] #randomly chose an individual from this commu
            i2=ifelse(length(candidate.ind)==1,candidate.ind,sample(candidate.ind,1))
            tpsg[,2][[c2]]=tpsg[,2][[c2]][tpsg[,2][[c2]]!=i2]
            stopifnot(length(unique(population[c(i1,i2),"community"]))==2)
            pairs=rbind(pairs,c(i1,i2))
        }
        lcom=apply(tpsg,2,lengths) ##update left individual
        lc1=lcom[,1]
        lc2=lcom[,2]
    }
    pairs
}

