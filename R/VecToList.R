listToVec <- function(listpop){
    vecpop=matrix(0)
    age=unname(sapply(listpop,"[[","age"))
    partner=unname(sapply(listpop,"[[","partner"))
    canrepro=unname(sapply(listpop,"[[","repro"))
    community=unname(sapply(listpop,"[[","community"))
    cid=rep(-1,N) 
    fid=rep(0,N) 
    sex=abs(as.numeric(as.factor(unname(sapply(listpop,"[[","sex"))))-2)
    id=1:N
    vecpop=cbind(id,age,partner,community,canrepro,cid,fid,sex)
    traits=t(unname(sapply(listpop,"[[","traits")))
    traitsid=paste0("t",1:ncol(traits))
    colnames(traits)=traitsid
    cbind(vecpop,traits)
}
