newpop <- function(n,age=NULL,minid=0,cid=NULL,fid=NULL,community=NULL){

    if(is.null(age)){
        age=rep(0,n)
    }
    else{
        if(age=="random") age=sample(0:85,n,replace=T)
    }
    if(is.null(cid))cid=rep(-1,n)
    if(is.null(community))community=rep(1,n)
    if(is.null(fid))fid=rep(-1,n)
    stopifnot(length(age)==n, length(fid)==n, length(community)==n)
    cbind(
          id=(1:n)+minid,
          age=age,
          partner=rep(-1,n),
          community=community,
          "canrepro"=rep(0,n),
          "cid"=cid,
          "fid"=fid,
          sex=sample(c(0,1),n,replace=T)
    )
}
## Evaluatiting different sampling strategies:
#n=1000
#microbenchmark::microbenchmark(sample(c(0,1),n,replace=T),replicate(n,sample(c(0,1),1)),times=100)
##microbenchmark
##microbenchmark::microbenchmark(sample.int(2)-1, sample(c(0,1),1),times=1000000)




reproduction <- function(p1,p2,tp,i=NULL){
    stopifnot(p1$community==p2$community)
    traits=ifelse(sample(c(0,1),length(tp$s),replace=T),p1$traits,p2$traits)
    if(!is.null(i))id=as.numeric(i)+1
    list(
         id=as.character(id),
         traits=traits,
         age=0,
         parents=c(p1$id,p2$id),
         community=p1$community,
         sex=sample(c("F","M"),1),
         repro=FALSE,
         partner=-1
    )
}



### Deprecated
reproductionVec <- function(p1,p2=NULL,tp,tid,i=NULL,id=NULL){
    id=-1
    if(!is.null(dim(p1))){
        p2=p1[2,]
        p1=p1[1,]
    }
    if(!is.null(i))id=as.numeric(i)+1
    stopifnot(p1["community"]==p2["community"])

    #"F" == 1
    if(p1["sex"]){ m=p2[tid] ; f=p1[tid]}
    else { m=p1[tid] ; f=p2[tid]}
    traits=vertical(m,f,tp)   
    if(!is.null(i))id=as.numeric(i)+1
    c(
      id=id,
      age=0,
      partner=-1,
      community=p1[["community"]],
      "canrepro"=0,
      "cid"=-1,
      "fid"=p1[["cid"]],
      sex=sample(c(0,1),1),
      traits
    )
}


firstop=function(){
    c(
      id=-1,
      age=0,
      partner=-1,
      community=2,
      "canrepro"=0,
      "cid"=-1,
      "fid"=2,
      sex=sample(c(0,1),1)
    )

}

secondop=function(n=10){
    cbind(
    id=rep(-1,n),
      age=rep(0,n),
      partner=rep(-1,n),
      community=rep(2,n),
      "canrepro"=rep(0,n),
      "cid"=rep(-1,n),
      "fid"=rep(2,n),
      sex=sample(c(0,1),n,replace=T)
    )
}

##microbenchmark::microbenchmark(t(replicate(20,firstop())),secondop(20),times=50000)

