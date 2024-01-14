#' Create a population matrix with specified attributes
#'
#' This function generates a matrix representing a population with each row corresponding to an individual. Attributes include age, community, family IDs, and more. The function allows customization of the population size, age distribution, community, and family structures.
#'
#' @param n Integer; the number of individuals in the population.
#' @param age Numeric vector or NULL; the ages of the individuals. If set to 'random', ages are assigned randomly between 0 and 85. If NULL, all ages are set to 0.
#' @param minid Integer; the starting ID for the population. Used to offset the IDs of individuals.
#' @param cid Numeric vector or NULL; the community IDs for the individuals. If NULL, all individuals are assigned to community -1.
#' @param fid Numeric vector or NULL; the family IDs for the individuals. If NULL, all individuals are assigned a family ID of -1.
#' @param community Numeric vector or NULL; the community numbers for the individuals. If NULL, all individuals are assigned to community 1.
#' @return A matrix with each row representing an individual and columns for 'id', 'age', 'partner', 'community', 'canrepro', 'cid', 'fid', 'justmarried', and 'sex'.
#' @examples
#' newpop(10, age = "random", minid = 100)
#' newpop(5, age = c(25, 30, 35, 40, 45), community = c(1, 2, 2, 3, 3))
#' @export
newpop <- function(n,age=NULL,minid=0,cid=NULL,fid=NULL,community=NULL){

    if(is.null(age)){
        age=rep(0,n)
    }
    else{
        if(length(age)==1 && age=="random") age=sample(0:85,n,replace=T)
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
          "justMarried"=rep(0,n),
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


#' Check Validity of Couples Based on Age and Partner Status
#'
#' This function determines if individuals in a population are part of a valid couple 
#' based on age and partner status. A valid couple is defined as individuals whose 
#' age is greater than or equal to the 'maturity' age and less than the 'endrepro' 
#' age, and who have a partner (indicated by a 'partner' value greater than 0).
#'
#' @param population A data frame representing the population. 
#'                   It must contain the columns 'age' and 'partner'.
#' @param maturity An integer representing the minimum age for reproductive maturity.
#' @param endrepro An integer representing the maximum age for reproduction.
#'
#' @return A logical vector indicating whether each individual in the population 
#'         is part of a valid couple.
#'
#' @examples
#' # maturity and endrepro ages are defined
#' valid_couples <- validCouple(population, maturity=1, endrepro=65)
#'
#' @export
validCouple <- function(population,maturity, endrepro){
		population[,"age"]>=maturity & population[,"age"] < endrepro & population[,"partner"] > 0
}
