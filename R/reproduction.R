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
##microbenchmark
##microbenchmark::microbenchmark(sample.int(2)-1, sample(c(0,1),1),times=1000000)


#' Sex-biased copying
#'
#' This function performs sex-biased copying based on bias proba
#'
#' @param ta A vector of values selected with proba 1-sb
#' @param tb A vector of values selected with proba sb
#' @param sb A numeric vector of probabilities (between 0 and 1) indicating the bias  for each traits
#' @return A vector of values selected from `ta` or `tb` based on the bias condition.
#' @examples
#' sexbiascopy(c(t1=0,t2=0,t3=1), c(1,1,0), c(0.1,0.9,0.5))
#' barplot(apply(replicate(100,sexbiascopy(c(t1=0,t2=0,t3=1),c(1,1,0),c(.1,.9,.5))),1,table),legend=T)
#' @export
sexbiascopy <- function(ta,tb,sb){

    stopifnot(length(ta)==length(tb),length(tb)==length(sb))
    sexbias=runif(length(sb))<sb #if bias is only 0 1 runif useless, maye save some time not doing it?
    ifelse(sexbias,tb,ta)
}

#' Vertical transmission with sex bias
#'
#' This function applies the `sexbiascopy` function vertically to a set of traits determined by a vertical preference vector `tp$pre[,"v"]`.
#' It returns a vector of traits where each trait has been subject to sex-biased copying based on the bias specified in `tp$s`.
#'
#' @param ta A vector of traits selected with proba 1-tp$s
#' @param tb A vector of traits selected with proba tp$s
#' @param tp A list containing a matrix `pre` which indicates vertical preferences, and a vector `s` indicating the sex-bias probabilities.
#' @param nametraits An optional string to be used as the prefix for naming the traits.
#'
#' @return A named vector of traits after applying sex-biased copying.
#' @examples
#' # Assuming 'tp' is a predefined list with appropriate structure
#' vertical(c(t1=0,t2=0,t3=1), c(1,1,0), tp, nametraits="trait")
#' @export
#' @note The function assumes that the `tp` list is structured with at least the `pre` matrix and `s` vector as components.

vertical <- function(ta,tb,tp,nametraits="t"){
    traits=rep(NA,length(tp$s))
    if(!is.null(nametraits))names(traits)=paste0(nametraits,1:length(tp$s))
    vert=as.logical(tp$pre[,"v"])
    traits[vert]=sexbiascopy(ta[vert],tb[vert],tp$s[vert])   
    return(traits)
}

