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

vertical <- function(p1,p2=NULL,tp,tid){
    if(!is.null(dim(p1))){
        p2=p1[2,]
        p1=p1[1,]
    }
    #"F" == 1
    if(p1["sex"]){ m=p2[tid] ; f=p1[tid]}
    else { m=p1[tid] ; f=p2[tid]}
    vert=as.logical(tp$pre[,"v"])
    sexbiascopy(m[vert],f[vert],tp$s[vert])   
}


#' Initialize Neutral Traits Pathways
#'
#' This function initializes the paramater matrix for the traits 
#'
#' @export
#'
#' @examples
#' initNeutralTraitsPathways(1, 5)
initNeutralTraitsPathways <- function(z=9,t_pre=c('v','h','o'),t_post=c('h','o')){
    pre = array(0,dim=c(z,length(t_pre)))
    colnames(pre)=t_pre
    post = array(0,dim=c(z,length(t_post)))
    colnames(post)=t_post
    s = rep(0,z)
    colnames(post)=t_post
    list(pre=pre,post=post,s=s)
}

#' Initialize Adaptive Traits for Communities
#'
#' This function initializes the traits for each community, creating a matrix of traits for migrants and incumbents communities.
#'
#' @param km integer, the number of migrants communities
#' @param ki integer, the number of incumbents communities
#' @param aval named numeric vector with initial values for "m" (migrants) and "i" (incumbents) traits default c("i" = 0, "m" = 1).
#' @param n integer, the number of adaptive traits  default is 3.
#'
#' @return A matrix where each column represents an individual and each row represents a trait, with migrants and incumbents.
#' @export
#'
#' @examples
#' initAdaptiveTraits(1, 5)
initAdaptiveTraits <- function(km,ki,aval=c("i"=0,"m"=1),n=3){

    # Create a vector with ki time aval["i"] for incubant and km time aval["m"] for migrant
    vectraits <- c(rep(aval["i"], ki * n), rep(aval["m"], km * n))

    #  convert vector into a matrix with n rows and (ki+km) columns, unsure cases with zeros
    traits <- matrix(vectraits, ncol = n, byrow = TRUE)
    colnames(traits)=paste0("a",1:n)
    return(traits)
}

initNeutralTraits <- function(N,z=9,traitnames="t",nastart=NULL){
    if(is.null(nastart))initval=sample(c(0,1),z,replace=T)
    else initval=rep(NA,z)
    traits= t(replicate(N,initval))
    colnames(traits)=paste0(traitnames,1:z)
    return(traits)
}
