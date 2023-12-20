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


### stil very messy, need to write all that down probably
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
initNeutralTraitsPathways <- function(z=9,t_pre=c('v','h','o'),t_post=c('h','o','i')){
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
    if(is.null(nastart))initval=c(0,1)
    else initval=rep(NA,z)
    traits= t(replicate(N,sample(initval,z,replace=T)))
    colnames(traits)=paste0(traitnames,1:z)
    return(traits)
}

getRatio <- function(listtraits) sum(listtraits)/length(listtraits)


#' Get Age Band
#'
#' This function compares ages in `age.peers` with a reference age `age.ref` 
#' based on a specified `threshold`. The type of comparison is determined by the `pos` parameter.
#' 
#' @param age.peers A numeric vector representing peer ages to compare against `age.ref`.
#' @param age.ref A numeric value or vector representing the reference age(s) for comparison.
#' @param threshold A numeric value representing the threshold for age difference.
#' @param pos A character string specifying which peers are selected:
#'        'h' horizontal copy ; the absolute difference in age is within the threshold, 
#'        'o' oblique,  `age.peers` is greater than or equal to `age.ref + threshold`, 
#'        'b' below,  `age.peers` is less than or equal to `age.ref - threshold`.
#'        Defaults to 'h'.
#'
#' @return A logical vector (if age is a unique value) or matrix (if ages is a vector) indicating whether each element in `age.peers` meets 
#'         the criteria specified by `pos` and `threshold` relative to `age.ref`.
#' @examples
getAgeBand <- function(age.peers,age.ref,threshold,pos="h"){
    if(length(age.ref)>1)return(sapply(age.ref,getAgeBand,age.peers=age.peers,threshold=threshold,pos=pos))
    if(pos=="h")return( abs(age.ref-age.peers) <= threshold )
    if(pos=="o") return( (age.ref+threshold) <= age.peers )
    if(pos=="b") return( (age.ref-threshold) >= age.peers )
    if(!(pos %in% c("b","h","o"))) return( NULL)
}


drawFromPool <- function(pool.traits,pool.sex,sexbiases){
    if(is.null(dim(pool.traits)))return(pool.traits)
    es=aggregate(pool.traits,by=list(factor(pool.sex,levels=c(0,1))),FUN=getRatio,drop=F)[,-1,drop=F]
    es[is.na(es)]=0
    as.numeric(runif(ncol(pool.traits))<((1-sexbiases)*es[1,]+(sexbiases)*es[2,]))
}

#' @title Social Learning
#' @description Core social learning routine
#' @param x matrix containing population of agents
#' @param when character defining whether the transmission is pre-marial ('pre') or post-marital ('post')
#' @param  list containing the transmission pathways for each trait (generated using \code{initNeutralTraitsPathways()})
#' @param threshold integer defining age tresholds for distinguishing horrizontal and oblique transmission.
#'
#' @return An updated matrix of the population of agents
#' @export

social.learning <- function(x=NULL,when='pre',pathways,threshold,traitsid=NULL)
{
    ntraits <- length(pathways$s)
    if(is.null(traitsid)){
        traitsid=paste0("t",1:ntraits)
    }
    index.learners=NULL
    if (when=='pre')
        index.learners  <- which(x[,'age']==0)
    if (when=='post')
        index.learners  <- which(x[,'justMarried']==1 & x[,'cid']!=-1)
    if(length(index.learners)>0)
    {
        used.pathway=colnames(pathways[[when]])[apply(pathways[[when]],2,sum)>0]
        for(pw in used.pathway )
        {
            pw.traits=traitsid[which(pathways[[when]][,pw]==1)]   #select all traits that goes throught this pathway
            pool.teacher.age= NULL
            if(length(unique(x[index.learners,"age"]))==1)ages=unique(x[index.learners,"age"])
            else ages=x[index.learners,"age"]

            pool.teacher.age=getAgeBand(age=ages,subpop=x,threshold=threshold,pos=pw)

            if(is.null(dim(pool.teacher.age)))
            {
                if(all(!pool.teacher.age))
                    pool.teacher.age=NULL
            }
            else{
                ##remove self learning?
                #diag(pool.teacher.age[index.learners,])=FALSE
                n.teachers=apply(pool.teacher.age,2,sum)
                index.learners=index.learners[n.teachers>0]
                pool.teacher.age=pool.teacher.age[,n.teachers>0,drop=F]
            }

            if(length(index.learners)>0 && !all(pool.teacher.age))
            {
                commu=sapply(x[index.learners,"community"],function(i)x[,"community"]==i)
                ##remove self learning?
                #diag(commu[index.learners,])=FALSE

                print("pool")
                print(dim(pool.teacher.age))
                print("commu")
                print(dim(commu))
                pool.teacher.age.commu=commu&pool.teacher.age
                stopifnot(!is.null(pool.teacher.age.commu))
                stopifnot(length(dim(pool.teacher.age.commu))>0)
                n.teachers=apply(pool.teacher.age.commu,2,sum)
                index.learners=index.learners[n.teachers>0]
                pool.teacher.age.commu=pool.teacher.age.commu[,n.teachers>0,drop=F]

                #if(is.null(dim(x))){print(index.learners)}
                if(length(index.learners)>0 && sum(n.teachers)>0)
                {
                    pools=apply(pool.teacher.age.commu,2,function(pool)x[pool,c("sex",pw.traits),drop=F],simplify=F)
                    stopifnot(length(dim(pool.teacher.age.commu))>0)
                    index.learners=index.learners[n.teachers>0]
                    x[index.learners,pw.traits] =t(sapply(pools,function(pool)drawFromPool(pool.traits=pool[,pw.traits],pool.sex=pool[,"sex"],sexbiases=pathways$s[pathways[[when]][,pw]==1])))  
                }
            }
        }
    }
    return(x)
}

