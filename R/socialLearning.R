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
#' barplot(apply(replicate(100,sexbiascopy(c(t1=0,t2=0,t3=1),c(1,1,0),c(.1,.9,.5))),1,table),legend=TRUE)
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
#' @param z number of traitsj
#' @param t_pre pathway pre marital
#' @param t_post pathway post marital
#'
#' @examples
#' generatePathways(1, 5)
generatePathways <- function(z=9,t_pre=c('v','h','o'),t_post=c('h','o','i')){
    pre = array(0,dim=c(z,length(t_pre)))
    colnames(pre)=t_pre
    post = array(0,dim=c(z,length(t_post)))
    colnames(post)=t_post
    s = rep(.5,z) #no sex bias by defaul
    tr = rep(1,z) #resocialisaiton by default
    colnames(post)=t_post
    list(pre=pre,post=post,s=s,tr=tr)
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


#' Generate a Matrix of Traits for a Population
#'
#' This function creates a matrix representing various traits for a population. 
#' Each row in the matrix represents an individual, and each column represents a different trait. 
#' The value of traits are initialized using initval 
#'
#' @param N An integer representing the number of individuals in the population.
#' @param z An optional integer representing the number of traits per individual, defaulting to 9.
#' @param traitnames A string representing the base name for the traits, which will be concatenated with their index.
#' @param initval An optional parameter; if a signle number all traits are initialised with this value, if vector of length > 1 values are sampled from the vector 
#'
#' @return A matrix of size N x z, where each row represents an individual and each column a trait.
#'         The traits are named according to 'traitnames' and their index.
#'
#' @examples
#' # Generate a 5 traits matrix for a population of 100 individuals
#' traits_matrix <- generateTraitsMatrix(100, z=5)
#'
#' @export
generateTraitsMatrix <- function(N,z=9,traitnames="t",initval=c(0,1)){
    if(length(initval)==1)
        traits= t(replicate(N,rep(initval,z,replace=T)))
    else
        traits= t(replicate(N,sample(initval,z,replace=T)))
    if(z==1)traits=t(traits)
    colnames(traits)=paste0(traitnames,1:z)
    return(traits)
}

#' ratio of traits equal to 1 other the total number of trait
#'
#' @param listtraits a vector of traits 0 or 1
#' @return the frequencie of 1 over 0 in listtraits 
#'
#' @export
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
#' @importFrom stats  aggregate  
getAgeBand <- function(age.peers,age.ref,threshold,pos="h"){
    if(!(pos %in% c("b","h","o"))) return( NULL)
    if(length(age.ref)>1)return(sapply(age.ref,getAgeBand,age.peers=age.peers,threshold=threshold,pos=pos))
    if(pos=="h")return( abs(age.ref-age.peers) <= threshold )
    if(pos=="o") return( (age.ref+threshold) <= age.peers )
    if(pos=="b") return( (age.ref-threshold) >= age.peers )
}

#drawFromPool <- function(pool.traits,pool.sex,sexbiases){
#    if(is.null(dim(pool.traits)) && length(pool.traits)==length(sexbiases))return(pool.traits)
#    stopifnot(length(dim(pool.traits))>0)
#    es=aggregate(pool.traits,by=list(factor(pool.sex,levels=c(0,1))),FUN=getRatio,drop=F)[,-1,drop=F]
#    es[is.na(es)]=0
#    as.numeric(runif(ncol(pool.traits))<((1-sexbiases)*es[1,]+(sexbiases)*es[2,]))
#}

drawFromPool <- function(pool.traits, pool.sex, sexbiases) {
    if (is.null(dim(pool.traits)) && length(pool.traits) == length(sexbiases)) {
        return(pool.traits)
    }
    stopifnot(length(dim(pool.traits)) > 0)

    # Pre-calculate dimensions
    ncolTraits = ncol(pool.traits)

    es = aggregate(pool.traits, by = list(factor(pool.sex, levels = c(0, 1))), FUN = getRatio, drop = FALSE)
    es = es[, -1, drop = FALSE]

    es[is.na(es)] = 0 #if ratio are na it means that no-one from with this sex possed the traits, thus there is no-one to copy from.

    # Vectorized operation for final calculation
    ratios = (1 - sexbiases) * es[1, ] + sexbiases * es[2, ]
    as.numeric(runif(ncolTraits) < ratios)
}

#' @title Social Learning
#' @description Core social learning routine
#' @param x matrix containing population of agents
#' @param when character defining whether the transmission is pre-marial ('pre') or post-marital ('post')
#' @param pathways list containing the transmission pathways for each trait (generated using \code{generatePathways()})
#' @param threshold integer defining age tresholds for distinguishing horrizontal and oblique transmission.
#' @param traitsid a vector with the name/index of the traits of the traits 
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
            pw.id=which(pathways[[when]][,pw]==1)
            pw.trait.names=traitsid[pw.id]   #select all traits that goes throught this pathway
            pool.teacher.age= NULL
            if(length(unique(x[index.learners,"age"]))==1)ages=unique(x[index.learners,"age"])
            else ages=x[index.learners,"age"]

            pool.teacher.age=getAgeBand(age.ref=ages,age.peers=x[,"age"],threshold=threshold,pos=pw)

            if(!is.null(dim(pool.teacher.age))){
                n.teachers=apply(pool.teacher.age,2,sum)
                index.learners=index.learners[n.teachers>0]
                pool.teacher.age=pool.teacher.age[,n.teachers>0,drop=F]
            }

            if(length(index.learners)>0 && !is.null(pool.teacher.age) )
            {
                commu=sapply(x[index.learners,"community"],function(i)x[,"community"]==i)
                ##remove self learning
                if(dim(commu)[2]==1)
                    commu[index.learners,]=FALSE
                else
                    diag(commu[index.learners,])=FALSE
                pool.teacher.age.commu=commu&pool.teacher.age
                stopifnot(!is.null(pool.teacher.age.commu))
                stopifnot(length(dim(pool.teacher.age.commu))>0)
                n.teachers=apply(pool.teacher.age.commu,2,sum)
                index.learners=index.learners[n.teachers>0]
                pool.teacher.age.commu=pool.teacher.age.commu[,n.teachers>0,drop=F]

                if(length(index.learners)>0 && sum(n.teachers)>0)
                {
                    pools=apply(pool.teacher.age.commu,2,function(pool)x[pool,c("sex",pw.trait.names),drop=F],simplify=F)
                    stopifnot(length(dim(pool.teacher.age.commu))>0)
                    if(length(index.learners)>0){
                        newtraits=t(sapply(pools,function(pool)drawFromPool(pool.traits=pool[,pw.trait.names,drop=F],pool.sex=pool[,"sex",drop=F],sexbiases=pathways$s[pw.id])))  
                        noresocial=(pathways$tr[pw.id]==0)
                        if(any(noresocial)){
                            newtraits[,noresocial]=x[index.learners,pw.trait.names[noresocial]]
                        }
                        probaresocial=(pathways$tr[pw.id]>0 & pathways$tr[pw.id] < 1 )
                        if(any(probaresocial)){
                            p_tr=pathways$tr[probaresocial]
                            transmit=sapply(p_tr,rbinom,n=length(index.learners),size=1)
                            newtraits[,probaresocial]=ifelse(transmit,newtraits[,probaresocial],x[index.learners,pw.trait.names[probaresocial]])
                        }

                        x[index.learners,pw.trait.names] =newtraits
                    }
                }
            }
        }
    }
    return(x)
}


# beta the paramater to fine tune the impact of size of migrants population
# migrants, a vector of size C of number of migrants comming from each communitys
# adaptivetraits   matrix of zie C x Z
# N the number of individual of the population wher migrants are comming
probaAdoptionAdaptiveTraits <- function(beta,migrants,N,adaptivetraits){
			kc=adaptivetraits * migrants
			ks=apply(kc,2,sum)
            ks^(1-beta)/(ks^(1-beta)+(N-ks)^(1-beta))
}

conf <- function(ks,beta,N) ks^(1-beta)/(ks^(1-beta)+(N-ks)^(1-beta))

#We assume here than k.size includes allready all migrants. Thus whencomputing the probability to adopt the trait no present in the population we compare the percentage of people who have _the other_ traits to all those who have the original traits, including the poulation that migrated from other communities with the same traits.
updateTraits <- function(k,k.size,alltraits,migrantscount,beta){
	k.traits=alltraits[k,]
	migrant.traits=alltraits
	migrant.traits=k.traits!=alltraits 
	proba=probaAdoptionAdaptiveTraits(beta=beta,N=k.size,adaptivetraits=migrant.traits,migrants=migrantscount)
	adopt=proba>runif(length(proba))
	k.traits[adopt]=!k.traits[adopt]
	k.traits
}
