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


#' @title Social Learning
#' @description Core social learning routine
#' @param x matrix containing population of agents
#' @param when character defining whether the transmission is pre-marial ('pre') or post-marital ('post')
#' @param pathways list containing the transmission pathways for each trait (generated using \code{initNeutralTraitsPathways()})
#' @param threshold integer defining age tresholds for distinguishing horrizontal and oblique transmission.
#'
#' @return An updated matrix of the population of agents
#' @export

social.learning <- function(x=population,when='pre',pathways=neutraltraitsParam,threshold)
{
	ntraits <- length(pathways$s)

	# Pre marriage learning (horrizontal or oblique)
	if (when=='pre')
	{
		#learners (age 0)
		index.learners  <- which(x[,'age']==0)

		#index communities
		id.communities <- x[index.learners,'community']

		# sampling pool
		age.pool.0.h <- x[which(x[,'age']<=threshold & x[,'sex']==0),]
		age.pool.1.h <- x[which(x[,'age']<=threshold & x[,'sex']==1),]
		age.pool.01.h <- x[which(x[,'age']<=threshold),]

		age.pool.0.o <- x[which(x[,'age']>threshold & x[,'sex']==0),]
		age.pool.1.o <- x[which(x[,'age']>threshold & x[,'sex']==1),]
		age.pool.01.o <- x[which(x[,'age']>threshold),]

		#sampling probabilities of novel variant
		sample.pool.0.h <- aggregate(age.pool.0.h[,paste0('t',1:ntraits)],by=list(as.factor(age.pool.0.h[,'community'])),FUN=function(x){sum(x)/length(x)})
		sample.pool.1.h <- aggregate(age.pool.1.h[,paste0('t',1:ntraits)],by=list(as.factor(age.pool.1.h[,'community'])),FUN=function(x){sum(x)/length(x)})
		sample.pool.01.h <- aggregate(age.pool.01.h[,paste0('t',1:ntraits)],by=list(as.factor(age.pool.01.h[,'community'])),FUN=function(x){sum(x)/length(x)})

		sample.pool.0.o <- aggregate(age.pool.0.o[,paste0('t',1:ntraits)],by=list(as.factor(age.pool.0.o[,'community'])),FUN=function(x){sum(x)/length(x)})
		sample.pool.1.o <- aggregate(age.pool.1.o[,paste0('t',1:ntraits)],by=list(as.factor(age.pool.1.o[,'community'])),FUN=function(x){sum(x)/length(x)})
		sample.pool.01.o <- aggregate(age.pool.01.o[,paste0('t',1:ntraits)],by=list(as.factor(age.pool.01.o[,'community'])),FUN=function(x){sum(x)/length(x)})

		for (i in 1:ntraits)
		{
			if (pathways$pre[i,'h']==1)
			{
				if (pathways$s[i]==0)
				{
					x[index.learners,paste0('t',i)]  <-  rbinom(length(index.learners),size=1,prob=sample.pool.0.h[match(id.communities,as.integer(sample.pool.0.h[,1])),i+1])
				}

				if (pathways$s[i]==1)
				{
					x[index.learners,paste0('t',i)]  <-  rbinom(length(index.learners),size=1,prob=sample.pool.1.h[match(id.communities,as.integer(sample.pool.1.h[,1])),i+1])
				}

				if (pathways$s[i]==-1)
				{
					x[index.learners,paste0('t',i)]  <-  rbinom(length(index.learners),size=1,prob=sample.pool.01.h[match(id.communities,as.integer(sample.pool.01.h[,1])),i+1])
				}
			}


			if (pathways$pre[i,'o']==1)
			{
				if (pathways$s[i]==0)
				{
					x[index.learners,paste0('t',i)]  <-  rbinom(length(index.learners),size=1,prob=sample.pool.0.o[match(id.communities,as.integer(sample.pool.0.o[,1])),i+1])
				}

				if (pathways$s[i]==1)
				{
					x[index.learners,paste0('t',i)]  <-  rbinom(length(index.learners),size=1,prob=sample.pool.1.o[match(id.communities,as.integer(sample.pool.1.o[,1])),i+1])
				}

				if (pathways$s[i]==-1)
				{
					x[index.learners,paste0('t',i)]  <-  rbinom(length(index.learners),size=1,prob=sample.pool.01.o[match(id.communities,as.integer(sample.pool.01.o[,1])),i+1])
				}
			}
		}
	}


	if (when=='post')
	{

		#learners (just married)
		index.learners  <- which(x[,'justMarried']==1 & x[,'cid']!=-1)

		#index communities
		id.communities <- x[index.learners,'community']

		# sampling pool: provide a list of index values of eligible teacher for each index.learner.*
		# Horrizontal
		age.pool.0.h <- sapply(1:length(index.learners),function(x,community,learner,pop,threshold){same.community.i = which(pop[,'community']==community[x] & pop[,'sex']==0);return(same.community.i[which(abs(pop[same.community.i,'age']-pop[learner[x],'age']) < threshold)])},pop=x,learner=index.learners,community=id.communities,threshold=threshold)

		age.pool.1.h <- sapply(1:length(index.learners),function(x,community,learner,pop,threshold){same.community.i = which(pop[,'community']==community[x] & pop[,'sex']==1);return(same.community.i[which(abs(pop[same.community.i,'age']-pop[learner[x],'age']) < threshold)])},pop=x,learner=index.learners,community=id.communities,threshold=threshold)

		age.pool.01.h <- sapply(1:length(index.learners),function(x,community,learner,pop,threshold){same.community.i = which(pop[,'community']==community[x]);return(same.community.i[which(abs(pop[same.community.i,'age']-pop[learner[x],'age']) < threshold)])},pop=x,learner=index.learners,community=id.communities,threshold=threshold)

		# Oblique
		age.pool.0.o <- sapply(1:length(index.learners),function(x,community,learner,pop,threshold){same.community.i = which(pop[,'community']==community[x] & pop[,'sex']==0);return(same.community.i[which(pop[same.community.i,'age']-pop[learner[x],'age'] > threshold)])},pop=x,learner=index.learners,community=id.communities.sex0,threshold=threshold)

		age.pool.1.o <- sapply(1:length(index.learners),function(x,community,learner,pop,threshold){same.community.i = which(pop[,'community']==community[x] & pop[,'sex']==1);return(same.community.i[which(pop[same.community.i,'age']-pop[learner[x],'age'] < threshold)])},pop=x,learner=index.learners,community=id.communities,threshold=threshold)

		age.pool.01.o <- sapply(1:length(index.learners),function(x,community,learner,pop,threshold){same.community.i = which(pop[,'community']==community[x]);return(same.community.i[which(pop[same.community.i,'age']-pop[learner[x],'age'] < threshold)])},pop=x,learner=index.learners,community=id.communities,threshold=threshold)

		#sampling probabilities of novel variant, matrix with row number corresponding to each learner and column representing the trait
		sample.pool.0.h <- sapply(1:length(index.learners),function(x,pool,pop){return(apply(pop[pool[[x]],paste0('t',1:ntraits)],2,sum)/length(pool[[x]]))},pool=age.pool.0.h,pop=x)

		sample.pool.1.h <- sapply(1:length(index.learners),function(x,pool,pop){return(apply(pop[pool[[x]],paste0('t',1:ntraits)],2,sum)/length(pool[[x]]))},pool=age.pool.1.h,pop=x)
		
		sample.pool.01.h <- sapply(1:length(index.learners),function(x,pool,pop){return(apply(pop[pool[[x]],paste0('t',1:ntraits)],2,sum)/length(pool[[x]]))},pool=age.pool.01.h,pop=x)

		sample.pool.0.o <- sapply(1:length(index.learners),function(x,pool,pop){return(apply(pop[pool[[x]],paste0('t',1:ntraits)],2,sum)/length(pool[[x]]))},pool=age.pool.0.o,pop=x)

		sample.pool.1.o <- sapply(1:length(index.learners),function(x,pool,pop){return(apply(pop[pool[[x]],paste0('t',1:ntraits)],2,sum)/length(pool[[x]]))},pool=age.pool.1.o,pop=x)
		
		sample.pool.01.o <- sapply(1:length(index.learners),function(x,pool,pop){return(apply(pop[pool[[x]],paste0('t',1:ntraits)],2,sum)/length(pool[[x]]))},pool=age.pool.01.o,pop=x)



		for (i in 1:ntraits)
		{
			# Horizzontal
			if (pathways$post[i,'h']==1)
			{
				if (pathways$s[i]==0)
				{
					x[index.learners,paste0('t',i)]  <- rbinom(length(index.learners),size=1,prob=sample.pool.0.h[i,]) 
				}

				if (pathways$s[i]==1)
				{
					x[index.learners,paste0('t',i)]  <- rbinom(length(index.learners),size=1,prob=sample.pool.1.h[i,]) 
				}

				if (pathways$s[i]==-1)
				{
					x[index.learners,paste0('t',i)]  <- rbinom(length(index.learners),size=1,prob=sample.pool.01.h[i,]) 
				}
			}

			# Oblique
			if (pathways$post[i,'o']==1)
			{
				if (pathways$s[i]==0)
				{
					x[index.learners,paste0('t',i)]  <- rbinom(length(index.learners),size=1,prob=sample.pool.0.o[i,]) 
				}

				if (pathways$s[i]==1)
				{
					x[index.learners,paste0('t',i)]  <- rbinom(length(index.learners),size=1,prob=sample.pool.1.o[i,]) 
				}

				if (pathways$s[i]==-1)
				{
					x[index.learners,paste0('t',i)]  <- rbinom(length(index.learners),size=1,prob=sample.pool.01.o[i,]) 
				}
			}
		}
	}
	return(x) #Returns the actual population matrix
}
