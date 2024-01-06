
#' Generate Random 2D Grid Coordinates
#'
#' This function generates a set of random 2D coordinates, useful for initializing locations on a grid.
#'
#' @param K integer, the number of communities to locate 
#' @param Gx integer, the size of the grid in the x-dimension.
#' @param Gy integer, the size of the grid in the y-dimension. If NULL, it assumes a square grid and uses Gx for the y-dimension.
#'
#' @return A 2-column matrix where each row represents the x and y coordinates of a community on the grid.
#' @export
#'
#' @examples
#' com=random2Dgrid(4, 100,100)
#' plot(com,xlim=c(0,100),ylim=c(0,100),pch=20)
random2Dgrid <- function(K,Gx,Gy=NULL){
    if(is.null(Gy))Gy=Gx
    stopifnot(K<=(Gx*Gy))
    allpossible=as.matrix(expand.grid(X=1:Gx,Y=1:Gy))
    ind=sample.int(nrow(allpossible),K)
    allpossible[ind,,drop=F]
}

#' Initialise Communities with Traits and Coordinates
#'
#' This function sets up communities by initializing their spatial coordinates, adaptive traits, and sizes.
#'
#' @param coordinates matrix, a pre-defined set of coordinates for the communities; if NULL, coordinates will be generated based on other criteria.
#' @param initcoor character, if "random", coordinates are generated randomly; otherwise, a specified setup is expected.
#' @param G integer, used when coordinates are to be generated randomly, indicating the grid size.
#' @param ks numeric, the proportion of incumbents; used when `K` is defined but `ki` and `km` are not.
#' @param traits matrix, a pre-defined set of traits for the community; if NULL, traits will be generated internally.
#' @param km integer, the number of migrants communities.
#' @param ki integer, the number of incumbents communities.
#' @param K integer, the total number of communities ; if NULL, it's derived from `ki` and `km.
#'
#' @return A list containing matrices or vectors for the coordinates, adaptive traits, and sizes of each the communities. 
#' @export
#'
#' @examples
#' comus=initialiseCommunities(G = 100, ki = 5, km = 5)
#' plot(comus$coordinates,pch=21,bg=apply(comus$adaptivetraits,1,mean)+2,xlim=c(0,100),ylim=c(0,100))

initialiseCommunities <- function(coordinates=NULL,initcoor="random",G=NULL,ks=NULL,traits=NULL,km=NULL,ki=NULL,K=NULL,migrantscount=NULL,sizes=NULL,plot=F){
    if(!is.null(traits)) K=nrow(traits)
    if(is.null(K)){
        stopifnot(!is.null(ki),!is.null(km))
        K=ki+km
    }
    else{
        if(!is.null(ks)){
            ki=K*ks
            km=K*(1-ks)
        }
    }
    if(is.null(coordinates)){
        if(initcoor=="random"){
            if(is.null(G))stop("G need a value to initialise coordinate at random")
            else{
                #could imagine different beavrio if G is raster/matrix/...
                coordinates=random2Dgrid(K,G)
            }
        }
    }
    if(is.null(traits)) traits=initAdaptiveTraits(km,ki)

	if(is.null(migrantscount))	migrantscount=matrix(0,nrow=K,ncol=K)
	if(is.null(G))stop("need to give grid limits")
    if(length(sizes)==K)size=sizes
    else if(length(sizes)==1)size=rep(sizes,K)
    else if(is.null(sizes))size=vector(mode="numeric",length=K)
    else{
        stop("what are community sizes")
    }
    
    occupation=matrix(0,nrow=G,ncol=G)
    occupation[coordinates]=1


    comus=list(coordinates=coordinates,adaptivetraits=traits,size=size,migrantscount=migrantscount,occupation=occupation)
    if(plot)plot.comu(comus)
    return(comus)
}



#' Split Communities by Families
#'
#' This function selects families from a specified community within a population dataset
#' until the total population size of the selected families reaches or exceeds a specified limit.
#' It then assigns a new community ID to these families.
#'
#' @param comid The ID of the community to be partitioned.
#' @param population A dataframe containing population data,
#'                   which includes columns for community IDs and family IDs.
#' @param newsize The target population size for the selected families.
#'                The function selects families until this size is reached or exceeded.
#' @param newid The new community ID to be assigned to the selected families.
#'
#' @return The population dataframe with updated community IDs for the selected families.
#' @examples
#' # Assuming `population` is a dataframe with 'community' and 'fid' columns
#' splitCommunitiesByFamilies(1, population, 800, 2)
#' @export
#'
splitCommunitiesByFamilies <- function(comid, population, newsize, newid) {
    # Initialize an empty vector to store selected family IDs
    fmig = c()

    # Randomly sample unique family IDs from the specified community
    fids = sample(unique(population[population[, "community"] == comid, "fid"]))

    # Start with selecting one family
    lim = 1
    fmig = fids[1:lim]

    # Continue adding families until the sum of individuals in selected families
    # is at least the specified new size
    while (sum(population[, "fid"] %in% fmig) < newsize) {
        lim = lim + 1
        fmig = fids[1:lim]
    }

    # Assign the new community ID to the selected families
    population[population[, "fid"] %in% fmig, "community"] = newid

    # Return the modified population data frame
    return(population)
}


#' Reassign Families to New Community
#'
#' This function reassigns families from a specified community to a new community ID
#' once the cumulative size of these families reaches a specified limit. This version work \emph{if and only if} 
#' \code{fid} identify children \emph{and} their parents
#'
#' @param comid The ID of the community to be processed.
#' @param population A dataframe containing population data, 
#'                   including community IDs and family IDs.
#' @param newsize The cumulative size limit for the family selection.
#' @param newid The new community ID to assign.
#'
#' @return A modified population dataframe with updated community IDs for the selected families.
#' @examples
#' # Assuming `population` is a dataframe with 'community' and 'fid' columns
#' # reassignFamiliesToNewCommunityFIDs(1, population, 800, 2)
#'
reassignFamiliesToNewCommunityFIDs <- function(comid, population, newsize, newid) {
    # Validate input parameters
    if (!is.numeric(newsize) || newsize <= 0) {
        stop("newsize must be a positive number")
    }
	commuConsistency(population)


    # Select community family IDs
    fids <- population[population[, "community"] == comid, "fid"]

    # Determine families to be reassigned
    cumulative_size <- cumsum(table(fids)[as.character(sample(unique(fids)))])
    selected_families <- names(cumulative_size[cumulative_size <= newsize])

    # Reassign the new community ID
    population[population[, "fid"] %in% selected_families, "community"] <- newid
	commuConsistency(population)

    return(population)
}


#' Check Community Consistency in Population
#'
#' quick test that each community identifier (cid) in a given population
#' dataset adheres to certain consistency rules:
#' 1. Each cid, appears either once (dead partner) or twice in the population.
#' 2. For each cid, all members of the population belonging to that cid
#'    are part of the same community.
#'
#' @export
commuConsistency <- function(population){
	uniqcids <- unique(population[,"cid"])
	cid.counts <- table(population[,"cid"])
	stopifnot(cid.counts[-1] %in% c(1,2))
	stopifnot(sapply(uniqcids[uniqcids!=-1],function(cid)length(unique(population[population[,"cid"]==cid,"community"])))==1)
}


#' Reassign Families to New Community
#'
#' This function reassigns families from a specified community to a new community ID
#' once the cumulative size of these families reaches a specified limit. This version 
#' recreate nuclear families using `cid` and `fid`, than split the families
#'
#' @param comid The ID of the community to be processed.
#' @param population A dataframe containing population data, 
#'                   including community IDs and family IDs.
#' @param newsize The cumulative size limit for the family selection.
#' @param newid The new community ID to assign.
#' @param debug verbose output
#'
#' @return A modified population dataframe with updated community IDs for the selected families.
#' @examples
#' # Assuming `population` is a dataframe with 'community' and 'fid' columns
#' # reassignFamiliesToNewCommunityNoFIDs(1, population, 800, 2)
#'
#' @export
reassignFamiliesToNewCommunityNoFIDs <- function(comid, population, newsize, newid,debug=FALSE) {
    # Validate input parameters
    if (!is.numeric(newsize) || newsize <= 0) {
        stop("newsize must be a positive number")
    }
	if(debug)commuConsistency(population)


    # Select community family IDs
    single.comu <- population[population[, "community"] == comid,,drop=F ]
    if(nrow(single.comu)==1){
        warning("splitting a one individual community")
        population[population[, "id"] == single.comu[,"id"],"community"] <- newid
        return(population)
    }
	single.comu <- cbind(single.comu,nfid=single.comu[,"fid"])

    kids <- single.comu[single.comu[, "cid"] == -1, "fid" ] 
    parents <- single.comu[single.comu[, "cid"] %in% kids , "cid" ]
	single.comu[single.comu[, "cid"] == -1, "nfid" ] = single.comu[single.comu[, "cid"] == -1, "fid" ] 
	single.comu[single.comu[, "cid"] %in% kids , "nfid" ] <- single.comu[single.comu[, "cid"] %in% kids , "cid" ]

	couple=single.comu[single.comu[,"cid"]>-1,"cid"]

	couplenokids=couple[!(couple%in% parents)] 
	single.comu[single.comu[, "cid"] %in% couplenokids , "nfid" ] <- single.comu[single.comu[, "cid"] %in% couplenokids , "cid" ]
    fids <- single.comu[,"nfid"]

    # Determine families to be reassigned
    candidates=unique(fids)
    if(length(candidates)>1){
        cumulative_size <- cumsum(table(fids)[as.character(sample(candidates))])
        selected_families <- as.numeric(names(cumulative_size[cumulative_size <= newsize]))
    }
    else{
        cumulative_size <- length(fids)
        selected_families=candidates
    }

	stopifnot(cumulative_size[length(cumulative_size)]==sum(population[,"community"]==comid))

    # Reassign the new community ID
	selected.indiv <- single.comu[single.comu[,"nfid"] %in% selected_families ,"id"]
    if(length(selected.indiv)>newsize) return(population)

	population[population[, "id"] %in% selected.indiv,"community"] <- newid

	if(debug)commuConsistency(population)
    return(population)
}


fissionCommunity <- function(comus,ol){
    potential=which(comus$occupation==0,arr.ind=T)
    if(nrow(potential)>0){
        distprob=apply(potential,1,function(x1,x2)sqrt(sum((x1 - x2)^2)),x2=comus$coordinates[ol,])
        newcoord=potential[sample(1:nrow(potential),1,prob=distprob),]
        comus$coordinates=rbind(comus$coordinates,unlist(newcoord))
        comus$occupation[newcoord[1],newcoord[2]]=1
        comus$adaptivetraits=rbind(comus$adaptivetraits,comus$adaptivetraits[ol,])
        comus$size=c(comus$size,0)
        comus$migrantscount=cbind(comus$migrantscount,rep(0,nrow(comus$migrantscount)))
        comus$migrantscount=rbind(comus$migrantscount,rep(0,ncol(comus$migrantscount)))
    }
    return(comus)
}

plot.comu <- function(comus,color_gradient=NULL,vidfile=NULL)
{
    if(is.null(color_gradient))
    {
        start_color <- "#006400" # Deep Green
        end_color <- "#FFD700" # Golden Yellow
        color_gradient <- colorRampPalette(c(start_color, end_color))(ncol(comus$adaptivetraits)+1)
    }

    if(!is.null(vidfile)) png(paste0(vidfile,".png"),height = 800,width = 800,pointsize = 18)
    plot(comus$coordinates,pch=21,bg=color_gradient[apply(comus$adaptivetraits,1,sum)+1],cex=log(comus$size),ylim=c(1,nrow(comus$occupation)),xlim=c(1,ncol(comus$occupation)))
    if(!is.null(vidfile)) dev.off()
}
