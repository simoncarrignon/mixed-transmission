
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
    cbind(x=sample(Gx,K),y=sample(Gy,K))
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

initialiseCommunities <- function(coordinates=NULL,initcoor="random",G=NULL,ks=NULL,traits=NULL,km=NULL,ki=NULL,K=NULL){
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
    if(is.null(traits)) traits=initraits(km,ki)

    size=vector(mode="numeric",length=K)

    list(coordinates=coordinates,adaptivetraits=traits,size=size)
}

