#' Calculate Adjusted Birth Rate
#'
#' This function calculates the adjusted birth rate for a community by taking into account the
#' base birth rate, a bonus rate, and the sum of advantageous traits within the community.
#'
#' @param base_rate Numeric, the base birth rate of the community.
#' @param bonus_rate Numeric, the rate at which each full farmer increases the birth rate.
#' @param advantageous_traits Numeric vector, each element representing the presence (1) or absence (0)
#' of an advantageous trait within the community.
#'
#' @return Numeric, the adjusted birth rate after accounting for advantageous traits.
#' @examples
#' lambda(2, 0.5, c(1, 1, 0))
#' lambda(1, 0.3, c(1, 1, 1))
#' @export
lambda <- function(base_rate,bonus_rate,advantageous_traits){
                base_rate+bonus_rate*sum(advantageous_traits) #birth rate wrt to community adaptive traits
}


##test
lambdas = sapply(seq(0,0.2,.01),lambda,base_rate=0.01,advantageous_traits=c(1, 1, 1))
plot(seq(0,0.2,.01),lambdas,type="n",xlab="base rate",ylab="bonus rate")
advs=list(c(0,0,0),c(0,0,1),c(0,1,1),c(1,1,1))

for(av in advs){
    lambdas = sapply(seq(0,0.2,.01),lambda,base_rate=0.01,advantageous_traits=av)
    points(seq(0,0.2,.01),lambdas)
}

