
# x ... vector of ages
# b ... start age of specific mortality, last value ranges to infinity
# m ... mortality per bracket

# x  <- round(runif(100,min=0,max=100))
# b  <- c(0,5,18,40,65,85)
# m  <- c(0.15,0.01,0.01,0.02,0.05,1)
# ageDeath(x,b,m)

ageDeath <- function(x,b=c(0,5,18,40,65,85),m=NULL)
{
    rnd <- runif(length(x))
    if(length(m)==1)
        return(rnd<m)
    else{
        b <- cbind(b,c(b[-1],Inf))
        return(rnd<sapply(x,function(x,b,m){m[which(b[,1]<=x & b[,2]>x)]},b=b,m=m))
    }
}
