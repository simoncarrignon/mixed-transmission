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

