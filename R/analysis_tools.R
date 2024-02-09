

#using a the result of a simulation
#return the type of comu (0,1,2,3) 
getCommuType <- function(exp){
    comusize=exp$comusize
    ncomu=length(comusize[[length(comusize)]])
    sapply(1:ncomu,function(com){
               st=min(which(!is.na(sapply(comusize,function(size)size[com]))))
               sum(exp$traitpercomu[[st]][com,paste0("a",1:3)]/comusize[[st]][com])
    })
}
