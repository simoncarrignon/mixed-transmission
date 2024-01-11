shuffle <- function(x)x[sample.int(length(x))]

print.pathways <- function(pathways,verb=T){
    for(i in 1:nrow(pathways$pre)){
        it=paste0("c",i)
        for(tm in c("pre","post")){
            pt=paste0(colnames(pathways[[tm]])[pathways[[tm]][i,]==1],collapse=",")
            if(verb)it=paste0(it," ",tm,": ",pt)
            else it=paste0(it," ",pt)
        }
        if(verb)it=paste0(it," sex:",pathways$s[i])
        else it=paste0(it," ",pathways$s[i])
        print(it)
    }

}

print.pathways.latex<- function(pathways){
    for(i in 1:nrow(pathways$pre)){
        it=paste0("$c_{",i,"}$")
        for(tm in c("pre","post")){
            pt=paste0(colnames(pathways[[tm]])[pathways[[tm]][i,]==1],collapse="&")
            it=paste0(it,"&",pt)
        }
        it=paste0(it,"&",pathways$s[i],"\\\\\n")
        cat(it)
    }

}

#for(tm in c("pre","post"))
#  paste(tm,colnames(neutraltraitsParam[[tm]])[neutraltraitsParam[[tm]][i,]==1],sep=":")
#}

resetIds <- function(population){
    population[population[,"cid"]!= -1 ,"cid"] = as.numeric(as.factor(as.character(population[population[,"cid"]!= -1 ,"cid"])))
    population[,"fid"] = as.numeric(as.factor(as.character(population[ ,"fid"])))
    ids = as.numeric(as.factor(as.character(population[ ,"id"])))
    names(ids)=population[,"id"]
    population[,"id"] = as.numeric(as.factor(as.character(population[ ,"id"])))
    population[,"partner"]=as.numeric(ids[as.character(population[,"partner"])])
    population[is.na(population[,"partner"]),"partner"]=-1
    population
}

getFixationOneTraits <- function(traits,v=1){
    fixed=traits==v
    if(sum(fixed)==0)return(NA)
    else(min(which(fixed)))
}

matmean <- function(m)Reduce("+",m)/length(m)



