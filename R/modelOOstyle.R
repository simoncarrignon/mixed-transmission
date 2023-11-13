
modelOOstyle <- function(N, F_Th=NULL, ki,km,K,m, b, r, rho=.5, d, maturity, endrepro,a,tp,population,comus,logging="time",tstep,ma=1){
    popsize=length(population)
    popsum=list()
    popsum[[1]]=sapply(names(population[[1]]),function(n)table(sapply(population,"[[",n)))
    for(time in 2:tstep){
        if("time"%in%logging)print(paste("------",time,"------"))
        for(i in sample(1:length(population))){
            ind=tryCatch(population[[i]]$id,error=function(e)NULL)
            if(!is.null(ind)){
                ##ageing

                population[[ind]]$age=population[[ind]]$age+1


                ##marriage
                if(population[[ind]]$age>maturity & population[[ind]]$partner <0){
                    #print("marriage attempt")
                    if(runif(1)<m){
                        potential=sapply(population,function(i,s)if(i$age>maturity & i$partner<0 & i$sex != s & i$age < endrepro)return(i$id),s=population[[ind]]$sex)
                        potential=unlist(potential)
                        if(length(potential)>0){
                            if(length(potential)==1)partner=potential
                            else partner=sample(potential,1)
                            population[[ind]]$partner=partner
                            population[[partner]]$partner=ind

                            if( population[[ind]]$sex == "F"){
                                fc=population[[ind]]$community
                                mc=population[[partner]]$community
                                stopifnot(population[[partner]]$sex=="M") #temp test
                            }
                            else{
                                mc=population[[ind]]$community
                                fc=population[[partner]]$community
                                stopifnot(population[[partner]]$sex=="F")

                            }
                            ## choice of new community  // ugly ; could do a function(rho,fc,mc) return(lc,jc) & ifelse
                            if(runif(1)<rho){
                                lc=fc #leaving the father's community
                                jc=mc #joining the mother's community
                            }
                            else{
                                jc=fc #joining the father's community
                                lc=mc #leaving the mother's community
                            }

                            comus$size[jc]=comus$size[jc]+1
                            comus$size[lc]=comus$size[lc]-1
                            population[[ind]]$community=population[[partner]]$community=jc

                            if("pairing"%in% logging)print(paste("marriage",ind,partner,"moving all to",jc," and leaving",lc, ",new:" ,population[[partner]]$community,population[[ind]]$community))
                        }
                    }
                }
                ##repro
                partner=population[[ind]]$partner
                canrepro=population[[ind]]$repro  #to check that partner did not already reproduce
                if(partner>0 & !canrepro ){
                    if((population[[ind]]$age < endrepro) & (population[[partner]]$age < endrepro)){
                        if("verbose"%in%logging)print(paste("childtest",ind,partner,population[[ind]]$community,population[[partner]]$community))
                        population[[partner]]$repro=TRUE #make sure proba of reproduction is unique for all pair 

                        ad_tr=comus$adaptivetraits[population[[ind]]$community,]

                        lbd=lambda(b,r,ad_tr)
                        lbd=lbd/ma #adjust rate for life time?

                        if(runif(1)<lbd){
                            newborn=reproduction(population[[ind]],population[[partner]],tp,population[[length(population)]]$id)
                            if("demo"%in%logging)print(paste("newborn in ",newborn$community))
                            population[[newborn$id]]=newborn
                            comus$size[newborn$community]=comus$size[newborn$community]+1
                        }
                    }
                }
                population[[ind]]$repro=FALSE

                #handle deaths
                if(runif(1)<d){
                    if(partner>0) population[[partner]]$partner=-1
                    comus$size[population[[ind]]$community]=comus$size[population[[ind]]$community]-1
                    if("demo"%in%logging ) print(paste("dead ind",ind,"in",population[[ind]]$community))
                    population[[ind]]=NULL
                }
            }


            ##pm transmission

        }

        ## Fission
        if(!is.null(F_Th)){
            overloaded=comus$size>F_Th
            if(sum(overloaded)>0){
                listcomu=sapply(population,"[[","community") ##keep list of agent in each commu to avoid that 
                for(split in which(overloaded)){
                    newcomunity=length(comus$size)+1
                    print(paste("community",split,"with size",comus$size[split]))
                    tosplit= (listcomu == split)
                    #comus
                    moving=sample(names(population)[tosplit],comus$size[split]/2)

                    #resize communities
                    comus$size[split]=comus$size[split]-length(moving)
                    comus$size=c(comus$size,length(moving))

                    #resize copy adaptive traits
                    comus$adaptivetraits=rbind(comus$adaptivetraits,comus$adaptivetraits[split,])

                    #position new community
                    comus$adaptivetraits=rbind(comus$adaptivetraits,comus$adaptivetraits[split,])
                    comus$coordinates=rbind(comus$coordinates,random2Dgrid(1,100))

                    #move each agent and partner
                    for( mov in moving){
                        population[[mov]]$community = newcomunity
                        part=population[[mov]]$partner 
                        if(part>-1){
                            comus$size[population[[part]]$community]  =  comus$size[population[[part]]$community] - 1
                            population[[part]]$community = newcomunity
                            comus$size[newcomunity]=comus$size[newcomunity]+1
                        }
                    }
                    print(paste("oldcoumnity",split,"is now",comus$size[split],"newcomunity",newcomunity,"of new size",comus$size[newcomunity]))
                    #population[moving]=lapply(population[moving],function(m,nc){m$community=newcomunity;m})
                    #could use that and then shoose randomly between parents
                }

            }

        }

        #quick summary of population at time 
        popsum[[time]]=sapply(names(population[[1]]),function(n)table(sapply(population,"[[",n)))

        ##
        if("visu"%in% logging)plot(comus$coordinates,pch=21,bg=apply(comus$adaptivetraits,1,mean)+1,cex=log(comus$size))
        popsize=c(popsize,length(population))

        #stopifnot(any(comus$size==table(sapply(population,"[[","community"))))
    }
    return(list(popsize=popsize,popsum=popsum))
}
