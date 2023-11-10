
modelOOstyle <- function(N, F_Th=NULL, ki,km,K,m, b, r, rho=.5, d, maturity, endrepro,a,neutraltraitsParam,population,initcomus,logging="time",tstep,ma=1){
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

                            initcomus$size[jc]=initcomus$size[jc]+1
                            initcomus$size[lc]=initcomus$size[lc]-1
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

                        ad_tr=initcomus$adaptivetraits[population[[ind]]$community,]

                        lbd=lambda(b,r,ad_tr)
                        lbd=lbd/ma #adjust rate for life time?

                        if(runif(1)<lbd){
                            newborn=reproduction(population[[ind]],population[[partner]],neutraltraitsParam,population[[length(population)]]$id)
                            if("demo"%in%logging)print(paste("newborn in ",newborn$community))
                            population[[newborn$id]]=newborn
                            initcomus$size[newborn$community]=initcomus$size[newborn$community]+1
                        }
                    }
                }
                population[[ind]]$repro=FALSE

                #handle deaths
                if(runif(1)<d){
                    if(partner>0) population[[partner]]$partner=-1
                    initcomus$size[population[[ind]]$community]=initcomus$size[population[[ind]]$community]-1
                    if("demo"%in%logging ) print(paste("dead ind",ind,"in",population[[ind]]$community))
                    population[[ind]]=NULL
                }
            }


            ##pm transmission

        }

        ## Fission
        if(!is.null(F_Th)){
            overloaded=initcomus$size>F_Th
            if(sum(overloaded)>0){
                listcomu=sapply(population,"[[","community") ##keep list of agent in each commu to avoid that 
                for(split in which(overloaded)){
                    newcomunity=length(initcomus$size)+1
                    print(paste("community",split,"with size",initcomus$size[split]))
                    tosplit= (listcomu == split)
                    #initcomus
                    moving=sample(names(population)[tosplit],initcomus$size[split]/2)

                    #resize communities
                    initcomus$size[split]=initcomus$size[split]-length(moving)
                    initcomus$size=c(initcomus$size,length(moving))

                    #resize copy adaptive traits
                    initcomus$adaptivetraits=rbind(initcomus$adaptivetraits,initcomus$adaptivetraits[split,])

                    #position new community
                    initcomus$adaptivetraits=rbind(initcomus$adaptivetraits,initcomus$adaptivetraits[split,])
                    initcomus$coordinates=rbind(initcomus$coordinates,random2Dgrid(1,100))

                    #move each agent and partner
                    for( mov in moving){
                        population[[mov]]$community = newcomunity
                        part=population[[mov]]$partner 
                        if(part>-1){
                            initcomus$size[population[[part]]$community]  =  initcomus$size[population[[part]]$community] - 1
                            population[[part]]$community = newcomunity
                            initcomus$size[newcomunity]=initcomus$size[newcomunity]+1
                        }
                    }
                    print(paste("oldcoumnity",split,"is now",initcomus$size[split],"newcomunity",newcomunity,"of new size",initcomus$size[newcomunity]))
                    #population[moving]=lapply(population[moving],function(m,nc){m$community=newcomunity;m})
                    #could use that and then shoose randomly between parents
                }

            }

        }

        #quick summary of population at time 
        popsum[[time]]=sapply(names(population[[1]]),function(n)table(sapply(population,"[[",n)))

        ##
        if("visu"%in% logging)plot(initcomus$coordinates,pch=21,bg=apply(initcomus$adaptivetraits,1,mean)+1,cex=log(initcomus$size))
        popsize=c(popsize,length(population))

        #stopifnot(any(initcomus$size==table(sapply(population,"[[","community"))))
    }
    return(list(popsize=popsize,popsum=popsum))
}
