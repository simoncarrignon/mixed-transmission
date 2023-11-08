population=matrix(0)
N=100
age=vector("numeric",N)
age=rep(0,N) 
partner=rep(-1,N) 
canrepro=rep(0,N) 
community=rep(1,N) 
cid=rep(-1,N) 
fid=rep(0,N) 
sex=sample(c(0,1),N,replace=T) 
id=1:N

population=cbind(id,age,partner,community,canrepro,cid,sex)

modelVector <- function(N, F_Th=NULL, ki,km,K,m, b, r, rho=.5, d, maturity, endrepro,a,neutraltraitsParam,population,initcomus,logging="time",tstep,ma=1){
    popsize=nrow(population)
    for(time in 2:tstep){
        if("time"%in%logging)print(paste("------",time,"------"))
        population[,"age"]=population[,"age"]+1
        couple=which(population[,"partner"]>-1)

                ##repro
        families=unique(population[,"cid"])
        families=families[families>=0]
        families=sapply(families,function(cid,population)if(all(population[population[,"cid"]==cid,"age"]<endrepro))cid,population=population)

        ###FINISH REPRO
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
        ###FINISH REPRO

                ##marriage
                potential=population[,"age"]>=maturity & population[,"partner"]<0
                single_male  =which(potential & population[,"sex"]==0)
                single_female=which(potential & population[,"sex"]==1)
                weds=sum(runif(min(length(single_male),length(single_female)))<m)
                # if m is 1, every people who could get married will do 
                # if m is .5, half o the people who could get married will do , but depdns on the balance male/female.
                if(weds>0){
                    single_male  =sample(single_male)
                    single_female=sample(single_female)
                    maxcid=max(population[,"cid"])
                    for(i in 1:weds){
                        maxcid=maxcid+1
                        c1=single_male[i]
                        c2=single_female[i]
                        population[c(c1,c2),"cid"]=maxcid
                        population[c1,"partner"]=population[c2,"id"]
                        population[c2,"partner"]=population[c1,"id"]

                        if( population[c1,"sex"] == 1){
                            fc=population[c1,"community"]
                            mc=population[c2,"community"]
                            stopifnot(population[c2,"sex"]==0) #temp test
                        }
                        else{
                            fc=population[c2,"community"]
                            mc=population[c1,"community"]
                            stopifnot(population[c1,"sex"]==0) #temp test

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
                        population[c2,"community"]= population[c1,"community"]=jc

                        if("pairing"%in% logging)print(paste("marriage",c1,c2,"moving all to",jc," and leaving",lc, ",new:" ,population[c2,"community"],population[c1,"community"]))
                        }
                    }
                }


                }

                #handle deaths
                dead=runif(nrow(population))<d

                if(sum(dead)>0){
                    singled=population[dead,"partner"]
                    if(sum(singled>0)>0)
                        population[population[,"id"] %in%  singled[singled>0],"partner"]=-1
                    population=population[!dead,]
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
