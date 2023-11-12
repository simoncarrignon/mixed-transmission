
modelVector <- function(N, F_Th=NULL, ki,km,K,m, b, r, rho=.5, d, maturity, endrepro,a,tp,population,initcomus,logging="time",tstep,ma=1,traitsid){
    popsize=nrow(population)
    popsum=list()
    popsum[[1]]=apply(population,2,table)
    for(time in 2:tstep){
        if("time"%in%logging)print(paste("------",time,"------"))
        population[,"age"]=population[,"age"]+1
        couple=which(population[,"partner"]>-1)


        ##marriage
        potential=population[,"age"]>=maturity & population[,"partner"]<0
        single_male  =which(potential & population[,"sex"]==0)
        stopifnot(population[single_male,"sex"]==0) #temp test
        single_female=which(potential & population[,"sex"]==1)
        stopifnot(population[single_female,"sex"]==1) #temp test
        weds=sum(runif(min(length(single_male),length(single_female)))<m)
        # if m is 1, every people who could get married will do 
        # if m is .5, half o the people who could get married will do , but depdns on the balance male/female.
        if(weds>0){
            if(weds>1){
                single_male  =sample(single_male)
                single_female=sample(single_female)
            }
            maxcid=max(population[,"cid"])
            for(i in 1:weds){
                maxcid=maxcid+1
                c1=single_male[i]
                c2=single_female[i]
                population[c(c1,c2),"cid"]=maxcid #Couple ID, to track couples
                population[c(c1,c2),"fid"]=maxcid #Family Id, to track kids
        ## quick note: in this version offspring stay with the partner that stay alive until the parents find a new partner; then offspring will be on their own.
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
            stopifnot(table(population[population[,"cid"]>-1,"cid"])==2)
        }

        ##repro
        families=population[,"cid"]
        families=families[families>=0]
        fcount=table(families)
        stopifnot(table(families)[fcount>1] == 2)
        stopifnot(population[population[,"cid"] %in% names(fcount)[fcount==1] ,"partner"]==-1)
        repro=population[,"age"]>=maturity & population[,"age"] < endrepro & population[,"partner"] > 0
        if(sum(repro)>0){
            #ad_trinitcomus$adaptivetraits[population[repro,"community"],]
            reprofam=table(population[repro,"community"])
            fam=population[repro,c("community","cid"),drop=F]
            fcount=table(fam[,"cid"])
            fam=fam[fcount>1,,drop=F]
            stopifnot(nrow(fam[,"cid"])%%2 == 0) #if not even then we have someone that can autoroproduce
            fam=unique(fam)
            stopifnot(table(families) == 2) #families should be made of 2 individual
            ad_tr=initcomus$adaptivetraits[fam[,"community"],,drop=F]
            lbd=apply(ad_tr,1,lambda,base_rate=b,bonus_rate=r)
            lbd=lbd/ma
            newborns=runif(nrow(fam))<lbd
            nchilds=sum(newborns)
            if(nchilds>0){
                givbirth=fam[newborns,2]
                ##get all parents who gave birth
                newparents=sapply(givbirth,function(cid)which(population[,"cid"]==cid))
                ##group parents together
                newparents=apply(newparents,2,function(i)population[i,],simplify=F)

                ##get families ids and communities
                famids=t(sapply(newparents,function(i)unique(i[,c("community","fid")])))
                offcom=famids[,1] ##offsprings community id
                offfid=famids[,2]  ##offfspsrings family id
                offtraits=initNeutralTraits(nchilds,z=z,nastart = T )
                ##Vertical Transmission
                if(sum(tp$pre[,"v"])>0)
                    offtraits[,tp$pre[,"v"]==1]=t(sapply(newparents,vertical,tp=tp,tid=traitsid))


                offsprings=cbind(newpop(nchilds,minid=max(population[,"id"]),community=offcom,fid=offfid),offtraits)

                ##### When do horizontal and oblique take place? who are the model? how to make it modular?
                #   ##Pre-Marital Horizontal Transmission
                #   if(sum(tp$pre[,"h"])>0)
                #       offtraits[,tp$pre[,"h"]==1]=population[,traitsid[tp$pre[,"h"]==1]]
                #   ##Pre-Marital Oblique Transmission
                #   if(sum(tp$pre[,"o"])>0)
                #       offtraits[,tp$pre[,"o"]==1]=t(sapply(newparents,vertical,tp=tp,tid=traitsid))


                population=rbind(population,offsprings[,colnames(population)])
            }
        }


        #handle deaths
        dead=runif(nrow(population))<d

        if(sum(dead)>0){
            singled=population[dead,"partner"]
            if("demo"%in%logging )
                paste("Death of:",paste0("ind ",apply(population[1:4,c("id","partner","cid")],1,paste0,collapse=" "),collapse=";"))
            if(sum(singled>0)>0){
                population[population[,"id"] %in%  singled[singled>0],c("partner","cid")]=-1
            }
            population=population[!dead,]
        }


        ##pm transmission

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
        popsum[[time]]=apply(population,2,table)

        ##
        if("visu"%in% logging)plot(initcomus$coordinates,pch=21,bg=apply(initcomus$adaptivetraits,1,mean)+1,cex=log(initcomus$size))
        popsize=c(popsize,nrow(population))

        #stopifnot(any(initcomus$size==table(sapply(population,"[[","community"))))
    }
    return(list(popsize=popsize,popsum=popsum))
}
