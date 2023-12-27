
modelVector <- function(N, F_Th=NULL, ki,km,K,m, b, r, rho=.5, d, maturity, endrepro,a,tp,age.threshold=20,population,comus,logging="time",tstep,ma=1,traitsid,getfinalpop=FALSE,out=c("popsize","popsumary"),beta=0.001){
	if("popsize"%in%out) popsize=nrow(population)
	if("weddings"%in%out) weddings=0
	if("popsumary"%in%out){
		popsum=list()
		popsum[[1]]=rep(0,length(comus$popsize))
	}
	if("migrsum"%in%out){
		migrsum=list()
		migrsum[[1]]=rep(0,K)
	}
	if("visu"%in% logging){
		start_color <- "#006400" # Deep Green
		end_color <- "#FFD700" # Golden Yellow
		color_gradient <- colorRampPalette(c(start_color, end_color))(ncol(comus$adaptivetraits))
	}

    if("comufull"%in%out){
        comufull=list()
        comufull[[1]]=comus
    }
	if(is.null(comus$migrants) ){
		#We nee a K x K  to store after migration from where are comming 
		comus$migrantscount=matrix(0,nrow=nrow(comus$adaptivetraits),nrow(comus$adaptivetraits))
	}
	for(time in 2:tstep){
		if("time"%in%logging)print(paste("------",time,"------"))
		population[,"age"]=population[,"age"]+1
		couple=which(population[,"partner"]>-1)

		coms=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
		stopifnot(coms == comus$size[as.numeric(names(coms))])


		##marriage
		population[,"justMarried"]  <- 0
		weds=matchingSingle(population,maturity=maturity)
		population[weds[,1],"justMarried"]  <- 1
		population[weds[,2],"justMarried"]  <- 1

		#current year migrant count
		migrantscount=matrix(0,nrow=nrow(comus$adaptivetraits),nrow(comus$adaptivetraits))
		# if m is 1, every people who could get married will do 
		# if m is .5, half o the people who could get married will do , but depdns on the balance male/female.
		if("weddings"%in%out) weddings=c(weddings,ifelse(is.null(weds),0,nrow(weds)))
		if(!is.null(weds)){
			maxcid=max(population[,"cid"])
            if(is.null(dim(weds)))weds=matrix(weds,nrow=1,ncol=2)
			for(i in 1:nrow(weds)){
				maxcid=maxcid+1
				c1=weds[i,1]
				c2=weds[i,2]
				population[c(c1,c2),"cid"]=maxcid #Couple ID, to track couples
				## population[c(c1,c2),"nfid"]=maxcid #To trakc nuclear family need to add here Family Id
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

				comus$size[jc]=comus$size[jc]+1
				comus$size[lc]=comus$size[lc]-1

				migrantscount[jc,lc]=migrantscount[jc,lc]+1
				#in-law transmission:
				migrant = c(c1,c2)[population[c(c1,c2),"community"]!=jc]
				local = c(c1,c2)[population[c(c1,c2),"community"]==jc]
				#teacher.offspring = c(c1,c2)[which(population[c(c1,c2),"community"]==jc)]
                if(length(local)==2){
                    warning("same community couple in the population")
                    local=sample(local,1)
                }
                inlawtraits=tp$post[,"i"]==1
                if(sum(inlawtraits)>0){
                    if(population[local,"fid"]==-1){warning("famillies not created yet, inlaw copying impossible")}
                    else{
                        it=traitsid[inlawtraits]
                        inlaws=population[ population[,"cid"] == population[local,"fid"],,drop=F]
                        if(dim(inlaws)[1]==2)
                        {
                            newtraits=drawFromPool(pool.traits=inlaws[,it,drop=F],pool.sex=inlaws[,"sex",drop=F],sexbiases=tp$s[inlawtraits])
                            population[migrant,it]=newtraits
                        }
                    }
                }

				# post-marital movement:
				population[c2,"community"]= population[c1,"community"]=jc

				if("pairing"%in% logging)print(paste("marriage",c1,c2,"moving all to",jc," and leaving",lc, ",new:" ,population[c2,"community"],population[c1,"community"]))
			}
			stopifnot(table(population[population[,"cid"]>-1,"cid"])==2)
			coms=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
			stopifnot(coms == comus$size[as.numeric(names(coms))])
		}


		## Post-Marital Horizontal and Oblique Transmission
		population  <- social.learning(population,when='post',pathways=tp,threshold=age.threshold)

		##repro
		families=population[,"cid"]
		families=families[families>=0]
		fcount=table(families)
		stopifnot(table(families)[fcount>1] == 2)
		stopifnot(population[population[,"cid"] %in% names(fcount)[fcount==1] ,"partner"]==-1)
		repro=population[,"age"]>=maturity & population[,"age"] < endrepro & population[,"partner"] > 0
		if(sum(repro)>0){
			reprofam=table(population[repro,"community"])
			fam=population[repro,c("community","cid"),drop=F]
			fcount=table(fam[,"cid"])
			noncelib=fam[,"cid"]%in%as.numeric(names(fcount[fcount>1]))
			fam=fam[noncelib,,drop=F]
			stopifnot(nrow(fam[,"cid"])%%2 == 0) #if not even then we have someone that can autoroproduce
			fam=unique(fam)
			stopifnot(table(families) == 2) #families should be made of 2 individual
			ad_tr=comus$adaptivetraits[fam[,"community"],,drop=F]
			lbd=apply(ad_tr,1,lambda,base_rate=b,bonus_rate=r)
			lbd=lbd/ma
			newborns=runif(nrow(fam))<lbd
			nchilds=sum(newborns)
			if("demo"%in%logging )print(paste(nchilds,"new childs"))
			if(nchilds>0){
				givbirth=fam[newborns,2]
				##get all parents who gave birth
				newparents=sapply(givbirth,function(cid)which(population[,"cid"]==cid))
				##group parents together
				newparents=apply(newparents,2,function(i)population[i,],simplify=F)

                ##get couple ids and their associated communities
                famids=t(sapply(newparents,function(i)unique(i[,c("community","cid")])))
                offcom=famids[,1] ##offsprings community id
                offfid=famids[,2]  ##parents "cid" will be used  as offfspsrings family id `fid`
                offtraits=initNeutralTraits(nchilds,z=length(traitsid),nastart = T )
                ##Vertical Transmission
                offtraits[,traitsid]=t(sapply(newparents,function(parents)drawFromPool(pool.traits=parents[,traitsid],pool.sex=parents[,"sex"],sexbiases=tp$s)))

				offsprings=cbind(newpop(nchilds,minid=max(population[,"id"]),community=offcom,fid=offfid),offtraits)
				birthpercom=table(factor(offsprings[,"community"],levels=1:length(comus$size)))
				for(i in seq_along(birthpercom)){
					comus$size[i]=comus$size[i]+birthpercom[i]
				}



				##### When do horizontal and oblique take place? who are the model? how to make it modular?
				#   ##Pre-Marital Horizontal Transmission
				#   if(sum(tp$pre[,"h"])>0)
				#       offtraits[,tp$pre[,"h"]==1]=population[,traitsid[tp$pre[,"h"]==1]]
				#   if(sum(tp$pre[,"o"])>0)
				#       offtraits[,tp$pre[,"o"]==1]=t(sapply(newparents,vertical,tp=tp,tid=traitsid))


				population=rbind(population,offsprings[,colnames(population)])
				coms=table(factor(population[,"community"],levels=1:length(comus$size)))

				stopifnot(coms == comus$size[as.numeric(names(coms))])
			}
			## Pre-Marital Horizontal and Oblique Transmission
			population  <- social.learning(population,when='pre',pathways=tp,threshold=age.threshold)
		}


		## For all community where migrants here
		for(pcs in which(apply(migrantscount,1,sum)>0)){
			#Here we assum comus$size has been updated after migration
			comus$adaptivetraits[pcs,]=updateTraits(k=pcs,k.size=comus$size[pcs],alltraits=comus$adaptivetraits,migrantscount=migrantscount[pcs,],beta=beta)
		}

		#handle deaths
		dead=runif(nrow(population))<d

		if(sum(dead)>0){
			singled=population[dead,"partner"]
			com=factor(population[dead,"community"],levels=1:length(comus$size))
			deathpercom=table(com)
			if("demo"%in%logging ){
				print(paste(sum(dead),"deaths"))
				#print(paste("Death of:",paste0("ind ",apply(population[1:4,c("id","partner","cid")],1,paste0,collapse=" "),collapse=";")))
			}
			if(sum(singled>0)>0){
				population[population[,"id"] %in%  singled[singled>0],c("partner","cid")]=-1
			}
			for(i in seq_along(deathpercom)){
				comus$size[i]=comus$size[i]-deathpercom[i]
			}
			population=population[!dead,,drop=F]

            coms=table(factor(population[,"community"],levels=1:length(comus$size)))
			stopifnot(coms == comus$size[as.numeric(names(coms))])
		}

		##pm transmission

		if("migrsum"%in%out)migrsum[[time]]=apply(migrantscount,2,sum)
		comus$migrantscount=comus$migrantscount+migrantscount
		if("migrantscount"%in%logging)print(comus$migrantscount)

		## Fission
		if(!is.null(F_Th)){
			overloaded=comus$size>F_Th
			if(sum(overloaded)>0){
				for(ol in which(overloaded)){
                    if("fission"%in%logging)print(paste("splitting community",ol))
                    new.com.id=-1
                    new.comus=fissionCommunity(comus,ol)
                    if(nrow(new.comus$coordinates)>nrow(comus$coordinates)){
                        comus=new.comus
                        new.com.id=nrow(comus$coordinates)
                    }
                    population=reassignFamiliesToNewCommunityNoFIDs(ol,population,F_Th/2,new.com.id)
                    if(new.com.id == -1){
                        population=population[population[,"community"]!=-1,]
                    }
                    comus$size=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
				}

			}
		}



		#quick summary of population at time 
		if("popsumary"%in%out)popsum[[time]]=apply(population,2,table)
		if("comufull"%in%out)comufull[[time]]=comus

		##
		if("visu"%in% logging)plot(comus$coordinates,pch=21,bg=color_gradient[apply(comus$adaptivetraits,1,sum)],cex=log(comus$size),ylim=c(1,nrow(comus$occupation)),xlim=c(1,ncol(comus$occupation)))
		if("popsize"%in%out) popsize=c(popsize,nrow(population))

		coms=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
		stopifnot(coms == comus$size[as.numeric(names(coms))])
        if(nrow(population)==0){print("extinction");break}
	}
	finalres=list()
	if("popsize"%in%out)finalres[["popsize"]]=popsize
	if("popsumary"%in%out)finalres[["popsumary"]]=popsum
	if("finalpop"%in%out)finalres[["population"]]=population
	if("weddings"%in%out)finalres[["weddings"]]=weddings
	if("finalmigrants"%in%out)finalres[["finalmigrants"]]=comus$migrantscount
	if("migrsum"%in%out)finalres[["migrsum"]]=migrsum
	if("finalcomus"%in%out)finalres[["finalcomus"]]=comus
    if("comufull"%in%out)finalres[["comufull"]]=comufull
	if("done"%in%logging)print("done")
	return(finalres)
}
