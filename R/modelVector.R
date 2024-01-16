
#' Model a Population Vector over Time
#'
#' This function simulates the evolution of a population over a given number of time steps. 
#' It models various aspects such as marriages, migrations, community changes, and more.
#'
#' @param N overall population size
#' @param F_Th Threshold for community fission, NULL by default. When a community's size exceeds this threshold, it splits.
#' @param ki,km,K,m,b,r,rho,d,maturity,endrepro,a,tp,age.threshold,population,comus,tstep,ma,traitsid Specific parameters used in the population model. Set marriage, communities etc...
#' @param logging Controls the level of logging. implemented so far: "time" ,"demo" ,"pairing" ,"migrantscount" ,"fission" ,"visu" ,"done"
#' @param getfinalpop Logical flag to determine if the final population should be returned.
#' @param out Character vector specifying the types of output to be generated. Possible values include "popsize" for population size and "popsumary" for a summary of the population.  implemented so far "popsize" ,"popsumary" ,"weddings" ,"traitsumary" ,"migrsum" ,"comufull" ,"finalpop" ,"finalmigrants" ,"finalcomus"
#' @param beta  parameter to adjust impact of migrant pop size model, with a default value of 0
#' @param vidfile Optional file path for saving video output, NULL by default.
#' @param warn Logical flag to control the display of warnings.
#' @param testdebug Logical flag to control stop test for debugging
#' @param remarriage Logical flag to control if widower can remarry or not
#' @param popcapsize Stop the population if it reach a certain size
#' 
#' @return A list containing various elements depending on the 'out' parameter. Elements can include population size, population summary, final population, and others as specified in 'out'.
#'
#' @examples
#' # Example usage of the modelVector function
#' #result <- modelVector(N = 100, ki = 1, km = 1, K = 5, m = 0.5, b = 0.1, r = 0.05, rho = 0.5, d = 0.01, 
#' #                     maturity = 18, endrepro = 50, a = 1, tp = tp_param, age.threshold = 20, 
#' #                     population = initial_population, comus = initial_comus, tstep = 10, 
#' #                     ma = 1, traitsid = 1:5, getfinalpop = TRUE, out = c("popsize", "popsumary"))
#'
#' @importFrom stats runif   
#' @export

modelVector <- function(N, F_Th=NULL, ki,km,K,m, b, r, rho=.5, d, maturity, endrepro,a,tp,age.threshold=20,population,comus,logging="time",tstep,ma=1,traitsid,getfinalpop=FALSE,out=c("popsize","popsumary"),beta=0,vidfile=NULL,warn=FALSE,testdebug=FALSE,remarriage=FALSE,popcapsize=NULL){
	if("popsize"%in%out) popsize=nrow(population)
	if("deaths"%in%out) deaths=c()
	if("births"%in%out) births=c()
	if("repros"%in%out) repros=c()
	if("weddings"%in%out) weddings=0
	if("popsumary"%in%out){
		popsum=list()
		popsum[[1]]=rep(0,length(comus$popsize))
	}
	if("traitsumary"%in%out){
		traitsum=list()
		traitsum[[1]]=apply(population[,traitsid,drop=F],2,sum)
    }
	if("migrsum"%in%out){
		migrsum=list()
		migrsum[[1]]=rep(0,K)
	}

    if("popfull"%in%out) {
		popfull=list()
		popfull[[1]]=population
	}
    if("comufull"%in%out){
        comufull=list()
        comufull[[1]]=comus
    }
	if(is.null(comus$migrants) ){
		#We nee a K x K  to store after migration from where are comming 
		comus$migrantscount=matrix(0,nrow=nrow(comus$adaptivetraits),nrow(comus$adaptivetraits))
	}
	if(is.null(vidfile)) stepfile=NULL
	else stepfile=vidfile
    if("visu"%in% logging)plot.comu(comus,vidfile=stepfile)
	for(time in 2:tstep){
		if("time"%in%logging)print(paste("------",time,"------"))
		population[,"age"]=population[,"age"]+1
		couple=which(population[,"partner"]>-1)

		if(testdebug){
            coms=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
            stopifnot(coms == comus$size[as.numeric(names(coms))])
        }


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
                    if(testdebug) stopifnot(population[c2,"sex"]==0) #temp test
				}
				else{
					fc=population[c2,"community"]
					mc=population[c1,"community"]
					if(testdebug) stopifnot(population[c1,"sex"]==0) #temp test

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
                    if(warn)warning("same community couple in the population")
                    local=sample(local,1)
                }
                inlawtraits=tp$post[,"i"]==1
                if(sum(inlawtraits)>0){
                    if(population[local,"fid"]==-1 && warn){warning("famillies not created yet, inlaw copying impossible")}
                    else{
                        it=traitsid[inlawtraits]
                        inlaws=population[ population[,"cid"] == population[local,"fid"],,drop=F]
                        if(dim(inlaws)[1]>0)
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
            if(testdebug){
                stopifnot(table(population[population[,"cid"]>-1,"cid"])==2)
                coms=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
                stopifnot(coms == comus$size[as.numeric(names(coms))])
            }
        }


		## Post-Marital Horizontal and Oblique Transmission
		population  <- social.learning(population,when='post',pathways=tp,threshold=age.threshold)

		##repro
		families=population[,"cid"]
		families=families[families>=0]
		fcount=table(families)
        if(testdebug){
            stopifnot(table(families)[fcount>1] == 2)
            stopifnot(population[population[,"cid"] %in% names(fcount)[fcount==1] ,"partner"]==-1)
        }
		repro=validCouple(population,maturity=maturity,endrepro=endrepro)
		if("repros"%in%out) repros=c(repros,sum(repro))
        nchilds=0
		if(sum(repro)>0){
			reprofam=table(population[repro,"community"])
			fam=population[repro,c("community","cid"),drop=F]
			fcount=table(fam[,"cid"])
			noncelib=fam[,"cid"]%in%as.numeric(names(fcount[fcount>1]))
			fam=fam[noncelib,,drop=F]
            if(testdebug){
                stopifnot(nrow(fam[,"cid"])%%2 == 0) #if not even then we have someone that can autoroproduce
                stopifnot(table(families) == 2) #families should be made of 2 individual
            }

			fam=unique(fam)
			ad_tr=comus$adaptivetraits[fam[,"community"],,drop=F]
			lbd=apply(ad_tr,1,lambda,base_rate=b,bonus_rate=r)
			lbd=lbd/ma
			newborns=runif(nrow(fam))<lbd
			nchilds=sum(newborns)
			if("demo"%in%logging )print(paste(nchilds,"new childs",paste(fam[newborns,"cid"],collapse=",")))
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
                offtraits=generateTraitsMatrix(nchilds,z=length(traitsid),nastart = T )
                ##Vertical Transmission
                offtraits[,traitsid]=t(sapply(newparents,function(parents)drawFromPool(pool.traits=parents[,traitsid],pool.sex=parents[,"sex"],sexbiases=tp$s)))

				offsprings=cbind(newpop(nchilds,minid=max(population[,"id"]),community=offcom,fid=offfid),offtraits)
				birthpercom=table(factor(offsprings[,"community"],levels=1:length(comus$size)))
				comus$size=comus$size+birthpercom

				population=rbind(population,offsprings[,colnames(population)])
                if(testdebug){
                    coms=table(factor(population[,"community"],levels=1:length(comus$size)))
                    stopifnot(coms == comus$size[as.numeric(names(coms))])
                }
			}
			## Pre-Marital Horizontal and Oblique Transmission
			population  <- social.learning(population,when='pre',pathways=tp,threshold=age.threshold)
		}
        if("births"%in%out) births=c(births,nchilds)


		## For all community where migrants here
		for(pcs in which(apply(migrantscount,1,sum)>0)){
			#Here we assum comus$size has been updated after migration
			comus$adaptivetraits[pcs,]=updateTraits(k=pcs,k.size=comus$size[pcs],alltraits=comus$adaptivetraits,migrantscount=migrantscount[pcs,],beta=beta)
		}

		#handle deaths
		dead=runif(nrow(population))<d
		if("deaths"%in%out) deaths=c(deaths,sum(dead))

		if(sum(dead)>0){
			singled=population[dead,"partner"]
			com=factor(population[dead,"community"],levels=1:length(comus$size))
			deathpercom=table(com)
			if("demo"%in%logging ){
				print(paste(sum(dead),"deaths"))
				#print(paste("Death of:",paste0("ind ",apply(population[1:4,c("id","partner","cid")],1,paste0,collapse=" "),collapse=";")))
			}
			if(remarriage & sum(singled>0)>0){
				population[population[,"id"] %in%  singled[singled>0],c("partner","cid")]=-1
			}
			comus$size=comus$size-deathpercom
			population=population[!dead,,drop=F]

            if(testdebug){
                coms=table(factor(population[,"community"],levels=1:length(comus$size)))
                stopifnot(coms == comus$size[as.numeric(names(coms))])
            }
		}

		##pm transmission

		if("migrsum"%in%out)migrsum[[time]]=apply(migrantscount,2,sum)
		comus$migrantscount=comus$migrantscount+migrantscount
		if("migrantscount"%in%logging)print(comus$migrantscount)

		## Fission
		if(!is.null(F_Th)){
			overloaded=comus$size>F_Th
			if(sum(overloaded)>0){
                if("fission"%in%logging)print(paste("fission starting"))
                if("fission"%in%logging)print(table(population[,"community"]))
				for(ol in which(overloaded)){
                    new.com.id=-1
                    new.comus=fissionCommunity(comus,ol)
                    if(nrow(new.comus$coordinates)>nrow(comus$coordinates)){
                        new.com.id=nrow(new.comus$coordinates)
                    }
                    newsize=comus$size[ol]/2 #how many people should leave  _ol_
                    population=reassignFamiliesToNewCommunityNoFIDs(ol,population,newsize,new.com.id)
                    oldol=comus$size[ol]
                    new.comus$size=table(factor(population[,"community"],levels=1:nrow(new.comus$coordinates)))
                    if(new.com.id == -1){
                        population=population[population[,"community"]!=-1,]
                        comus$size=table(factor(population[,"community"],levels=1:nrow(new.comus$coordinates)))
                        if("fission"%in%logging)print(paste("splitting community",ol,",",newsize,"should leave",oldol-comus$size[ol],"actually did"))
                    }
					if(new.com.id != -1){
						if(new.comus$size[new.com.id]>0){
							comus=new.comus
							if("fission"%in%logging)print(paste0("splitting community ",ol," ( of size ",comus$size[ol],", was ",oldol,") in comu: ",new.com.id," ( of size ",comus$size[new.com.id],"); expected migrant:",newsize,""))
						}
						else
							if("fission"%in%logging)print(paste0("failed splitting community ",ol," (of size ",comus$size[ol],", was ",oldol,") in comu: ",new.com.id," (of size ",new.comus$size[new.com.id],"); expected migrant:",newsize,""))
					}
				}

			}
		}



		#quick summary of population at time 
		if("popsumary"%in%out)popsum[[time]]=apply(population,2,table)
		if("traitsumary"%in%out)traitsum[[time]]=apply(population[,traitsid,drop=F],2,sum)
		if("comufull"%in%out)comufull[[time]]=comus
		if("popfull"%in%out) popfull[[time]]=population

		##
		if(!is.null(vidfile))stepfile=sprintf(paste0(vidfile,"%05d"),time)
		if("visu"%in% logging)plot.comu(comus,vidfile=stepfile)
		if("visu"%in% logging)mtext(time,3,1)
		if("popsize"%in%out) popsize=c(popsize,nrow(population))

		if(testdebug){
            coms=table(factor(population[,"community"],levels=1:nrow(comus$coordinates)))
            stopifnot(coms == comus$size[as.numeric(names(coms))])
        }
        if(nrow(population)==0){print("extinction");break}
        if(!is.null(popcapsize))if(nrow(population)>popcapsize){print("pop cap reached");break}
	}
	finalres=list()
	if("popsize"%in%out)finalres[["popsize"]]=popsize
	if("deaths"%in%out)finalres[["deaths"]]=deaths
	if("births"%in%out)finalres[["births"]]=births
	if("repros"%in%out)finalres[["repros"]]=repros
	if("popsumary"%in%out)finalres[["popsumary"]]=popsum
	if("traitsumary"%in%out)finalres[["traitsumary"]]=do.call("rbind",traitsum)
	if("finalpop"%in%out)finalres[["population"]]=population
	if("weddings"%in%out)finalres[["weddings"]]=weddings
	if("finalmigrants"%in%out)finalres[["finalmigrants"]]=comus$migrantscount
	if("migrsum"%in%out)finalres[["migrsum"]]=migrsum
	if("finalcomus"%in%out)finalres[["finalcomus"]]=comus
    if("comufull"%in%out)finalres[["comufull"]]=comufull
    if("popfull"%in%out)finalres[["popfull"]]=popfull
	if("done"%in%logging)print("done")
	return(finalres)
}
