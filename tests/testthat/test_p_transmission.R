testthat::test_that("no social learning",{
                        replicate(5,{
                        z = sample(1:10,1)
                        neutraltraitsParam=generatePathways(z)
                        neutraltraitsParam$tr=rep(0,z)
                        traitsid=paste0("t",1:z)
                        N=sample(10:200,1)
                        population=cbind(newpop(N,community = rep(0,N)),generateTraitsMatrix(N,z,initval=c(0,1)))
                        np=social.learning(population,when="post",pathways=neutraltraitsParam,threshold=0,traitsid=traitsid)
                        testthat::expect_true(all(apply(np[,traitsid,drop=F],2,sum)== apply(population[,traitsid,drop=F],2,sum) ))
})
})


testthat::test_that("test when no resocialisation",{
                        replicate(5,{

                                      z = 1
                                      neutraltraitsParam=generatePathways(z)
                                      neutraltraitsParam$tr=0
                                      neutraltraitsParam$post[,"h"]=rep(1,z)
                                      traitsid=paste0("t",1:z)
                                      N=sample(10:200,1)
                                      population=cbind(newpop(N,community = rep(0,N)),generateTraitsMatrix(N,z,initval=c(1,2)))
                                      population[,"justMarried"]=1
                                      population[,"cid"]=1
                                      np=social.learning(population,when="post",pathways=neutraltraitsParam,threshold=0,traitsid=traitsid)
                                      testthat::expect_true(all(np[,traitsid[1]]==population[,traitsid[1]]))
})
})


testthat::test_that("test when pure resocialisation of younger",{
                        z = 2
                        neutraltraitsParam=generatePathways(z)
                        neutraltraitsParam$tr=c(0,1)
                        neutraltraitsParam$post[,"o"]=rep(1,z)
                        traitsid=paste0("t",1:z)
                        N=100
                        population=cbind(newpop(N,community = rep(0,N)),generateTraitsMatrix(N,z,initval=c(0)))
                        population[,"justMarried"]=1
                        population[,"cid"]=1
                        population[1:50,"age"]=0
                        population[1:50,traitsid[1]]=1
                        population[51:100,"age"]=25
                        population[51:100,"cid"]=1
                        population[51:100,traitsid[2]]=1
                        np=social.learning(population,when="post",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)[,traitsid]
                        testthat::expect_true(all(np[,1]==population[,traitsid[1]]))
                        testthat::expect_true(all(np[,2]==1))
})

testthat::test_that("test when pure resocialisation of younger by different sex",{
                        z = 3
                        neutraltraitsParam=generatePathways(z)
                        neutraltraitsParam$tr=c(0,1,1)
                        neutraltraitsParam$s=c(.5,1,0)
                        neutraltraitsParam$post[,"o"]=rep(1,z)
                        traitsid=paste0("t",1:z)
                        N=100
                        population=cbind(newpop(N,community = rep(0,N)),generateTraitsMatrix(N,z,initval=c(0)))
                        population[,"justMarried"]=1
                        population[,"cid"]=1
                        population[1:50,"age"]=0
                        population[1:50,traitsid[1]]=1
                        population[1:50,traitsid[2]]=1
                        population[51:100,"age"]=25
                        population[51:100,"cid"]=1
                        population[76:100,traitsid[2]]=1
                        population[51:75,"sex"]=1
                        population[76:100,"sex"]=0
                        population[76:100,traitsid[3]]=1
                        np=social.learning(population,when="post",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)[,traitsid]
                        testthat::expect_true(all(np[,1]==population[,traitsid[1]]))
                        testthat::expect_true(all(np[1:50,2]==0)) #from old female 
                        testthat::expect_true(all(np[1:50,3]==1)) #from old male

})


testthat::test_that("test when non deterministic resocialisation ",{
                        replicate(5,{
                        z = sample(2:10,1)
                        neutraltraitsParam=generatePathways(z)
                        frac=sort(round(runif(z),1))
                        neutraltraitsParam$tr=frac
                        neutraltraitsParam$post[,"o"]=rep(1,z)
                        traitsid=paste0("t",1:z)
                        N=2000
                        population=cbind(newpop(N,community = rep(0,N)),generateTraitsMatrix(N,z,initval=c(0)))
                        population[,"justMarried"]=1
                        population[,"cid"]=1
                        g1=(1:(N/2))
                        g2=((N/2)+1):N
                        population[g1,"age"]=0
                        population[g2,"age"]=25
                        population[g2,traitsid]=1
                        np=social.learning(population,when="post",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)[,traitsid]
                        testthat::expect_true(all(sapply(1:z,function(i)round(sum(np[g1,i])/length(g1),1)==frac[i])))
        })
})


#social.learning(population,when="post",pathways=neutraltraitsParam,threshold=0,traitsid=traitsid)[50:100,]
#
#
#testthat::expect_true(all(apply(np[,traitsid],2,sum)== apply(population[,traitsid],2,sum) ))
#neutraltraitsParam=generatePathways(z)
#neutraltraitsParam$post[,"h"]=c(1,1,1,1)
#neutraltraitsParam$tr=c(0,1,.1,.9)
#neutraltraitsParam$s=rep(.5,4)
#
#
#traitsid=paste0("t",1:z)
#N=100
#population=cbind(newpop(N,community = rep(0,N)),generateTraitsMatrix(N,z,initval=0))
#population[1:10,traitsid]=1 
#population[population[,"sex"]==0,"t3"]=1
#population[,"t4"]=rbinom(N,1,prob=.5)
#population[,"community"]=sample(1:2,size=N,replace=T)
#population[1:(N/20) ,"community"]=3 
#population[1:(N/2),"community"]=1
#population[((N/2)+1):N,"community"]=2
#population[((N/2)+1):N,"t2"]=0
#population[1:(N/2),"t3"]=0
#population[1:(N/2),"t4"]=sample(0:1,size=(N/2),replace=T)
#pool=population[11:N,]
#social.learning(population,when="post",pathways=neutraltraitsParam,threshold=10,traitsid=traitsid)
#aggregate(tochange[,traitsid],by=list(tochange[,"community"]),FUN=getRatio)
#
#
