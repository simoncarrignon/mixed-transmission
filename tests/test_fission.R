
testthat::test_that("test group fission  ",
                    {
replicate(100,{
              ki=sample(2:6,1)
              km=sample(2:6,1)
              mx=sample((ki+km):100,1)
              pos=random2Dgrid(K=ki+km,Gx=mx)
              a=initAdaptiveTraits(ki=ki,km=km,n=10)
              initcomus=initialiseCommunities(traits=a,coordinates=pos)
              initcomus$size=rep(N/K,K)

              potential=expand.grid(x=1:mx,y=1:mx)
              pot=sample(0:(nrow(potential)-K)) #sample a number o potential coordinate
              potential=potential[sample(1:nrow(potential),pot),] #subsample potential
              plot(initcomus$coordinates,ylim=c(0,mx),xlim=c(0,mx),pch=20,cex=4)
              ol=sample.int(K,1)
              newcom=fissionCommunity(initcomus,ol,potential)
              plot(newcom$coordinates,ylim=c(0,mx),xlim=c(0,mx),pch=20,cex=4)
})
                    })

testthat::test_that("test filling the grid",
                    {
                        replicate(5,{
                                      K=1
                                      mx=sample(1:15,1)
                                      pos=random2Dgrid(K=K,Gx=mx)
                                      a=initAdaptiveTraits(ki=K,km=0,n=10)
                                      initcomus=initialiseCommunities(traits=a,coordinates=pos,G=mx)
                                      initcomus$size=rep(10/K,K)
                                      new.comus=initcomus
                                      for(i in 1:((mx*mx)-1)){
                                          Ki=nrow(new.comus$coordinates)
                                          ol=sample.int(Ki,1)
                                          new.comus=fissionCommunity(new.comus,ol)
                                          testthat::expect_equal(Ki,i)
                                      }
                                      new.comus=fissionCommunity(new.comus,ol)
                                      testthat::expect_equal(sum(new.comus$occupation),mx*mx)

})
                    })
testthat::test_that("test randomly adding element to the grid",
                    {
replicate(50,{
              ki=sample(2:6,1)
              km=sample(2:6,1)
              mx=sample((ki+km):100,1)
              pos=random2Dgrid(K=ki+km,Gx=mx)
              a=initAdaptiveTraits(ki=ki,km=km,n=10)
              initcomus=initialiseCommunities(traits=a,coordinates=pos,G=mx)
              new.comus=initcomus
              for(i in 1:5){
                  Ki=nrow(new.comus$coordinates)
                  ol=sample.int(Ki,1)
                  new.comus=fissionCommunity(new.comus,ol)
                  testthat::expect_equal(Ki,ki+km+i-1)
              }

                })
})

neutraltraitsParam=initNeutralTraitsPathways(z = 5)
percomu=sample(1:100,1)
K=sample(2:8,1)
km=round(K/3)
ki=K-km
N=K*percomu
pos=random2Dgrid(K=K,Gx=10)
a=initAdaptiveTraits(ki=ki,km=km,n=20)
initcomus=initialiseCommunities(traits=a,coordinates=pos,G=10)
initcomus$size=rep(percomu,K)
communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(N,age="random",community = communities),initNeutralTraits(N,z))
table(reassignFamiliesToNewCommunityNoFIDs(2,population,200,-1)[,"community"])

z=sample(2:20,1)
neutraltraitsParam=initNeutralTraitsPathways(z = z)
neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$s=rbinom(z,1,.5)
traitsid=paste0("t",1:z)

testthat::test_that("test random communities and distance to one",
                    {
                        replicate(100,{
                                      km=sample(1:10,1)
                                      ki=sample(1:10,1)
                                      G=sample(4:40,1)
                                      initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=ki,km=km,n=20),coordinates=random2Dgrid(K=ki+km,Gx=G) ,G=G)
                                      potential=which(initcomus$occupation==0,arr.ind=T)
                                      ol=sample.int(nrow(initcomus$coordinates),1)
                                      plot(initcomus$coordinates,pch=21,bg=1,cex=2,ylim=c(1,nrow(initcomus$occupation)),xlim=c(1,ncol(initcomus$occupation)))
                                      points(initcomus$coordinates[ol,1],initcomus$coordinates[ol,2],col="yellow",cex=2,pch=20)
                                      distprob=apply(potential,1,function(x1,x2)sqrt(sum((x1 - x2)^2)),x2=initcomus$coordinates[ol,])
                                      points(potential,pch=21,bg=2,ylim=c(1,nrow(initcomus$occupation)),xlim=c(1,ncol(initcomus$occupation)),cex=log(distprob + 1))
                })
                    }
)

km=sample(1:10,1)
ki=sample(1:10,1)
G=sample(20:40,1)
initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=ki,km=km,n=20),coordinates=random2Dgrid(K=ki+km,Gx=G) ,G=G)
potential=which(initcomus$occupation==0,arr.ind=T)
ol=sample.int(nrow(initcomus$coordinates),1)
plot(initcomus$coordinates,pch=21,bg=1,cex=2,ylim=c(1,nrow(initcomus$occupation)),xlim=c(1,ncol(initcomus$occupation)))
points(initcomus$coordinates[ol,1],initcomus$coordinates[ol,2],col="yellow",cex=2,pch=20)
distprob=apply(potential,1,function(x1,x2)sqrt(sum((x1 - x2)^2)),x2=initcomus$coordinates[ol,])
points(potential,pch=21,bg=2,ylim=c(1,nrow(initcomus$occupation)),xlim=c(1,ncol(initcomus$occupation)),cex=log(distprob + 1))

quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=150, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop"),logging=c("visu","fission"),ma=1,traitsid=paste0("t",1:z),F_Th = 20)

km=sample(1:5,1)
ki=sample(1:5,1)
G=sample(4:8,1)
initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=ki,km=km,n=20),coordinates=random2Dgrid(K=ki+km,Gx=G) ,G=G)


z=sample(2:20,1)

neutraltraitsParam=initNeutralTraitsPathways(z = z)
neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$s=rbinom(z,1,.5)
traitsid=paste0("t",1:z)
initcomus=initialiseCommunities(traits=initAdaptiveTraits(ki=ki,km=km,n=20),coordinates=random2Dgrid(K=ki+km,Gx=G),G=G,sizes=sample(10:100,1))
communities=unlist(lapply(seq_along(initcomus$size),function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(n=sum(initcomus$size),age="random",community = communities),initNeutralTraits(sum(initcomus$size),z))
quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus"),logging=c("visu","fission"),ma=1,traitsid=paste0("t",1:z),F_Th = 20)
population=quickV$population
initcomus=quickV$finalcomus
quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus"),logging=c("visu","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 20)
population=quickV$population
initcomus=quickV$finalcomus
quickV=modelVector(K=K, m=1, b=0.2, r=0.1, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop","finalcomus"),logging=c("visu","time"),ma=1,traitsid=paste0("t",1:z),F_Th = 20)
