
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
                        replicate(10,{
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
