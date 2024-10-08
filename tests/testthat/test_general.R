N=200
Th=400 #fission threshold
ki <- 1
km <- 1
K  <-  ki+km
m=1 #proba marriage
rho=.5 #marriage rule
d=0.001
maturity=0
endrepro=20
tstep=20*10
generation.threshold = 20
z=5
traitsid=paste0("t",1:z)



neutraltraitsParam=generatePathways(z = z)
neutraltraitsParam$s=c(0,1,0,1,0)
neutraltraitsParam$pre[,"v"]=c(1,1,1,1,1) #this needs to be always like this otherwise there are NA traits
neutraltraitsParam$pre[,"o"]=c(0,0,0,1,1)
neutraltraitsParam$pre[,"h"]=c(0,0,1,1,1)
neutraltraitsParam$post[,"i"]=c(1,1,0,0,0)
neutraltraitsParam$post[,"o"]=c(0,0,0,1,1)
neutraltraitsParam$post[,"h"]=c(0,0,1,1,1)

pos=random2Dgrid(K=K,Gx=100)
a=initAdaptiveTraits(ki=ki,km=km)
initcomus=initialiseCommunities(traits=a,coordinates=pos,G=100)
initcomus$size=rep(N/K,K)

communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))

testthat::test_that('Quick general test',{
                        suppressWarnings(
                        replicate(5,
                                  {
                                      testthat::expect_warning(regexp="famillies not*",
                                                               {
                                                                   quickV=modelVector(K=K, m=1, b=0, r=0, rho=1, d=0, maturity=50, endrepro=60, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=generation.threshold, out=NULL,logging=NULL,ma=1,traitsid=paste0("t",1:z),warn=T)
                                                               })
                                  })
                        )
})

testthat::test_that('Testing popsize hasnt changed',{
                        suppressWarnings(
                        testthat::expect_true({
                            quickV=modelVector(K=K, m=1, b=0, r=0, rho=1, d=0, maturity=50, endrepro=60, population=population, comus=initcomus, tstep=50, tp=neutraltraitsParam,age.threshold=generation.threshold, out="popsize",logging=NULL,ma=1,traitsid=paste0("t",1:z))
                            all(quickV$popsize == nrow(population))
                        })
)
                    })

testthat::test_that("Testing when all pathways at 0 that traits distribution hasn't changed",
                    {
                        suppressWarnings({
                            quickV=modelVector(K=K, m=1, b=0, r=0, rho=1, d=0, maturity=50, endrepro=60, population=population, comus=initcomus, tstep=50, tp=generatePathways(z = z),age.threshold=generation.threshold, out="finalpop",logging=NULL,ma=1,traitsid=paste0("t",1:z))
testthat::expect_true(all( apply(quickV$population[,traitsid],2,sum)[1:2]== apply(population[,traitsid],2,sum)[1:2]))
                        }
)
                    })


testthat::test_that("model don't change when no growth and no social learning",{
                        neutraltraitsParam$pre[,"v"]=c(1,1,1,1,1) #this needs to be always like this otherwise there are NA traits
                        neutraltraitsParam$pre[,"o"]=c(0,0,0,0,0)
                        neutraltraitsParam$pre[,"h"]=c(0,0,0,0,0)
                        neutraltraitsParam$post[,"i"]=c(0,0,0,0,0)
                        neutraltraitsParam$post[,"o"]=c(0,0,0,0,0)
                        neutraltraitsParam$post[,"h"]=c(0,0,0,0,0)

                        replicate(5,
                                  {

                                      quickV=modelVector(K=K, m=1, b=0, r=0, rho=1, d=0, maturity=sample(100,1), endrepro=sample(100,1), population=population, comus=initcomus, tstep=sample(4:150,1), tp=neutraltraitsParam,age.threshold=sample(100,1), out=c("finalpop"),logging="",ma=1,traitsid=traitsid)

                                      testthat::expect_true(all(apply(quickV$population[,traitsid],2,sum)== apply(population[,traitsid],2,sum) ))
                                      testthat::expect_true(nrow(population)== nrow(population))
                                  })
})

testthat::test_that("model don't when only social learning but no grow",{
                        replicate(5,
                                  {

                                      neutraltraitsParam$pre[,"v"]=c(1,1,1,1,1) #this needs to be always like this otherwise there are NA traits
                                      neutraltraitsParam$pre[,"o"]=c(1,1,1,1,1)
                                      neutraltraitsParam$pre[,"h"]=c(1,1,1,1,1)
                                      neutraltraitsParam$post[,"i"]=c(0,0,0,0,0)
                                      neutraltraitsParam$post[,"o"]=c(1,1,1,1,1)
                                      neutraltraitsParam$post[,"h"]=c(1,1,1,1,1)

                                      quickV=modelVector(K=K, m=1, b=0.07, r=0, rho=1, d=0.001, maturity=sample(100,1), endrepro=sample(100,1), population=population, comus=initcomus, tstep=10, tp=neutraltraitsParam,age.threshold=sample(100,1), out=c("finalpop"),logging=c(""),ma=1,traitsid=paste0("t",1:z))

                                      testthat::expect_false(all(apply(quickV$population[,traitsid],2,sum)== apply(population[,traitsid],2,sum) ))
                                      testthat::expect_true(nrow(population)== nrow(population))
                                  })
})

testthat::test_that("model when only social learning changes but no grow",{
                        replicate(5,
                                  {
                                      z=sample(2:20,1)

                                      neutraltraitsParam=generatePathways(z = z)
                                      neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
                                      neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
                                      neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
                                      neutraltraitsParam$s=rbinom(z,1,.5)

                                      N=200
                                      population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))
                                      population[1:50,"age"]=0
                                      quickV=modelVector(K=K, m=1, b=0, r=0, rho=1, d=0, maturity=1, endrepro=50, population=population, comus=initcomus, tstep=10, tp=neutraltraitsParam,age.threshold=20, out=c("finalpop"),logging=c("",""),ma=1,traitsid=paste0("t",1:z))
                                      traitsid=paste0("t",1:z)
                                      testthat::expect_false(all(apply(quickV$population[,traitsid],2,sum)== apply(population[,traitsid],2,sum) ))
                                      testthat::expect_true(nrow(population)== nrow(population))
                                  })
})


z=sample(2:20,1)
neutraltraitsParam=generatePathways(z = z)
neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
neutraltraitsParam$s=rbinom(z,1,.5)
N=200
population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))
quickV=modelVector(K=K, m=1, b=0.01, r=0.005, rho=1, d=0.01, maturity=10, endrepro=65, population=population, comus=initcomus, tstep=200, tp=neutraltraitsParam,age.threshold=10, out=c("finalpop"),logging=NULL,ma=1,traitsid=paste0("t",1:z))
traitsid=paste0("t",1:z)

testthat::expect_false(all(apply(quickV$population[,traitsid],2,sum)== apply(population[,traitsid],2,sum) ))
                                      testthat::expect_true(nrow(population)== nrow(population))

testthat::test_that("model when random social learning, steady grow/decrease",{
                        replicate(5,
                                  {
                                      z=sample(2:20,1)

                                      neutraltraitsParam=generatePathways(z = z)
                                      neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
                                      neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
                                      neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
                                      neutraltraitsParam$s=rbinom(z,1,.5)
                                      traitsid=paste0("t",1:z)

                                      percomu=sample(1:100,1)
                                      K=sample(2:8,1)
                                      km=round(K/3)
                                      ki=K-km
                                      N=K*percomu
                                      pos=random2Dgrid(K=K,Gx=100)
                                      a=initAdaptiveTraits(ki=ki,km=km,n=20 )
                                      initcomus=initialiseCommunities(traits=a,coordinates=pos,G=100)
                                      initcomus$size=rep(percomu,K)
                                      communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
                                      population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))
                                      quickV=suppressWarnings(modelVector(K=K, m=1, b=0.07, r=0, rho=1, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=sample(50:60,1), tp=neutraltraitsParam,age.threshold=sample(100,1), out=c("finalpop"),logging="",ma=1,traitsid=paste0("t",1:z)))

                                      testthat::expect_named(quickV)
                                  })
})

testthat::test_that("model everything, everything random: social learning,adaptive learning,pathways...",
                    {
                        replicate(5,
                                  {
                                      z=sample(2:20,1)

                                      neutraltraitsParam=generatePathways(z = z)
                                      neutraltraitsParam$post[,"o"]=rbinom(z,1,.5)
                                      neutraltraitsParam$post[,"i"]=rbinom(z,1,.5)
                                      neutraltraitsParam$post[,"h"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"o"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"v"]=rbinom(z,1,.5)
                                      neutraltraitsParam$pre[,"h"]=rbinom(z,1,.5)
                                      neutraltraitsParam$s=round(runif(z),1)
                                      neutraltraitsParam$tr=round(runif(z),1)
                                      traitsid=paste0("t",1:z)
                                      percomu=sample(1:100,1)
                                      K=sample(2:8,1)
                                      km=round(K/3)
                                      ki=K-km
                                      N=K*percomu
                                      pos=random2Dgrid(K=K,Gx=100)
                                      a=initAdaptiveTraits(ki=ki,km=km,n=20 )
                                      initcomus=initialiseCommunities(traits=a,coordinates=pos,G=100,ki=ki,km=km)
                                      initcomus$size=rep(percomu,K)
                                      communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
                                      population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))
                                      quickV=suppressWarnings(modelVector(K=K, m=1, b=0.07, r=0.005, rho=0, d=0.01, maturity=18, endrepro=65, population=population, comus=initcomus, tstep=sample(50:60,1), tp=neutraltraitsParam,age.threshold=20, out=c("finalpop"),logging=c(""),ma=1,traitsid=paste0("t",1:z)))
                                      testthat::expect_true(max(quickV$population[,traitsid])<=1)
                                      testthat::expect_true(min(quickV$population[,traitsid])>=0)
                                  })
})
