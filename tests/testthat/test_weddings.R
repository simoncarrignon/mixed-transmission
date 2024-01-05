
testthat::test_that("test group sex and communities",
                    {
                        replicate(100,{
                                      n=sample(100:400,1)
                                      fake.pop.potential=sample(c(0,1),n,replace=T)
                                      fake.pop.comu=1:n
                                      fake.pop.sex=rbinom(n,1,0.5)

                                      testthat::expect_identical(nrow(tablePerSexAndGroup(fake.pop.potential,fake.pop.comu,fake.pop.sex)),n)
                                      fake.pop.sex=rep(0,n)
                                      tt=tablePerSexAndGroup(fake.pop.potential,fake.pop.comu,fake.pop.sex)
                                      testthat::test_that("there is no candidate", {testthat::expect_true(all(lengths(tablePerSexAndGroup(fake.pop.potential,fake.pop.comu,fake.pop.sex)[,2])==0))})
                                      testthat::test_that("there are some one candidate", {testthat::expect_false(all(lengths(tablePerSexAndGroup(fake.pop.potential,fake.pop.comu,fake.pop.sex)[,1])==0))})
                                      testthat::test_that("but max one candidate (n comu)", {testthat::expect_true(max(lengths(tablePerSexAndGroup(fake.pop.potential,fake.pop.comu,fake.pop.sex)[,1]))==1)})
})
                    }            
)

z=4
neutraltraitsParam=initNeutralTraitsPathways(z = z)
percomu=sample(1:100,1)
K=sample(2:8,1)
km=round(K/3)
ki=K-km
N=K*percomu
pos=random2Dgrid(K=K,Gx=100)
a=initAdaptiveTraits(ki=ki,km=km,n=20 )
initcomus=initialiseCommunities(traits=a,coordinates=pos,G=100)
initcomus$size=rep(percomu,K)
testthat::expect_named(initcomus)
communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
population=cbind(newpop(N,age="random",community = communities),initNeutralTraits(N,z))
testthat::expect_vector(population)


neutraltraitsParam=initNeutralTraitsPathways(z = z)
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
population=cbind(newpop(N,age="random",community = communities),initNeutralTraits(N,z))
quickV=modelVector( K=K, m=1, b=0.08, r=0.005, rho=1, d=0.01, maturity=18, endrepro=60, population=population, comus=initcomus, tstep=200, tp=neutraltraitsParam,age.threshold=generation.threshold, logging=c(""),out="finalpop",ma=1,traitsid=paste0("t",1:z))
population=quickV$population

testthat::expect_true(all(sapply(matchingSingle(population,10),function(i)population[i,"cid"])==-1))
testthat::expect_true(all(sapply(matchingSingle(population,10),function(i)population[i,"partner"])==-1))
testthat::expect_true(all(table(matchingSingle(population,10))==1))
testthat::test_that('matching is ok',{
                        replicate(10,{
                        newmatches=matchingSingle(population,10)
                        testthat::expect_true(all(sapply(newmatches,function(i)population[i,"cid"])==-1))
                        testthat::expect_true(all(sapply(newmatches,function(i)population[i,"partner"])==-1))
                        testthat::expect_true(all(table(newmatches)==1))
                        testthat::expect_true(all(table(newmatches)==1))
                        testthat::expect_true(all(population[newmatches[,1],"sex"]==0)) 
                        testthat::expect_true(all(population[newmatches[,2],"sex"]==1))
                        testthat::expect_true(all(population[newmatches[,2],"community"]!=population[newmatches[,1],"community"]))
})
                    })


