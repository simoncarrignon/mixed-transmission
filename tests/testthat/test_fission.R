
testthat::test_that("test group fission  ",
                    {
replicate(50,{
              ki=sample(2:6,1)
              km=sample(2:6,1)
              mx=sample((ki+km):100,1)
              pos=random2Dgrid(K=ki+km,Gx=mx)
              a=initAdaptiveTraits(ki=ki,km=km,n=10)
              initcomus=initialiseCommunities(traits=a,coordinates=pos,G=mx)
              initcomus$size=sample(20:50,ki+km)

              potential=expand.grid(x=1:mx,y=1:mx)
              pot=sample.int(nrow(potential),1) #sample a number o potential coordinate
              potential=potential[sample(1:nrow(potential),pot),] #subsample potential
              #plot(initcomus$coordinates,ylim=c(0,mx),xlim=c(0,mx),pch=20,cex=4)
              testthat::expect_identical(nrow(potential),pot)
              ol=sample.int(ki+km,1)
              newcom=fissionCommunity(initcomus,ol)
              testthat::expect_named(newcom)
              #plot(newcom$coordinates,ylim=c(0,mx),xlim=c(0,mx),pch=20,cex=4)
})
                    })

testthat::test_that("test filling the grid",
                    {
                        replicate(5,{
                                      K=1
                                      mx=sample(2:15,1)
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
testthat::test_that("test randomly adding element to the grid", {
                        replicate(10,{
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

testthat::test_that("test reassignFamiliesToNewCommunityNoFIDs", {
                        replicate(20,{
                                      z=5
                                      neutraltraitsParam=generatePathways(z = z)
                                      percomu=sample(1:100,1)
                                      km=sample(1:8,1)
                                      ki=km*3
                                      K=ki+km
                                      N=K*percomu
                                      pos=random2Dgrid(K=K,Gx=10)
                                      a=initAdaptiveTraits(ki=ki,km=km,n=20)
                                      initcomus=initialiseCommunities(traits=a,coordinates=pos,G=10)
                                      initcomus$size=rep(percomu,K)
                                      communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
                                      population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))
                                      testthat::expect_true(length(table(reassignFamiliesToNewCommunityNoFIDs(2,population,200,-1)[,"community"]))>0)
})
                    })


testthat::test_that("test reassignFamiliesToNewCommunityNoFIDs", {
                        replicate(20,{
                                      z=5
                                      neutraltraitsParam=generatePathways(z = z)
                                      percomu=sample(1:100,1)
                                      km=sample(1:8,1)
                                      ki=km*3
                                      K=ki+km
                                      N=K*percomu
                                      pos=random2Dgrid(K=K,Gx=10)
                                      a=initAdaptiveTraits(ki=ki,km=km,n=20)
                                      initcomus=initialiseCommunities(traits=a,coordinates=pos,G=10)
                                      initcomus$size=rep(percomu,K)
                                      communities=unlist(lapply(1:K,function(i)rep(i,initcomus$size[i])))
                                      population=cbind(newpop(N,age="random",community = communities),generateTraitsMatrix(N,z))
                                      testthat::expect_true(length(table(reassignFamiliesToNewCommunityNoFIDs(2,population,200,-1)[,"community"]))>0)
})
                    })

