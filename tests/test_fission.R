
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
