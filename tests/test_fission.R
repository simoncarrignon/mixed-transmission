
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
