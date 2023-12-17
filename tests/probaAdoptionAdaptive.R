


beta=0

N=rep(100,4)
adaptivetraits=initAdaptiveTraits(2,2,n=6)

#community 1 and 2 don't have adaptive traits but 3 and 4 yes

migrants = c(0,10,0,0)

#likelyhood of community on t switch knowing that 20 migrants are comming from 2, 40 from 3 and 50 from for:

probaAdoptionAdaptiveTraits(beta=beta,migrants=migrants,N=N[1],adaptivetraits=adaptivetraits)



testthat::test_that("probaAdoptionAdaptiveTraits returns 0.5 when beta == 1",{
                        replicate(10,{
                                      counts=sample(100,4,replace=T)
                                      N=sample(100,1)
                                      proba=probaAdoptionAdaptiveTraits(beta=1,migrants=counts,N=N+sum(counts[2:4]),adaptivetraits=adaptivetraits)
                                      testthat::expect_true(all(proba==0.5))}
                        )
})

testthat::test_that("probaAdoptionAdaptiveTraits returns k/N when beta == 0",
                    {
                        replicate(5,{
                                      adaptivetraits=initAdaptiveTraits(2,2)
                                      counts=sample(100,4,replace=T)
                                      N=sample(100,1)
                                      ks=apply(adaptivetraits*counts,2,sum)
                                      ks=ks/(N+sum(counts[2:4]))
                                      proba=probaAdoptionAdaptiveTraits(beta=0,migrants=counts,N=(N+sum(counts[2:4])),adaptivetraits=adaptivetraits)
                                      testthat::expect_true(all(proba==ks))
                        })
                    })


updateTraits()

