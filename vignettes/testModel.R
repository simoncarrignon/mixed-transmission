
devtools::load_all(".")
N=200
Th=400 #fission threshold
ki <- 1
km <- 1
K  <-  ki+km
m=1 #proba marriage
b=.2 #HG birth rate
r=.05 #farmer scale param
rho=.5 #marriage rule
d=0.001
maturity=0
endrepro=20
tstep=20*10
z=5


population=cbind(newpop(N,age="random"),initNeutralTraits(N,z))

neutraltraitsParam=initNeutralTraitsPathways(z = z)
neutraltraitsParam$s=c(0,0,1,1)
neutraltraitsParam$pre[,"v"]=c(1,1,1,0,0)



pos=random2Dgrid(K=K,Gx=100)
initcomus=initialiseCommunities(traits=a,coordinates=pos)
initcomus$size=rep(N/K,K)
plot(initcomus$coordinates,pch=21,bg=apply(initcomus$adaptivetraits,1,mean)+1,cex=log(initcomus$size))
