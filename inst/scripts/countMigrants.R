
singlesimu=modelVector(K=K, m=1, b=0.3, r=0.005*1, rho=rho, d=mortality, maturity=18, endrepro=45, population=population, comus=initcomus, tstep=200, tp=fullpathways,age.threshold=20, out=c("popsize","finalpop","finalcomus","traitsumary","comusize","traitpercomu","popfull","migrantscount","comufull"),logging=c("done","time","pairing"),ma=.67,traitsid=traitsid,F <- Th=100,testdebug=F,fracfiss=.5,beta=beta,popcapsize=4000,up=3)



allmigrants=sapply(3:length(singlesimu$comufull),function(t){
           prev=singlesimu$comufull[[t-1]]$migrantscount
           new=singlesimu$comufull[[t]]$migrantscount[1:dim(prev)[1],1:dim(prev)[2]]
           apply(new-prev,2,sum)/singlesimu$comufull[[t]]$size[1:dim(prev)[1]]
})


par(mar=c(5,5,2,5))
boxplot(allmigrants,xlab="time",ylab="% of migrant in community population")
par(new=T)
plot(lengths(singlesimu$comusize),lwd=3,col="green",ann=F,axes=F,type="l")
axis(4,col="green")
mtext("# comus",4,2,col="green")
