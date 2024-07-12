devtools::load_all()
library(parallel)
population=readRDS("population.RDS")
initcomus=readRDS("initcomus.RDS")
fullpathways=readRDS("fullpatways.RDS")
z=45
traitsid=paste0("t",1:z)

expname=paste0("BonusExploNewAges_poppaperNewRangesLimited_rho03")
dir.create(expname)
bonus=seq(0,0.02,length.out =11)
beta=seq(-2,1,length.out=11)
params= do.call("rbind",lapply(1:10,function(i)expand.grid(bonus,beta)))



cl<-makeCluster(80,type="FORK",outfile=file.path(expname,"log.txt"))
allpopsizesonly=parSapply(cl,1:nrow(params),function(b){
                              set.seed(as.numeric(Sys.time())+b)
                              tryCatch({
                                  if(!file.exists(file.path(expname,paste0("singlesimu_s_",b,".RDS")))){
                                  a=Sys.time()
                                  singlesimu=modelVector(K=K, m=1, b=0.216, r=params[b,1], rho=0, d=mortality, maturity=18, endrepro=45, population=population, comus=initcomus, tstep=300, tp=fullpathways,age.threshold=20, out=c("popsize","finalpop","finalcomus","traitsumary","comusize","traitpercomu","popwhenfull"),logging=c("done"),ma=.67,traitsid=traitsid,F_Th=100,testdebug=F,fracfiss=.5,beta=params[b,2],stopwhenfull=T)
                                  print(Sys.time()-a)
                                  saveRDS(file=file.path(expname,paste0("singlesimu_s_",b,".RDS")),singlesimu)
                                  }
                              },error=function(e){ print("problem ======");print(e)})

})
stopCluster(cl)

