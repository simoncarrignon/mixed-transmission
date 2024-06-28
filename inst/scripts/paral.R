devtools::load_all()
library(parallel)
population=readRDS("population.RDS")
initcomus=readRDS("initcomus.RDS")
fullpathways=readRDS("fullpatways.RDS")
z=45
traitsid=paste0("t",1:z)
for(rho in rev(c(0,0.5))){
    for(beta in rev(c(-10,0))){
        for(bonus in c(0,1,3)){
            expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
            dir.create(expname)
            cl<-makeCluster(35,type="FORK",outfile=file.path(expname,"log.txt"))
            allpopsizesonly=parSapply(cl,1:35,function(b){
                                          set.seed(as.numeric(Sys.time())+b)
                                          tryCatch({
                                              singlesimu=modelVector(K=K, m=1, b=0.216, r=0.005*bonus, rho=rho, d=mortality, maturity=18, endrepro=45, population=population, comus=initcomus, tstep=500, tp=fullpathways,age.threshold=20, out=c("popsize","finalpop","finalcomus","traitsumary","comusize","traitpercomu","popwhenfull"),logging=c("done"),ma=.67,traitsid=traitsid,F_Th=100,testdebug=F,fracfiss=.5,beta=beta)
                                              singlesimu$popsize
                                              id=b
                                              while(file.exists(file.path(expname, paste0("singlesimu_s_", id, ".RDS")))) id <- id + 1
                                              saveRDS(file=file.path(expname,paste0("singlesimu_s_",id,".RDS")),singlesimu)

                                          },error=function(e){ print("problem ======");print(e)})
})
            stopCluster(cl)
            saveRDS(file=file.path(expname,"neutralTraits_longer.RDS"),allpopsizesonly)
        }
    }
}


