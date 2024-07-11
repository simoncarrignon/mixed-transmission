##EXPLORE Growth Rate obtained for all our simulations
devtools::load_all()
setwd(file.path(here::here(),"simulations"))

bonus=c(0,1,3)
names(bonus)=c(0,0.005,0.015)
alleffect = lapply(bonus, function(f) {
   #get all file name of all experiment for on bonus value
  allfilename = unlist(sapply(c(-10, 0), function(beta) {
    sapply(c(0, 0.5), function(rho) {
      expname = paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_", rho, "_G10_bonus_", f, "_beta_", beta)
      list.files(expname, pattern = "si.*\\.RDS", full.names = TRUE)
    })
  }))
  sapply(allfilename, function(expr) {
    singlesimu = readRDS(expr)
    pop = singlesimu$popsize
    end = getSimFull(singlesimu)
    pop = pop[1:end]
    mean((pop[-1] - pop[-end]) / pop[-end])
  })
})
par(mar=c(4,6,2,2))
boxplot(alleffect,ylab=expression(frac(N[t+1]-N[t],N[t])),xlab="f")

