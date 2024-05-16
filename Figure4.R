devtools::load_all()
sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)
require(latex2exp)
setwd(file.path(here::here(),"simulations"))


exprefix="NewPW_TraitTraj_StratTraj_500ts_"
pathwaysnames=c(
           "c_1","c_{2},p_{transmit}=0.9","c_2","c_{3},p_{transmit}=0.9","c_3",
           "c_4","c_{5},p_{transmit}=0.9","c_5","c_{6},p_{transmit}=0.9","c_6",
           "c_7","c_{8},p_{transmit}=0.9","c_8","c_{9},p_{transmit}=0.9","c_9")
pathwaysnames=paste("c_",1:5)

betas=c(-10,0)
bonuses=c(0,1,3)
rho=0
allfilenames=lapply(betas,function(ba)lapply(bonuses,function(bo)paste0(exprefix,"RHO_",rho,"_G10_bonus_",bo,"_beta_",ba)))

allexpe=lapply(allfilenames,lapply,list.files,pattern = "si.*\\.RDS",full.names = TRUE)


#Get all  pathways from the paper they each correspon to 5 pathway for 3 pbias
traitsel=c(1:5,16:20,31:35)

## First use the function extractResults to extract the percentage of $c_i ==$ in all communities, for all simulation, for all pathways, for the three bonuses and the two betas
allresults=lapply(c(-10,0),function(beta)lapply(c(0,1,3),function(bonus) extractResults(paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta),traitsel=traitsel,pathways=fullpathways)))

## we convert the percentages in density estimates
bw=.01 #bandwith of the kernel estimate 
alldensities=lapply(allresults,function(beta)lapply(beta,function(bonus)lapply(bonus,getDensities,bw=bw)))

alpha=.8 # transparancy used for the paper

cairo_pdf("Figure4.pdf",10,10,pointsize=22)
joyplot(alldensities,pathwaysnames=pathwaysnames[1:5],yadj=1.5,angle=5,miniaxes=T,baselines=T)
legend("bottomright",fill=rev(adjustcolor(sexcols,alpha)),legend=rev(names(sexcols)),title="bias",bty="n")
dev.off()

