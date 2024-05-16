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

rho=0 
traitsel=c(1:5,16:20,31:35)
renametypes=c("A","B & C") #ancestral pop. just A and B, no C
allresults_types=lapply(1:2,function(ou)lapply(c(-10,0),function(beta)lapply(c(0,1,3),function(bonus) extractResults(paste0(exprefix,"RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta),traitsel=traitsel,pathways=fullpathways,type=T,ntype=2)[[ou]])))

##for the supplementary we rearange a bit the things; firs f0.05 becom f0, then f0 mixe the two types 
allresults_types=lapply(allresults_types,function(t1){
t1[[1]][[2]]=t1[[1]][[1]]
t1[[2]][[2]]=t1[[2]][[1]]
t1})

type1f0=allresults_types[[1]][[1]][[1]]
type2f0=allresults_types[[2]][[1]][[1]]
bothetypes=lapply(seq_along(type1f0),function(i)c(type1f0[[i]],type2f0[[i]]))
allresults_types[[1]][[1]][[1]]=bothetypes
allresults_types[[2]][[1]][[1]]=bothetypes
t1[[1]][[1]]=beta10


for(ctype in 1:2){
	cairo_pdf(paste0("Figure4_Type",gsub("[ &]+","_",renametypes[ctype]),".pdf"),width=12,height=10,pointsize=15)
	alldensities=lapply(allresults_types[[ctype]],function(beta)lapply(beta,function(bonus)lapply(bonus,getDensities,bw=0.01)))
	joyplotA(alldensities,pathwaysnames=pathwaysnames[1:5],yadj=2,angle=5,miniaxes=T,baselines=T)
if(ctype==1)legend("bottomright",fill=rev(adjustcolor(sexcols,.8)),legend=rev(names(sexcols)),title="bias",bty="n")
     text(1,7,paste("Type",renametypes[ctype]))
	dev.off()
}
