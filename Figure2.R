library(parallel)
devtools::load_all()
setwd(file.path(here::here(),"simulations")) #move to the folder with all simulations' results

data(fullpathways) #load fullpathways
pathwaysnames=paste0("c_",1:15)

renametypes=c("A","B") #ancestral pop. just A and B, no C

twotypes=list()
stratclass=c(0,1,2,3)
colstrat=colorRampPalette(c("#006400","#FFD700"))(length(stratclass)) ## we keep the three possible technology dependin on how man variant $a_i==1$ each communities have

for(beta in c(-10,0)){
    allbonus=c()
    for(bonus in c(0,1,3)){
        expname=paste0("NewPW_TraitTraj_StratTraj_500ts_RHO_0_G10_bonus_",bonus,"_beta_",beta)
        print(expname)
        allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
        cl<-makeCluster(6,type="FORK",outfile="log.txt")
        allrep=parLapply(cl,allsingle.exp,function(totes){
                             one=readRDS(totes)
                             alltypes=factor(getCommuType(one),level=stratclass) #Get technologies of the communities at the end.
                             ancestors=one$finalcomus$strat #get original strategy of communities ancestors
                             res=tapply(alltypes,ancestors,table) #count how many commmunity for each
                             names(res)=paste("Type",renametypes[seq_along(res)])
                             do.call("cbind",res)
        })
        stopCluster(cl)

        allstrats=sapply(seq_along(stratclass),function(strat)apply(sapply(allrep,function(i)i[strat,]),1,mean))
        allbonus=rbind(allbonus,allstrats)
    }
    twotypes[[as.character(beta)]]=allbonus
}

cairo_pdf(paste0("Figure2_growthbehaviour.pdf"),width=16,height=10,pointsize=20)
par(mfrow=c(1,2))
par(oma=c(0,3,0,2),mar=c(4,2,2,0),xpd=NA)
lapply(names(twotypes),function(exp){
           a=barplot(unname(t(twotypes[[exp]])),space=c(0,0,1,0,1,0),border="black",xlab="",ylab="average number of communitiies",col=colstrat,main=bquote(beta==.(exp)))
           if(exp =="-10")mtext(2,3,text="Number of communities")
           mtext(1,2,text=paste0("",c(0,1,3)*0.005),at=sapply(seq(1,length(a)-1,2),function(i)sum(a[i:(i+1)])/2),cex=1)
           text(x = a, y = par("usr")[3] - 3, labels =rownames(twotypes[[exp]]), srt = 30, adj = .5,cex=.8)
           mtext(1,2,text="f:",at=-.5,cex=1)
})

dev.off()

