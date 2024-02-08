z=45
fullpathways=generatePathways(z = z)

pw=1
for(sb in c(0,.5,1)){
    print(pw)
    for(pre in c("v","h","o")){
        fullpathways$pre[pw:(pw+3),pre]=1
        fullpathways$s[pw:(pw+3)]=sb
        pw=pw+1
        print(pw)
        fullpathways$s[pw]=sb
        for(tr in c(.1,.9)){
            for(post in c("h","o")){
                fullpathways$post[pw,post]=1
                print(pw)
                fullpathways$s[pw]=sb
                fullpathways$tr[pw]=tr
                pw=pw+1
            }
        }
    }
}
for(rho in c(1)){
pdf(file=paste0("rho_",rho,".pdf"),width=27,height=18)
par(mfrow=c(2,2))
for(beta in c(-10,0)){
for(bonus in c(0,3)){
    expname=paste0("NewPW_invertSex_TraitTraj_10t_RHO_",rho,"_G10_bonus_",bonus,"_beta_",beta)
    allsingle.exp <- list.files(expname,pattern = "si.*\\.RDS",full.names = TRUE)
    traitsfreq=sapply(allsingle.exp,function(expfname){
                          one=readRDS(expfname)
                          if(any(lengths(one$comusize)==100))
                              end=min(which(lengths(one$comusize)==100))
                          else
                              end=length(one$popsize)
                          one$traitsumary[end,]/one$popsize[end]
    })
    traitfreq=t(traitsfreq)
    colnames(traitfreq)=rep(c("-","h_l","o_l","h_h","o_h"),9)
    cols=1:3
    names(cols)=c(0,.5,1)
    par(xpd=F)
    boxplot(traitfreq,col=cols[as.character(fullpathways$s)],ylab="% of population",ylim=c(0,1),main=paste0("rho:",rho,", br:0.216 bonus:",0.005*bonus,", beta:",beta))
    bns=as.numeric(sub(".*_","",expname))
    bns=ifelse(is.na(bns),bns,1)
    #mtext(side=1,line=3,text=paste0("freq. at end rho=1, br=0.216 maxbonus=",0.005*bonus,", beta ",beta),cex=1.5)
    par(xpd=NA)
    arrows(x0=seq(1,45,5),y0=rep(max(traitfreq),9),x1=seq(1,45,5)+4,y1=rep(max(traitfreq),9),lwd=2,angle=90,code=3,length=.1)
    text(((seq(1,45,5)+3)+seq(1,45,5))/2,max(traitfreq),c("v","h","o"),pos=3)
    text(0,max(traitfreq),"pre-marital\n social learning",pos=2)
    text(0,-.1,"post-marital\n social learning",pos=2)
    legend("topright",fill=1:3,legend=c(0,.5,1),title="sex bias")
}
}
dev.off()
}
#popsize:

