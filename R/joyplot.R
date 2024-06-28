joyplot <- function(listdensities,pathwaysnames=pathwaysnames,main="",angle=25,yadj=2,sep_ba=c(0,.4),alpha=0.8,baselines=F,miniaxes=F){


    tot_traits=length(listdensities[[1]][[1]])
    sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
    names(sexcols)=c(0,.5,1)
    allcols=unlist(lapply(sexcols,rep,tot_traits/length(sexcols)))
    par(mar=c(0,0,0,0),oma=c(1,0,2,1),xpd=NA)
    

    xadj=ifelse(angle==0,0,1/angle)
    ylims=c(0,(tot_traits+1)/yadj)
    xlims=c(0,.7+sum(sep_ba)+(1+tot_traits)*xadj+5*(1.3+xadj))
    plot(1,1,type="n",xlim=xlims,ylim=ylims,axes=F,ann=F)
	par(xpd=F)
    if(baselines){
# abline(h=(1:tot_traits)/yadj,lwd=.4,lty=2)
    segments(x0=rep(0,tot_traits)+1:tot_traits*xadj,y0=(1:tot_traits)/yadj,y1=(1:tot_traits)/yadj,x1=(1+0.7+xadj)*5+max(sep_ba)+(xadj*(1:tot_traits)-0.4),lwd=.4,lty=2)
}
	par(xpd=NA)
    xs=c()
    xs=c(xs,.7+xadj+listdensities[[1]][[1]][[1]][1,1])
    for(tr in length(listdensities[[1]][[1]]):1){
        #First column, f0
        polygon(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1],tr/yadj+listdensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
        if(miniaxes){
            na=range(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1])
            arrows(x0=na[1],y0=tr/yadj,x1=(na[1]+na[2])/2,y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
            arrows(x0=(na[1]+na[2])/2,y0=tr/yadj,x1=na[2],y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
            text(c(0,0.5,1),x=c(na[1],(na[2]+na[1])/2,na[2]),y=tr/yadj+c(0,0,0)+0.1,cex=.4,pos=1)
        }
    }
    for(ba in 1:length(listdensities)){
        for(bo in 2:length(listdensities[[ba]])){
            xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.6+tr*xadj+listdensities[[ba]][[bo]][[1]][1,1])
            for(tr in length(listdensities[[ba]][[bo]]):1){
                polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr*xadj+listdensities[[ba]][[bo]][[tr]][,1],tr/yadj+listdensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
            }
        }
    }
    text(x=rep(0,tot_traits)+1:tot_traits*xadj,y=(1:tot_traits)/yadj,label=TeX(lapply(rep(pathwaysnames,3),function(i)paste("$",i,"$"))),las=2,pos=2,cex=.8)
    text(y=0,x=xs+.5,label=c("0",0.005,0.015,0.005,0.015))
    text(y=0,x=xs[1],label="f:")
    text(y=max(ylims)+1/yadj,x=tot_traits*xadj+xs[1]+.5,label=expression(beta~":"))
    text(y=max(ylims)+1/yadj,x=tot_traits*xadj+xs[c(2,4)]+1,label=c(-10,0))
    mtext(main,3,-1,outer=T)

}


#joyplotAllBeta <- function(listdensities,pathwaysnames=pathwaysnames,main="",angle=25,yadj=2,sep_ba=c(0,.4),alpha=0.8,baselines=F,miniaxes=F){
#
#
#    tot_traits=length(listdensities[[1]][[1]])
#    sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
#    names(sexcols)=c(0,.5,1)
#    allcols=unlist(lapply(sexcols,rep,tot_traits/length(sexcols)))
#    par(mar=c(0,0,0,0),oma=c(1,0,2,1),xpd=NA)
#    
#
#    xadj=ifelse(angle==0,0,1/angle)
#    ylims=c(0,(tot_traits+1)/yadj)
#    xlims=c(0,.7+sum(sep_ba)+(1+tot_traits)*xadj+6*(1.3+xadj))
#    plot(1,1,type="n",xlim=xlims,ylim=ylims,axes=F,ann=F)
#	par(xpd=F)
#    if(baselines){
## abline(h=(1:tot_traits)/yadj,lwd=.4,lty=2)
#    segments(x0=rep(0,tot_traits)+1:tot_traits*xadj,y0=(1:tot_traits)/yadj,y1=(1:tot_traits)/yadj,x1=(1+0.7+xadj)*6+max(sep_ba)+(xadj*(1:tot_traits)-0.4),lwd=.4,lty=2)
#}
#	par(xpd=NA)
#    xs=c()
#    xs=c(xs,.7+xadj+listdensities[[1]][[1]][[1]][1,1])
#    for(tr in length(listdensities[[1]][[1]]):1){
#        #First column, f0
#        polygon(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1],tr/yadj+listdensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
#        if(miniaxes){
#            na=range(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1])
#            arrows(x0=na[1],y0=tr/yadj,x1=(na[1]+na[2])/2,y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
#            arrows(x0=(na[1]+na[2])/2,y0=tr/yadj,x1=na[2],y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
#            text(c(0,0.5,1),x=c(na[1],(na[2]+na[1])/2,na[2]),y=tr/yadj+c(0,0,0)+0.1,cex=.4,pos=1)
#        }
#    }
#    xs=c(xs,.7+xadj+listdensities[[2]][[1]][[1]][1,1])
#    for(tr in length(listdensities[[2]][[1]]):1){
#        #First column, f0
#        polygon(.7+tr*xadj+listdensities[[2]][[1]][[tr]][,1],tr/yadj+listdensities[[2]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
#        }
#    }
#    for(ba in 1:length(listdensities)){
#        for(bo in 2:length(listdensities[[ba]])){
#            xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.6+tr*xadj+listdensities[[ba]][[bo]][[1]][1,1])
#            for(tr in length(listdensities[[ba]][[bo]]):1){
#                polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr*xadj+listdensities[[ba]][[bo]][[tr]][,1],tr/yadj+listdensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
#            }
#        }
#    }
#    text(x=rep(0,tot_traits)+1:tot_traits*xadj,y=(1:tot_traits)/yadj,label=TeX(lapply(rep(pathwaysnames,3),function(i)paste("$",i,"$"))),las=2,pos=2,cex=.8)
#    text(y=0,x=xs+.5,label=c("0",0.005,0.015,0.005,0.015))
#    text(y=0,x=xs[1],label="f:")
#    text(y=max(ylims)+1/yadj,x=tot_traits*xadj+xs[1]+.5,label=expression(beta~":"))
#    text(y=max(ylims)+1/yadj,x=tot_traits*xadj+xs[c(2,4)]+1,label=c(-10,0))
#    mtext(main,3,-1,outer=T)
#
#}


joyplotA <- function(listdensities,pathwaysnames=pathwaysnames,main="",angle=25,yadj=2,sep_ba=c(0,.4),alpha=0.8,baselines=F,miniaxes=F){


    tot_traits=length(listdensities[[1]][[1]])
    sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
    names(sexcols)=c(0,.5,1)
    allcols=unlist(lapply(sexcols,rep,tot_traits/length(sexcols)))
    par(mar=c(0,0,0,0),oma=c(1,0,2,1),xpd=NA)
    

    xadj=ifelse(angle==0,0,1/angle)
    ylims=c(0,(tot_traits+1)/yadj)
    xlims=c(0,.7+sum(sep_ba)+(1+tot_traits)*xadj+5*(1.3+xadj))
    plot(1,1,type="n",xlim=xlims,ylim=ylims,axes=F,ann=F)
	par(xpd=F)
    if(baselines){
# abline(h=(1:tot_traits)/yadj,lwd=.4,lty=2)
    segments(x0=rep(0,tot_traits)+1:tot_traits*xadj,y0=(1:tot_traits)/yadj,y1=(1:tot_traits)/yadj,x1=(1+0.7+xadj)*5+max(sep_ba)+(xadj*(1:tot_traits)-0.4),lwd=.4,lty=2)
}
	par(xpd=NA)
    xs=c()
    xs=c(xs,.7+xadj+listdensities[[1]][[1]][[1]][1,1])
    for(tr in length(listdensities[[1]][[1]]):1){
        #First column, f0
        polygon(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1],tr/yadj+listdensities[[1]][[1]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
        if(miniaxes){
            na=range(.7+tr*xadj+listdensities[[1]][[1]][[tr]][,1])
            arrows(x0=na[1],y0=tr/yadj,x1=(na[1]+na[2])/2,y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
            arrows(x0=(na[1]+na[2])/2,y0=tr/yadj,x1=na[2],y1=tr/yadj,lwd=.5,angle=90,code=3,length=.02)
            text(c(0,0.5,1),x=c(na[1],(na[2]+na[1])/2,na[2]),y=tr/yadj+c(0,0,0)+0.1,cex=.4,pos=1)
        }
    }
    for(ba in 1:length(listdensities)){
        for(bo in 2:length(listdensities[[ba]])){
            xs=c(xs,1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.6+tr*xadj+listdensities[[ba]][[bo]][[1]][1,1])
            for(tr in length(listdensities[[ba]][[bo]]):1){
                polygon(1.5+(ba-1)*2*1.5+sep_ba[ba]+(bo-1)*1.5+tr*xadj+listdensities[[ba]][[bo]][[tr]][,1],tr/yadj+listdensities[[ba]][[bo]][[tr]][,2],col=adjustcolor(allcols[tr],alpha=alpha),lwd=.3)
            }
        }
    }
    text(x=rep(0,tot_traits)+1:tot_traits*xadj,y=(1:tot_traits)/yadj,label=TeX(lapply(rep(pathwaysnames,3),function(i)paste("$",i,"$"))),las=2,pos=2,cex=.8)
    text(y=0,x=xs+.5,label=c("f: 0 ",0,0.015,0,0.015))
    text(y=-.2,x=xs[1]+.5,label=c("(two types, beta=-10)"),cex=.7)
    #text(y=0,x=xs[1],label="f:")
    text(y=max(ylims)+1/yadj,x=tot_traits*xadj+xs[1]+.5,label=expression(beta~":"))
    text(y=max(ylims)+1/yadj,x=tot_traits*xadj+xs[c(2,4)]+1,label=c(-10,0))
    mtext(main,3,-1,outer=T)

}
