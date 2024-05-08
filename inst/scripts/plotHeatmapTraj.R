sexcols=c('#EE5A45', '#D4D6B9','#1E8F89')
names(sexcols)=c(0,.5,1)
allsexcols=unlist(lapply(sexcols,rep,5))
traits=1:15
mat=matrix(traits,ncol=3,nrow=5)
traits=as.vector(t(mat))
tmax=250

uu=readRDS("alldamnedres.RDS")
for(bonus in c(1,2,3)){
    for(bs in c(-10,0)){
        beta=as.character(bs)
        for(logfreq in c(0,1)){
            png(paste0("distribCommunitiesThroughtTime",ifelse(logfreq==0,"","logfreq"),"_bonus",bonus,"_beta",beta,".png"),width=1000,height=1600,pointsize=22,type="cairo")
            par(mfrow=c(5,6),mar=c(2,0,0,1),oma=c(4,4,6,4))
            for(trait in traits){
                #    for(bonus in as.character(c(0,1,3))){
                for(type in 1:2){
                    datas=uu[[beta]][[bonus]][,type][[trait]][,tmax:1]
                    if(logfreq)datas=apply(datas,2,function(i){new=log((i+1)/sum(i+1));new-log(1/sum(i+1))})
                    else datas=apply(datas,2,function(i){i/sum(i)})
                    #datas=apply(datas,2,function(i)(log(i+1)))
                    image(datas,ylab=paste0("c",trait-10),xaxt="n",main=paste("Type",as.roman(type)),col=colorRampPalette(c("white",allsexcols[trait],"black"))(100)[1:70],yaxt="n")
                    if(trait %in% traits[1:3]) mtext(paste("type",as.roman(type)),3,1)
                    if(type == 1 & trait %in% c(1:5)) mtext(bquote(c[.(trait)]),cex=1.4,2,2)

                    if(type == 2 & trait %in% c(11:15)){
                        axis(4,labels=round(seq(tmax,1,length.out=5)),at=seq(0,1,length.out=5))
                        mtext("time step",4,3)
                    }
                    if( trait %in% c(5,10,15)){
                        axis(1)
                        mtext("c_i=1",1,3)
                    }

                }
            }
            #}
            mtext(bquote(f*":"*.(bonus*0.005)*","*beta*":"*.(beta)*.(ifelse(logfreq==0,"",",log prop."))),3,3,outer=T,cex=2)
            dev.off()
        }
    }
}



beta=as.character(-10)
type=2
tmax=250
bonus=3
traits=11:15

for(trait in 11:15){
    datas=uu[[beta]][[bonus]][,type][[trait]][,tmax:1]
    if(logfreq)datas=apply(datas,2,function(i){new=log((i+1)/sum(i+1));new-log(1/sum(i+1))})
    if(!logfreq) datas=apply(datas,2,function(i){i/sum(i)})

    volcano=datas
    ggvolcano = volcano %>% 
        reshape2::melt() %>%
        ggplot() +
        geom_tile(aes(x=Var1,y=Var2,fill=value)) +
        scale_x_continuous("%",expand = c(0,0),label=c(0,.5,1),breaks=seq(1,nrow(volcano),length.out=3)) +
        scale_y_continuous("time",expand = c(0,0),breaks=seq(0,250,length.out=6),labels=rev(pretty(seq(250,1,length.out=6)))) +
        scale_fill_gradientn("Z",colours = colorRampPalette(c("white",allsexcols[trait],"black"))(100)[1:70]) +
        coord_fixed() + 
        labs(title = paste0("c", trait - 10)) +  # Update the title here
        theme(legend.position = "none")
        ggvolcano


        plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 2, height = 7, 
                scale = 110, windowsize = c(800, 1200), zoom = 0.6, phi = 45, theta = -20)
        render_snapshot(filename=paste0("typeIIpathway_pbias1_beta-10_bonus3","_c", trait - 10))
        rgl::clear3d()
}
