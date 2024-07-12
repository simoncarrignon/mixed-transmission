devtools::load_all()
# This R script calculates the probability of adoption with respect to beta and number of migrants
# in an incumbent community of size 100. The calculations are performed over a range of beta values
# and different numbers of migrants.
# Results are shown in Figure SM4
migrants=seq(1,50,1)
betas=seq(-10,1,length.out=20)

migrants=seq(1,10,1)
betas=seq(-2,1,length.out=20)
allprop=sapply(migrants,function(m)(sapply(betas,function(beta)conf(ks=m,beta=beta,N=70))))
#pdf("BetaProba.pdf")
par(mar=c(5,5,7,5),xpd=NA)
options(scipen=999)
filled.contour(betas, migrants, log10(allprop), color.palette=colorRampPalette(c("blue", "green", "yellow", "red")),
               main=expression(atop("Probability of adoption with respect to "*beta*" and number of migrants","for an incumbent community of size 70")),
               xlab = expression(beta), ylab = "number of migrants",key.title = title(main = "\n\n\n\n\nProbability of\nadoption", cex.main = .8),
               key.axes=axis(4,at=seq(-1,-5,-1),labels=10^seq(-1,-5,-1),cex=.8)
)
options(scipen=0)
#dev.off()
