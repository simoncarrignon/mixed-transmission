


devtools::load_all(".")
population.ages=0:101
which(getAgeBand(age.ref=10,age.peers = population,threshold=1,pos="h")) == c(9,10,11)

sapply(1:100,function(i) population.ages[getAgeBand(age.ref=i,age.peers = population.ages,threshold=1,pos="h")] == (i-1):(i+1))

sapply(2:99,function(i) population.ages[getAgeBand(age.ref=i,age.peers = population.ages,threshold=2,pos="h")] == (i-2):(i+2))
       
sapply(1:100,function(i)population.ages[getAgeBand(age.ref=i,age.peers = population.ages,threshold=2,pos="o")] == population.ages[population.ages>=(i+2)])

sapply(1:99,function(i)population.ages[getAgeBand(age.ref=i,age.peers = population.ages,threshold=2,pos="o")] == population.ages[population.ages>=(i+2)])



unlist(replicate(100,{threshold=sample.int(200,1);sapply(1:99,function(i)population.ages[getAgeBand(age.ref=i,age.peers = population.ages,threshold=threshold,pos="o")] == population.ages[population.ages>=(i+threshold)])}))

replicate(100,
          {
              nref=sample.int(100,1)
              ar=sample.int(100,nref,replace=T)
              pos=sample(c("o","h","v","i"),1)
              getAgeBand(age.ref=ar,age.peers = population.ages,threshold=2,pos="o")
              replicate(100,length(getAgeBand(age.ref=c(),age.peers = population.ages,threshold=sample(1000),pos=pos))==0)
              replicate(100,length(getAgeBand(age.ref=c(),age.peers = c(),threshold=sample(1000),pos=pos))==0)
              replicate(100,length(getAgeBand(age.ref=sample(1000,1),age.peers = c(),threshold=sample(1000,1),pos=pos))==0)
              replicate(100,length(getAgeBand(age.ref=sample(1000,1),age.peers = c(),threshold=sample(1000,1),pos=pos))==0)
          })

population.ages=unlist(sapply(1:100,function(i)rep(i,3)))
population.ages=1:100
population.ages=sort(rep(population.ages,3))
replicate(100,getAgeBand(age.ref=sample(1000,10),age.peers = population.ages,threshold=10,pos="o"))
replicate(1000,{
              size.ref=sample(2:100,1)
              dim(getAgeBand(age.ref=sample(1000,size.ref,replace=T),age.peers = population.ages,threshold=10,pos="o"))==c(length(population.ages),size.ref)
          })

all(replicate(1000,{
              size.ref=sample(2:100,1)
              population.ages=50:100
              dim(getAgeBand(age.ref=sample(49,size.ref,replace=T),age.peers = population.ages,threshold=10,pos="o"))==c(length(population.ages),size.ref)
          }))

size.ref=sample(2:100,1)
population.ages=61:100
all(replicate(100,getAgeBand(age.ref=sample(49,size.ref,replace=T),age.peers = population.ages,threshold=10,pos="o")))

population.ages=1:60
replicate(100,
                {
                    size.ref=sample(2:100,1)
                    all(!getAgeBand(age.ref=sample(70:100,size.ref,replace=T),age.peers = population.ages,threshold=10,pos="o"))
                }
)

population.ages=1:60
replicate(100,
                {
                    size.ref=sample(1:100,1)
                    dim(getAgeBand(age.ref=sample(70:100,size.ref,replace=T),age.peers = population.ages,threshold=10,pos="h"))
                }
)
