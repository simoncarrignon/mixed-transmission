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


usethis::use_data(fullpathways)
