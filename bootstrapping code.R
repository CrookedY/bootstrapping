library(tidyverse)
std <- function(x) sd(x)/sqrt(length(x))

bootstrappingdata <- read_csv("~/plots for thesis/bootstrappingdata.csv", 
                              col_types = cols(conditions = col_factor(levels = c("HC-HL", 
                                                                                  "HC-LL", "LC-HL", "LC-LL"))))
HLHC<-filter(bootstrappingdata, conditions == "HC-HL")
HLLC<-filter(bootstrappingdata, conditions == "LC-HL")


samplethis<-c(5, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30)

samplelist <- HLLC$SD


bootstrapit <- function(sampling, samplelist){
  x = NULL
  y=NULL
  z=NULL
  for (i in sampling) {
    
    x = append(x, i)
    
    Torandom<-sample(samplelist, i)
    y = append (y, mean(Torandom))
    z = append (z,std(Torandom))
    
  }
  d = data.frame(x,y,z)
  return (d)
}

results1<-bootstrapit(samplethis, samplelist)

ggplot(results1, aes(x=x, y=y))+
  geom_line()+
  geom_errorbar(aes(ymin=y-z, ymax=y+z), width=.1)+
  xlab("sample number") + ylab("Mean Stomatal Density (per mm2) per permutation")+
  theme_classic()

permutations <- function(samplethis, samplelist){

a=0
results = NULL

while (a<100) {
  a=a+1
  tempdata = (bootstrapit(samplethis, samplelist))
  results = rbind(results,tempdata)

}
return (results)
}

results2<-permutations(samplethis, samplelist)

ggplot(results2, aes(x=x, y=y))+
  geom_boxplot(aes(group=x))+
  xlab("sample number") + ylab("Mean Stomatal Density (per mm2) per permutation")+
  theme_classic()

