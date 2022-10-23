
library(Rarefy)
library(adiv)

data("duneFVG") #plot/species matrix
data("duneFVG.xy") #plots geographic coordinate

data("dune")


dist_sp<-dist(duneFVG.xy$tot.xy)


ser_rarefaction<-directionalSAC(duneFVG$total,c(1:8128))



a<-list(NA,'Shannon')
names(a)<-c('comm','method')

rare_shanon<-rare_alpha(duneFVG$total,
                         method="hill",
                         q=2,
                         random=999)

rare_richness<-rare_alpha(duneFVG$total,
                        method="hill",
                        q=1,
                        random=999)


plot(rare_richness[,1],ylab="richness",xlab="Number of sampling units",type="l", ylim=range(rare_richness,na.rm=TRUE))
lines(rare_richness[,2],lty=2)
lines(rare_richness[,3],lty=2)
lines(rare_shanon[,1],col=4)
lines(rare_shanon[,2],lty=2,col=4)
lines(rare_shanon[,3],lty=2,col=4)
legend("bottomright",legend=c("Non spatially-explicit Rarefaction","Spatially-explicit Rarefaction"),lty=1,col=c(1,4))





caves<-data %>% filter(site == "Caves")

caves_richness<-Rarefy::rare_alpha(caves[5:ncol(caves)],
                          method="hill",
                          q=0,
                          random=999,
                          mean = F)

plot(caves_richness[,1],ylab="richness",xlab="Number of sampling units",type="l", ylim=range(caves_richness,na.rm=TRUE))
lines(caves_richness[,2],lty=2)
lines(caves_richness[,3],lty=2)

caves_richness$samples<-1:nrow(caves_richness)

ggplot(caves_richness,aes(x= samples,y=Rarefaction))+geom_line()








