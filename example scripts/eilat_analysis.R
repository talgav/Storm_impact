library("dplyr")
library("reshape")
library("rfishbase")
library("magrittr")
library("stringr")
library("ggplot2")
library("lme4")
library("arsenal")

eilat_data <- read.csv("eilat_raw.csv")
eilat_species <- data.frame("name"=as.character(unique(eilat_data$Species)))
eilat_species$name <- as.character(eilat_species$name)

uppercasefirstletter <- function(x){
  a <- toupper(substr(as.character(x),1,1))
  b <- substr(as.character(x),2,nchar(as.character(x)))
  return(paste(a,b,sep=""))
}

eliminatelastdot <- function(x){
  if (substr(x,nchar(x)-3,nchar(x)) == "spp.")
    return(x)
  else{
     if (substr(x,nchar(x),nchar(x)) == "."){
    nodot <- eliminatelastdot(substr(x,1,nchar(x)-1))
    return(nodot)
  }
  else 
    return(x) 
  }
}


eliminatelastspace <- function(x){
    if (substr(x,nchar(x),nchar(x)) == " "){
      nodot <- eliminatelastspace(substr(x,1,nchar(x)-1))
      return(nodot)
    }
    else 
      return(x) 
}

eilat_species$name <- sapply(eilat_species[1:nrow(eilat_species),], uppercasefirstletter)

eilat_grouped <- eilat_data %>% 
  group_by(Location,Trans_No,Length) %>% 
  summary("n"=n())

output <- sapply(eilat_species[1:nrow(eilat_species),],function(x) tryCatch(length_weight(x,fields=c("a","b"))[,3:4],error=function(e) NULL))

meantwocolumns<- function(x){
  return(c(mean(x[,1]),mean(x[,2])))
} 

#write.csv(t(data.frame(sapply(output,meantwocolumns))),"ab_eilat.csv")
ab_data <- read.csv("ab_eilat_complete.csv")
colnames(ab_data) <- c("Species","a","b","method")
eilat_data$Species <- sapply(eilat_data$Species, uppercasefirstletter)
eilat_data$Species <-  sapply(eilat_data$Species, eliminatelastspace)
ab_data$Species <- sapply(as.character(ab_data[,1]), function(x) ifelse((substr(x,nchar(x)-3,nchar(x)) == "spp."),sub("[.]"," ",x),
                                                          gsub("\\.", " ", x)  ))
ab_data$Species <- sapply(ab_data$Species, eliminatelastspace)
ab_data[ab_data$Species == "X siphamia majimai",]$Species <- "Siphamia majimai"
eilat_data[eilat_data$Species == " siphamia majimai",]$Species <- "Siphamia majimai"
eilat_data <-  left_join(eilat_data,ab_data,by = "Species")


#####NOW WE CAN START
working_data <- eilat_data
working_data$Weight <- with(working_data,a*Length^b)
working_data$depth.g <- with(working_data,ifelse(Depth.m.<14,"shallow","deep"))
working_data[which(working_data$SiteNo == "Katza"),]$SiteNo <- "KATZA"
working_data[which(working_data$SiteNo == "Japanese gardens"),]$SiteNo <- "Japanese Gardens"
working_data <- subset(working_data,SiteNo != "mistake")
working_data$SiteNo %<>% factor() 
working_data$Trans_Location <- ifelse(substr(working_data$Trans_No,nchar(as.character(working_data$Trans_No)),nchar(as.character(working_data$Trans_No))) == "C",
                                      "C","T")

write.csv(working_data,"working_data_inital_cleanup.csv")

####get size class####
#we will round a log with base 2 so, for example,   #
#a fish that weighs 9g will be class 4. log2 of 9 is#
# 3.17 so we will round up to 4 using ceiling func. #

working_data$Class <- with(working_data,(ceiling(log(Weight,2))))

working_data$Total.Weight <- with(working_data, Number_Ind * Weight)
wd_no_nb <- subset(working_data,SiteNo != "North Beach")

sumdata <- working_data%>%
  group_by(SiteNo,Trans_Location,Direction,Class)%>%
  summarize("Sum.Biomass" = sum(Total.Weight))%>%
  arrange(SiteNo,Trans_Location)

sumdata$midpoint <- (2^sumdata$Class+2^(sumdata$Class-1))/2
sumdata$Width <- (2^((sumdata$Class)))-((2^(sumdata$Class-1)))
sumdata$Corrected.Sum.B <- with(data = sumdata,expr = Sum.Biomass/Width)
sumdata$logtransformed <- log10(sumdata$midpoint)

#sumdata$Trans_Location <- ifelse(substr(sumdata$Trans_No,nchar(sumdata$Trans_No),nchar(sumdata$Trans_No)) == "C",
#                                  "C","T")
#sumdata$TransNo <- substr(sumdata$Trans_No,1,nchar(sumdata$Trans_No)-1)


theme <- theme_bw()+
  theme(axis.line = element_line(colour = "black",size=0.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks.length=unit(-0.2,"cm"),
        legend.position = "none",
        strip.text = element_text(size=25),
        axis.text.x = element_text(margin=unit(c(0.5,0.2,0.2,0.2), "cm"),size = 15),
        axis.text.y = element_text(margin=unit(c(0.2,0.5,0.2,0.2), "cm"),size = 15),
        axis.title= element_text(size = 30))

ggplot(working_data_mle)+
  aes(x=(Length),fill = Trans_Location)+scale_y_log10()+
  geom_histogram()+
  theme+
  facet_wrap(~Trans_Location)

ggplot(subset(sumdata,logtransformed> 1.1 | Trans_Location == "C"))+
  aes(x=logtransformed,y=log10(Corrected.Sum.B),color=Trans_Location)+
  #geom_point()+
  stat_summary(fun.y = mean,geom = "point")+
  geom_smooth(method="lm",se=F)+
  xlab(label = "Log10 Midpoint Size Class")+ylab("Log10 Biomass")+
  theme

summary(lm(data =(subset(sumdata,logtransformed > 1.1 & Trans_Location == "T")), formula = log10(Corrected.Sum.B) ~ logtransformed ))

ggplot(subset(sumdata,SiteNo == "North Beach" & (logtransformed> 1.1 & Trans_Location == "T")))+
  aes(x=logtransformed,y=log10(Corrected.Sum.B))+
  #geom_point()+
  stat_summary(fun.y = mean,geom = "point")+
  geom_smooth(method="lm",se=F)+
  xlab(label = "Log10 Midpoint Size Class")+ylab("Log10 Biomass")+
  theme

summary(lm(data =subset(sumdata,SiteNo == "North Beach" & (logtransformed> 1.1 & Trans_Location == "T")), formula = log10(Corrected.Sum.B) ~ logtransformed ))
