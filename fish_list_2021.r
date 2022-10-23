

load("fish_full_data.rdata")


one_observer<-list()
loop<-1
set.seed(1)
for (i in unique(fish_full_data$survey_id)) {
  
  data <- fish_full_data %>% filter(survey_id == i)
  
  obs_amount <- length(unique(data$Observer))
  obs_name <- unique(data$Observer)
  chosen_obs <- sample(obs_name,1)  
  
  filter_data<-data %>% filter(Observer == chosen_obs)
  one_observer[[loop]]<-filter_data
  loop<-loop+1
  
  
}

one_observer<- bind_rows(one_observer)

rm(data,filter_data)



one_observer$Confidence[is.na(one_observer$Confidence)]<-(0)
one_observer<-one_observer %>% filter(Confidence <2)

one_observer$year_month<-ifelse(month(one_observer$Date)>6,
                                paste(one_observer$Year,"b",sep= " "),
                                paste(one_observer$Year,"a",sep= " "))



fish_abundance_list<-one_observer %>% 
  filter(year_month =="2021 a") %>% 
  group_by(Species,Family) %>% summarise("abundance" = sum(Abundance))

write.csv(fish_abundance_list,file = "fish_species_list_2021.csv")
