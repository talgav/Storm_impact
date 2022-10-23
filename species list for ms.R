
# species list 2022 - for ms

load("fish_full_data.rdata")


species_list<- fish_full_data %>%
  distinct(Species,.keep_all = T) %>%
  select(Family,Species,Species_2015,Status) %>% 
  arrange(Family,Species)

species_list$Status[species_list$Status == "T"]<-"Transient"
species_list$Status[species_list$Status == "C"]<-"Reef associated"

write.csv(species_list,"species_list_for_ms.csv")


# how many confidance = 0 in the data:

fish_full_data$Confidence[is.na(fish_full_data$Confidence)]<-(0)
summary.factor(fish_full_data$Confidence)

#27971*100

#2797100/28744

