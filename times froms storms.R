
survey_dates<- fish_full_data %>%
  distinct(Year,.keep_all = T) %>%
  select(Date) 


survey_dates[nrow(survey_dates) + 1, ] <- "2020-10-13" 
survey_dates<-survey_dates  %>% 
  mutate(time_form_storm =interval(ymd("2020-03-13"), survey_dates$Date) %/% months(1))

   

survey_dates$diff_month <- diff(survey_dates$Date,survey_dates$storm_date,unit = "m")

2020-10-13
