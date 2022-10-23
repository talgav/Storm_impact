
library(plotrix)
library(tidyverse)

load("fish_full_data.rdata")
load("coral_comm.rdata")
load("coral_cover.rdata")
breakage <- read.csv("data\\breakage 2020 - 2018.csv")


save_path<-"G:\\My Drive\\PHD\\chapter 1 strom effect\\R Scripts\\plots for ms\\knoll_stat"

# depth

mean(coral_cover$Max_depth)
std.error(coral_cover$Max_depth)
var(coral_cover$Max_depth)
sd(coral_cover$Max_depth)

# depth plots

ggplot(data = coral_cover, aes(x= Site, y = Max_depth))+
  geom_violin(fill = "#F1DCB8")+ 
  stat_summary(fun.y=mean, geom="point",size=2)+
  theme_classic()+ylab("Bottom depth (m)")+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size =16),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        plot.title = element_text(size=24))

ggsave("depth_violin.pdf", width = 10, height = 5, dpi = 300,path = save_path)
ggsave("depth_violin.png", width = 10, height = 5, dpi = 300,path = save_path)


# surface area

mean(coral_cover$Surface_Area)
std.error(coral_cover$Surface_Area)
var(coral_cover$Surface_Area)
sd(coral_cover$Surface_Area)

# surface area plot

ggplot(data = coral_cover, aes(x= Site, y = Surface_Area))+
  geom_violin(fill = "#F1DCB8")+ 
  stat_summary(fun.y=mean, geom="point",size=2)+
  theme_classic()+ylab(bquote('Surface area '~(m^2)))+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size =16),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        plot.title = element_text(size=24))

ggsave("surface_violin.pdf", width = 10, height = 5, dpi = 300,path = save_path)
ggsave("surface_violin.png", width = 10, height = 5, dpi = 300,path = save_path)

# STONY CORAL COVER

mean(coral_cover$Stony.Coral,na.rm =T)
std.error(coral_cover$Stony.Coral,na.rm =T)
var(coral_cover$Stony.Coral,na.rm =T)
sd(coral_cover$Stony.Coral,na.rm =T)


coral_cover %>%
  group_by(storm) %>%
  summarise("mean" = mean(Stony.Coral,na.rm =T),
            "sd" =sd(Stony.Coral,na.rm =T),
            "SE" = std.error(Stony.Coral,na.rm =T),
            "min"= min(Stony.Coral,na.rm =T),
            "max"= max(Stony.Coral,na.rm =T))


coral_cover$storm<-factor(coral_cover$storm,levels = c("Before","After"))

#violin plot

ggplot(data = coral_cover, aes(x= Site, y = Stony.Coral,fill = storm))+
  geom_violin()+ 
  stat_summary(fun.y=mean, geom="point",size=2,position = position_dodge(width = 0.9))+
  theme_classic()+ylab("Hard coral cover (%)")+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size =16),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        plot.title = element_text(size=24))+
  scale_fill_manual(values=c("#9DD5EA","#E84B40"))

#box plot

ggplot(data = coral_cover, aes(x= Site, y = Stony.Coral,fill = storm))+
  geom_boxplot()+ 
  theme_classic()+ylab("Hard coral cover (%)")+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size =16),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        plot.title = element_text(size=24))+
  scale_fill_manual(name = "Storm",values=c("#70B7D9","#A5C99A"))


ggsave("coral_cover.pdf", width = 10, height = 5, dpi = 300,path = save_path)
ggsave("coral_cover.png", width = 10, height = 5, dpi = 300,path = save_path)


# braekage %




breakage<-breakage %>%
  select(1:5) %>% 
  mutate("precentage"=Broken*100/Total) %>% 
  drop_na("precentage")

breakage$Storm<-ifelse(breakage$Year == "2020","After","Before")
breakage$Storm<-factor(breakage$Storm, levels = c("Before","After"))


ggplot(data = breakage,aes(x = Site,y=precentage,fill = Storm))+
  geom_boxplot()+
  theme_classic()+ylab("Broken coral colonies (%)")+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size =16),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        plot.title = element_text(size=24))+
  scale_fill_manual(values=c("#70B7D9","#A5C99A"))

ggsave("breakege_boxplot.pdf", width = 10, height = 5, dpi = 300,path = save_path)
ggsave("breakege_boxplot.png", width = 10, height = 5, dpi = 300,path = save_path)


fish_full_data %>% filter(year_month == c("2020 a","2018 b")) %>% distinct(Observer)
fish_full_data %>% filter(year_month == "2020 a") %>% distinct(Observer)

sp<- fish_full_data %>% distinct(Species,.keep_all = T) %>% select(Family,Species,Species_2015,Status)
