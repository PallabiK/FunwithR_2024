library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stats)
library (nnet)
library(car)
library(aod)
library(ggfortify)
library(ggpubr)
library(rstatix)

setwd("~/Desktop/FunwithR_2024/")
shows<-read_csv(file = "shows_2024.csv")
#show1<-show %>% filter(!is.na(enddate)) #filter out shows that I haven't finished watching

plat<-shows %>% group_by(platform) %>% tally() #group shows by where I watched it
plat<-shows %>% group_by(platform) %>% tally() %>% filter(n>2) #group shows by where I watched it more than once

ggplot(data=plat, aes(x=platform, y=n)) + 
  geom_bar(stat="identity", position = "dodge",
                  fill=c("#15177A", "#26D345", "#8E1E32", "#22D4DC",
                         "#EEE71B", "#0A4F11", "#893490", "#8856FC", "#0045DA"), color='black') +
             scale_y_continuous(breaks = seq(0, 100, by=20), limits=c(0,100))+
             theme_bw() + ylab("# shows watched") + xlab("Platform (2024)")+
             theme(axis.title = element_text(face="bold"),
                   axis.text.x=element_text(face="bold", color='black', 
                                            angle = 35, vjust = 1, hjust=1),
                   axis.text.y=element_text(face="bold", color='black'))

#summary by platform of how many episodes/part of episodes or movies watched in a day
plat1<-shows %>% group_by(platform) %>% select(platform, unitspday) %>% 
  get_summary_stats(unitspday, type = "mean_sd")%>% filter(n>2)

ggplot(plat1)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#15177A", "#C343E3", "#26D345",
                   "#8E1E32", "#BF3C11", "#22D4DC",
                          "#EEE71B", "#0A4F11", "#893490"), color='black', alpha=0.75) +
              geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                             width=0.4, colour="black", alpha=0.9, size=0.5)+
              scale_y_continuous(breaks = seq(-1, 11, by=2), limits=c(-1,11))+
              theme_bw() + ylab("Average no. of units\nwatched per day") + 
              xlab("Platform (2024)") +
              theme(axis.title = element_text(face="bold"),
                    axis.text.x=element_text(face="bold", color='black', 
                                             angle = 35, vjust = 1, hjust=1),
                    axis.text.y=element_text(face="bold", color='black'))
            

#summary by platform of how many days it took to watch 1 movie/series
plat2<-shows %>% group_by(platform) %>% select(platform, time_taken) %>% 
  get_summary_stats(time_taken, type = "mean_sd")%>% filter(n>2)

ggplot(plat2)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#15177A", "#C343E3", "#26D345",
                   "#8E1E32", "#BF3C11", "#22D4DC",
                    "#EEE71B", "#0A4F11", "#893490"), color='black', alpha=0.75) +
              geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                             width=0.4, colour="black", alpha=0.9, size=0.5)+
              scale_y_continuous(breaks = seq(-10, 80, by=20), limits=c(-10,80))+
              theme_bw() + ylab("Average no. of days\nto watch one show") + 
              xlab("Platform (2024)") +
              theme(axis.title = element_text(face="bold"),
                    axis.text.x=element_text(face="bold", color='black',
                                             angle = 35, vjust = 1, hjust=1),
                    axis.text.y=element_text(face="bold", color='black'))

#with
plat3<- shows %>% select(platform, with, time_taken)

#shows I watched by myself
onlyp<-plat3 %>% group_by(platform) %>% filter(with=="Myself") %>% 
  get_summary_stats(time_taken, type = "mean_sd") %>% filter(n>2)

ggplot(onlyp)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#15177A", "#C343E3", "#26D345",
                   "#8E1E32", "#BF3C11", "#22D4DC",
                   "#EEE71B", "#0A4F11", "#893490"), color='black', alpha=0.75) +
  geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-10, 40, by=10), limits=c(-10,40))+
  theme_bw() + ylab("Average no. of days\nto watch one show by Pallabi") + 
  xlab("Platform (2024)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 35, vjust = 1, hjust=1),
        axis.text.y=element_text(face="bold", color='black'))

#shows per year by movie/show
#shows overall

#showsbytype
plat4<- shows %>% group_by(type) %>% tally()
ggplot(plat4)+
  geom_bar( aes(x=type, y=n), stat="identity", fill = c("red", "blue", "green", "yellow", "violet"), color='black', alpha=1) +
  scale_y_continuous(breaks = seq(0, 110, by=20), limits=c(0,110))+
  theme_bw() + ylab("No. of shows") + 
  xlab("Types (2024)") + theme_bw()+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 15, vjust = 0.5, hjust=0.6),
        axis.text.y=element_text(face="bold", color='black'))

#showsbylanguage
plat5<- shows %>% group_by(language) %>% tally()
ggplot(plat5)+
  geom_bar( aes(x=language, y=n), stat="identity", fill = c("maroon", "darkgreen", "violet"), color='black', alpha=1) +
  scale_y_continuous(breaks = seq(0, 110, by=20), limits=c(0,110))+
  theme_bw() + ylab("No. of shows") + 
  xlab("Types (2024)") + theme_bw()+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 0.5, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))


