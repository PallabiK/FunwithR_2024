library(tidyverse)
library(reshape2)
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
  xlab("Languages (2024)") + theme_bw()+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 0.5, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))


##art
art<-read_csv(file = "art_2024.csv")

#time taken for art
art1<-art %>% group_by(type) %>% select(id, type, time_taken) %>% 
  get_summary_stats(time_taken, type = "mean_sd")

ggplot(art1)+
  geom_bar(aes(x=type, y=n), stat="identity",
           fill=c("#E222FF", "#26D345", "#EEE71B","#66D4DC"), color='black', alpha=0.75) +
  scale_y_continuous(breaks = seq(0, 50, by=10), limits=c(0,50))+
  theme_bw() + ylab("No. of projects") + 
  xlab("Type of art (2024)") +
  scale_x_discrete(labels = c("Origami", "Pen and\nPaper", "Thread", "Crochet")) +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

ggplot(art1)+
  geom_bar( aes(x=type, y=mean), stat="identity",
            fill=c("#E222FF", "#26D345", "#EEE71B","#66D4DC"), color='black', alpha=0.75) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-50, 320, by=50), limits=c(-50,320))+
  theme_bw() + ylab("Average no. of days\nto make one project") + 
  xlab("Type of art (2024)") +
  scale_x_discrete(labels = c("Origami", "Pen and\nPaper", "Thread", "Crochet")) +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

#removing pen and paper
art2<- art1 %>% filter(mean<50)
ggplot(art2)+
  geom_bar( aes(x=type, y=mean), stat="identity",
            fill=c("#E222FF", "#EEE71B","#66D4DC"), color='black', alpha=0.75) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-5, 25, by=10), limits=c(-5, 25))+
  theme_bw() + ylab("Average no. of days\nto make one project") + 
  xlab("Type of art (2024)") +
  scale_x_discrete(labels = c("Origami", "Thread", "Crochet")) +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

#books
books<-read_csv(file="books_2024.csv")

#books by fiction/non-fiction and days
book1<-books %>% group_by(type) %>% get_summary_stats(time_taken, type = "mean_sd")

ggplot(book1) +
  geom_bar(aes(x=type, y=n), stat="identity",
           fill=c("#AAAFFF", "#DDD111"), color='black', alpha=1) +
  scale_y_continuous(breaks = seq(0, 15, by=5), limits=c(0,15))+
  theme_bw() + ylab("No. of books") + 
  xlab("Type of books (2024)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

ggplot(book1)+
  geom_bar( aes(x=type, y=mean), stat="identity",
            fill=c("#AAAFFF", "#DDD111"), color='black', alpha=1) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(0, 40, by=10), limits=c(0, 40))+
  theme_bw() + ylab("Average no. of days\nto read 1 book") + 
  xlab("Type of books (2024)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

#books by fiction/non-fiction and pages
book2<- books %>%  group_by(type) %>% get_summary_stats(pagespday, type = "mean_sd")

ggplot(book2)+
  geom_bar( aes(x=type, y=mean), stat="identity",
            fill=c("#AAAFFF", "#DDD111"), color='black', alpha=1) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(0, 65, by=10), limits=c(0, 65))+
  theme_bw() + ylab("Average no. of pages\nread per day") + 
  xlab("Type of books (2024)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

#number of books vs publisher
book3<- books %>% group_by(publisher) %>% get_summary_stats(time_taken, type="mean_sd")

ggplot(book3) +
  geom_bar(aes(x=publisher, y=n), stat="identity",
           fill=c("#f43810", "#f4a110", "#f4f410", "#90f410",
                  "#10f4af", "#105cf4", "#4410f4", "#9310f4"), color='black', alpha=1) +
  scale_y_continuous(breaks = seq(0, 4, by=1), limits=c(0,4))+
  theme_bw() + ylab("No. of Books") + 
  xlab("Publishers read in 2024") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 45, vjust = 1, hjust=1),
        axis.text.y=element_text(face="bold", color='black'))

#total number of pages by fiction/non-fiction
book4<-books %>% group_by(type) %>% get_summary_stats(pages, type="mean_sd")

ggplot(book4)+
  geom_bar( aes(x=type, y=mean), stat="identity",
            fill=c("#AAAFFF", "#DDD111"), color='black', alpha=1) +
  geom_errorbar( aes(x=type, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(0, 800, by=100), limits=c(0, 800))+
  theme_bw() + ylab("No. of pages") + 
  xlab("Type of books (2024)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 0, vjust = 1, hjust=0.5),
        axis.text.y=element_text(face="bold", color='black'))

#yearly comparisons of activities
num_activities <- read_csv("number_of_activities.csv")
num_activities1 <- melt(num_activities[c('activities','2023','2024')],id.vars = 1)
num_activities2<-num_activities1 %>% group_by(activities)
num_activities2<-num_activities2 %>% rename("Years"="variable")

ggplot(num_activities2)+
  geom_bar(aes(x=activities, y=value, fill=Years), stat="identity", position="dodge")+
  scale_y_continuous(breaks = seq(0, 110, by=10), limits=c(0, 110))+
  scale_fill_manual(values = c("#e0b710","#0dafc6"))+
  theme_bw() + ylab("No. of times\nactivity done") + 
  xlab("Activities (2024)")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 45, vjust = 1, hjust=1),
        axis.text.y=element_text(face="bold", color='black'),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

#yearly comparison of number of days per activity
days_activity <- read_csv("days_spent_per_activity.csv")
days_activity <- days_activity %>% rename("2023"="days_spent_2023", "2024"="days_spent_2024")
day_activity1 <- melt(days_activity[c('activities', '2023', '2024')], id.vars = 1)
day_activity2 <- day_activity1 %>% group_by(activities)
day_activity2 <- day_activity2 %>% rename("Years"="variable")

ggplot(day_activity2)+
  geom_bar(aes(x=activities, y=value, fill=Years), stat="identity", position="dodge")+
  scale_y_continuous(breaks = seq(0, 310, by=30), limits=c(0, 310))+
  scale_fill_manual(values = c("#e0b710","#0dafc6"))+
  theme_bw() + ylab("No. of days") + 
  xlab("Activities (2024)")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black', angle = 45, vjust = 1, hjust=1),
        axis.text.y=element_text(face="bold", color='black'),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))
