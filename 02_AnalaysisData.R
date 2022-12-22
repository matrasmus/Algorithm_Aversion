library(tidyverse)
library(tidytext)
library(dplyr)
library(vader)
library(academictwitteR)
library(data.table)
library(readr)


  

#load data here
#alg.data <- read.csv("...\\algtweetsalgData.csv")


alg.data <- alg.data[, c  ("index" ,"text", "id" ,"created_at", "VADER",
                           "VADERclass", "Business" ,  "Social.Media", 
                           "Technology" ,  "Immutability" ,"Influence", 
                           "Application",   "Aversion",
                           "Year") ]

  




#-------------completed----------
alg.data %>% group_by(topic, Year, VADERclass) %>% summarise(Sent = n()) -> summ.data
alg.data %>% group_by(Year, VADERclass) %>% summarise(Sent = n()) -> summ.year

listofdfs <- list()

#-------------------Sample Validation------------------


sample.test <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(sample.test) <- c("text", "topic")

for (i in c(7:13)){
  tmp <- subset(alg.data, index %in% sample(filter(alg.data, alg.data[,i] >= 1)$index, size = 20))
  sample.test <- rbind(sample.test, tmp[c("text", "topic")])
}

sample.test




#data group for topic oriented dataframe

listofdfs <- list()

for (i in c(7:13)){
  alg.data %>% 
    filter(alg.data[,i]>=1)%>% 
    mutate(Topic = colnames(alg.data)[i])%>% 
    group_by( VADERclass, Topic) %>% 
    summarise(Sent = n()) ->listofdfs[[i]]
}
summ.topic <- bind_rows(listofdfs)
summ.topic

#adds percentage to cummulated DF
summ.cumm <- 
  summ.cumm %>% 
  group_by(Topic, Year) %>% 
  mutate(All = sum(Sent),percent=(100*Sent/All))


#adds percentage to yearwise DF
summ.year<- 
  summ.year %>% 
  group_by(Year) %>% 
  mutate(All = sum(Sent),percent=(100*Sent/All))

#adds percentage to topicwise DF
summ.topic<- 
  summ.topic %>% 
  group_by(Topic) %>% 
  mutate(All = sum(Sent),percent=(100*Sent/All))

summ.cumm
summ.topic
summ.year

#--------------Test wordgroups---------------------
listofdfs <- list()


for (i in c(7:13)){
  alg.data %>% 
    filter(alg.data[,i]>=1)%>% 
    mutate(Topic = colnames(alg.data)[i])%>% 
    sample_n(size = 20)%>% 
  group_by( VADERclass, Topic)  ->listofdfs[[i]]
}
Wordgrouptest<- bind_rows(listofdfs)




#--------------Bootstraping---------------------
listofdfs <- list()


for (i in c(7:13)){
  alg.data %>% 
  filter(alg.data[,i]>=1)%>% 
  mutate(Topic = colnames(alg.data)[i])%>% 
    sample_n(size = 100)%>% 
    group_by( VADERclass, Topic) ->listofdfs[[i]]
}
bootstrap<- bind_rows(listofdfs)
bootstrap %>% select(VADERclass,Year,Topic) -> bootstrap





#-------------------Plotting------------------


#---------------------Year--------------------

plot(summ.year$Year, summ.year$percent, main ="Yearwise",xlab = 'Analyzed year', ylab = 'Percent',cex.lab=2, cex.axis=1.5, cex.main=3, cex.sub=2)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Positive'], col = "green", lwd=4)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Neutral'], col = "black", lwd=4)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Negative'], col = "blue", lwd=4)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Aversive'], col = "red",lwd=5)
legend("topright", legend=c("positive", "neutral", "negative", "aversive"), col=c("green","black", "blue", "red"), lty=1:1,lwd=4, cex=1.7)


ggplot(data=summ.year, 
       aes(x=Year, y=percent, fill=VADERclass)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")


ggplot(data=summ.year, 
       aes(x=Year, y=percent, fill=VADERclass)) + 
  geom_bar(stat="identity",  colour="black")



ggplot(data=summ.year, aes(x=VADERclass, y=Sent, color=VADERclass)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + stat_summary(fun.y=mean, geom="point", shape=23, size=4)



#-----------------Topic-----------------------
topics = c('Business','Social.Media','Technology','Immutability','Influence','Application','Aversion')

for (i in 1:length(topics)){
  
  summ.cumm %>% 
    filter(Topic==topics[i]) %>%
    with(plot(Year, Sent, main =topics[i],xlab = 'Analyzed ear', ylab = 'Tweets'))
  summ.cumm %>% 
    filter(Topic==topics[i]) %>% 
    with(lines(unique(Year), Sent[VADERclass=='Positive'], col = 3) )
  summ.cumm %>% 
    filter(Topic==topics[i]) %>%
    with(lines(unique(Year), Sent[VADERclass=='Neutral'], col = 1))
  summ.cumm %>% 
    filter(Topic==topics[i]) %>%
    with(lines(unique(Year), Sent[VADERclass=='Negative'], col = 2))
  summ.cumm %>% 
    filter(Topic==topics[i]) %>%
    with(lines(unique(Year), Sent[VADERclass=='Aversive'], col = 4))
  summ.cumm %>% 
    filter(Topic==topics[i]) %>%
    with(legend("topright", legend=c("positive", "neutral", "negative", "aversive"), col=c("black","green", "red"), lty=1:1, cex=0.8)) 
}

#----------------BarPlot for topics-----

ggplot(data=summ.topic, 
       aes(x=Topic, y=percent, fill=VADERclass)) + 
  ggtitle("Topics 2010-2021") +
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  theme(axis.text=element_text(size=14),plot.title = element_text(color="Black", size=15, face="bold", hjust = 0.5),
        legend.text=element_text(size=12),
  axis.title=element_text(size=14,face="bold"))






summ.year %>%
  filter(VADERclass=="Aversive")%>%
  with(hist(x=Year,y=Sent))


summ.year %>%
  filter(VADERclass=="Aversive")%>%
  with(shapiro.test(Sent))
  with(hist(Sent))

summ.year %>%
  filter(VADERclass=="Aversive")%>%
  with(t.test(Sent, mu= 128, alternative = "greater"))

