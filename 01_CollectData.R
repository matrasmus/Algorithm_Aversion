library(tidyverse)
library(tidytext)
library(dplyr)
library(vader)
library(academictwitteR)
library(data.table)
library(readr)


#generic dataframe to store all tweets
alg <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(alg) <- c("text", "id", "created_at")



#----------------


for(x in 2010:2021){
  for (m in 1:12) {
    tmp <- get_all_tweets(
      "algorithm",
      start_tweets =paste(x,"-",m,"-01T00:00:01Z", sep = ""),
      end_tweets = paste(x,"-",m,"-",sample(2:28, 1),"T00:00:00Z", sep = ""),
      file = "algtweets",
      data_path = "data21/",
      lang="en",
      bearer_token = get_bearer(),
      n = 1000,
      bind_tweets = TRUE)
    alg <- rbind(alg, tmp[c("text", "id", "created_at")])
  }
}





#VADER sentiment analysis



algVADER <- vader_df(alg.data$text)
alg.data$VADER <- algVADER$compound

alg.data$VADER %>% length()


alg.data$VADERclass <- "Neutral"
alg.data$VADERclass[alg.data$VADER < -0.3 & alg.data$VADER > -0.7] <- "Negative"
alg.data$VADERclass[alg.data$VADER > 0.3] <- "Positive"
alg.data$VADERclass[alg.data$VADER <= - 0.7] <- "Aversive"


pattern.b <- "business|money|entrepreneur|CEO|innovation|trading|trade"
pattern.s <- "twitter|youtube|reddit|facebook|instagram|content|google"
pattern.t <- "decision|support|software|program|coding|scrum|network|engineer"
pattern.immu <- "immutable|persistent|stable|unchangeable|durable|continuity|unflexible"
pattern.infl <- "influence|effect|impact|control|changeability|changeable|controllable"
pattern.appl <- "apply|application|improvement|productive|pitch|set off"
pattern.averse <- "aversion|aversive|hate|disgusting|disgust|disfavor|dislike"


#initializing empty dataframe, that contains text for further analyzing
textdf <- data.frame(text=character(), stringsAsFactors = F)



for (i in seq(1,nrow(alg.data))){
  textdf <- rbind(textdf, tibble(text = alg.data$text[i]))
  
  clean.text <- textdf[i,1] %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) 
  
  alg.data[i, "Aversion"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.averse, word)))
  
  alg.data[i, "Business"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.b, word)))
  
  alg.data[i, "Social Media"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.s, word)))
  
  alg.data[i, "Technology"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.t, word)))
  
  alg.data[i, "Immutability"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.immu, word)))
  
  alg.data[i, "Influence"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.infl, word)))
  
  alg.data[i, "Application"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.appl, word)))
}




#Create Year column to make filter easier

for(i in 2010:2021){
  alg.data$Year[alg.data$created_at >= paste(i, "01-01", sep="-") & alg.data$created_at <= paste(i+1, "01-01", sep="-")] <- as.character(i)
  
}


#Create index
alg.data <- rowid_to_column(alg.data, "index")



