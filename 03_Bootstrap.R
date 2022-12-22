#--------------Bootstraping---------------------
listofdfs <- list()

## Sample of 100 tweets per topic
for (i in c(7:13)){
  alg.data %>% 
    filter(alg.data[,i]>=1)%>% 
    mutate(Topic = colnames(alg.data)[i])%>% 
    mutate(Neutral = VADERclass=="Neutral")%>% 
    mutate(Positive = VADERclass=="Positive")%>% 
    mutate(Negative = VADERclass=="Negative")%>% 
    mutate(Aversive = VADERclass=="Aversive")%>% 
    sample_n(size = 100)%>% 
    group_by( VADERclass, Topic) ->listofdfs[[i]]
}
bootstrap<- bind_rows(listofdfs)
bootstrap %>% select(Year,Topic,VADER,VADERclass,Neutral,Positive,Negative,Aversive) -> bootstrap
bootstrap 


library(boot)



bootstrap  -> ObservedVader
ObservedVaders <- ObservedVader$VADER

mean(ObservedHeights)
ReturnMean <- function(datav, sampleindices) 
  
{
  d <- datav[sampleindices] # we will use this for bootstrapping
  return( mean(d) )
}

## Bootstrapping with 10000 replications
results <- boot(data=as.vector(ObservedVaders), statistic=ReturnMean, R=10000)


hist(results$t)
results

# Bootstrap 95% CI for R-Squared
confAll<- boot.ci(results, type="bca")


#Confidence interval for each topic

topics = c('Business','Social.Media','Technology','Immutability','Influence','Application','Aversion')

#initialise empty list
pinata <- list()
for (i in 1:length(topics)){
    
    bootstrap %>%
    filter(Topic==topics[i]) -> ObservedVader
  ObservedVaders <- ObservedVader$VADER
  
  ## Bootstrapping with 10000 replications
  results <- boot(data=as.vector(ObservedVaders), statistic=ReturnMean, R=10000)
  
  
  hist(results$t)
 
  
  # Bootstrap 95% CI for R-Squared
  
  #results
  test<- boot.ci(results, type="bca")
  pinata[[i]] <- test
    
}

confBusiness<- pinata[1]
confSocialMedia<- pinata[2]
confTechnology<- pinata[3]
confImmutability<- pinata[4]
confInfluence<- pinata[5]
confApplication<- pinata[6]
confAversion<- pinata[7]

confAll
confBusiness
confSocialMedia
confTechnology
confImmutability
confInfluence
confApplication
confAversion



