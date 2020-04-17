library(tidyverse)
library(stringr)
library(lubridate)

data(brexit_polls)
brexit_polls_april<-brexit_polls %>%  filter(startdate >='2016-04-01') %>% filter(startdate <'2016-05-01') 

data_round<-brexit_polls %>% mutate(weekday=weekdays(enddate)) %>% group_by(weekday) %>% summarise(n())

data(movielens)

movielens_date<-movielens %>% mutate(datetime=as_datetime(timestamp)) %>% mutate(year2=round_date(datetime,unit = 'hour')) %>% 
  mutate(hora=hour(datetime)) %>% group_by(hora) %>% summarise(n())

#textmining

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gtb_pride<-gutenberg_metadata %>% filter(str_detect(title,'Pride and Prejudice'))
gutenberg_works(gtb_pride,languages = "en")

pride<-gutenberg_download(1342)

words<-pride %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% filter(!str_detect(word,regex('[0-9]'))) %>%
  count(word,sort = T)

             
more_100<-words %>% filter(n>100)       

afinn <- get_sentiments("afinn")

afinn_sentiments<-inner_join(afinn,words)
afinn_positive<-afinn_sentiments %>% filter(value>0)
afinn_4<-afinn_sentiments %>% filter(value==4)
