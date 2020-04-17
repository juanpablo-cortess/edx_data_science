library(dslabs)
library(dplyr)

as.data.frame(data("murders"))
tab1<-slice(murders,1:6) %>% select(state,population)

tab2<-slice(results_us_election_2016,c(1:3,5)) %>% select(state,electoral_votes)

#leftjoin
left_join(tab1,tab2)

#rightjoin
right_join(tab1,tab2, )

inner_join(tab1,tab2)

full_join(tab1,tab2)

semi_join(tab1,tab2)

anti_join(tab1,tab2)

bind_cols(a=1:3,b=4:6)
str(cbind(a=1:3,b=4:6))

bind_cols(tab1,tab2)


library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
Salaries %>% as_tibble()
premiados_2016<-AwardsPlayers %>% as_tibble() %>% filter(yearID==2016)
