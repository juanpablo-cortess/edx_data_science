library(Lahman)
library(tidyverse)
library(ggplot2)


#HRs and wins
teams2<-Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game = R/G) %>%
  mutate(BB_per_game = BB/G) %>%
  mutate(HR_per_game = HR/G)
fit2<-lm(runs_per_game~ BB_per_game + HR_per_game,data = teams2)
fit2$coefficients

#assessment
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_03<- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_03_02<-bat_03 %>% group_by(playerID) %>% summarise(mean_singles=mean(singles),mean_bb=mean(bb))

bat_03 %>% group_by(playerID) %>% summarise(mean_singles=mean(singles)) %>%
  filter(mean_singles>0.2)

bat_03 %>% group_by(playerID) %>% summarise(mean_bb=mean(bb)) %>%
  filter(mean_bb>0.2)

resume_table<-inner_join(bat_02,bat_03_02) 

resume_table %>% ggplot(aes(mean_singles,singles)) + geom_point(stat = 'identity')
resume_table %>% ggplot(aes(mean_bb,bb)) + geom_point(stat = 'identity')

model_01<-lm(data=resume_table,singles~mean_singles)
model_01$coefficients
model_02<-lm(data=resume_table,bb~mean_bb)
model_02$coefficients
