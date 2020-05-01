library(Lahman)
library(dplyr)
library(broom)
library(stringr)
library(ggplot2)
library(tidyverse)

# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE) #returns estimates and related info for the dataframe

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

summary(fit) #show us all the info in the model

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data.frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()


#A way to actually pick the players for the team can be done using what computer scientists call linear programming. 
#Although we don't go into this topic in detail in this course, we include the code anyway:

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

#algorithm choose this 9 players
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#note that these all players are above av BB and HR

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

#assessment

# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .),conf.int=T))

fit %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)

# effect of year on the impact of BB
fit_2 <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%
  lm(BB ~ yearID,data = .)
  
#assessment verified
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, HR = HR/G,  R = R/G)

tidy(lm(avg_attendance~R,data = Teams_small))
tidy(lm(avg_attendance~HR,data = Teams_small))
tidy(lm(avg_attendance~W,data = Teams_small))
tidy(lm(avg_attendance~yearID,data = Teams_small))

#correlation
summarize(Teams_small, cor(`W`,`R`))
summarize(Teams_small, cor(`W`,`HR`))

# Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest integer. 
# Keep only strata 5 through 10, which have 20 or more data points.

Teams_small_strata<-Teams_small %>%
  mutate(strata_W=round(W/10)) %>%
  filter(strata_W %in% 5:10)

Teams_small_strata %>% group_by(strata_W) %>% 
  summarise(n())

tidy(lm(avg_attendance~strata_W,data = Teams_small_strata))

Teams_small_strata %>%
  group_by(strata_W) %>%
  do(tidy(lm(avg_attendance~R,data =.),conf.int = TRUE))

Teams_small_strata %>%
  group_by(strata_W) %>%
  do(tidy(lm(avg_attendance~HR,data =.),conf.int = TRUE))

Teams_small_strata %>%
  group_by(strata_W) %>%
  do(tidy(lm(avg_attendance~HR + R,data =.),conf.int = TRUE))

# Fit a multivariate regression determining the effects of runs per game, 
# home runs per game, wins, and year on average attendance. 
# Use the original Teams_small wins column, not the win strata from question 3.

fit<-Teams_small %>%
  lm(avg_attendance~R+HR+W+yearID,data = .)
tidy(fit,conf.int = TRUE)

predict_teams_smalls<-Teams %>% 
  mutate(HR = HR/G,  R = R/G) %>%
  filter(yearID %in% 2002) %>% 
  mutate(A_hat = predict(fit, newdata = .),avg_attendance = attendance/G)
  
predict_teams_smalls %>% ggplot(aes(A_hat, avg_attendance, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

summarize(predict_teams_smalls, cor(`A_hat`,`avg_attendance`))
