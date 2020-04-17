library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#there are 5 ways a batter can succed (not make an out)
# 1. Bases on balls, 
# 2. Single, hit the ball and gets to first base
# 3. Double, hit the ball and gest to second base
# 4. Triple, hit the ball and gest to third base
# 5. Home run, hits the ball and goes all the way home and scores a run



#scatterplot of relationship 
#HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#stolen bases vs wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Bases in balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#At bat per game vs runs per game 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#correlation
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  summarize(r = cor(AB_per_game, R_per_game)) %>% pull(r)

#wins per game vs error per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

#correlation
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  summarize(r = cor(W_per_game, E_per_game)) %>% pull(r)

#triples per game vs double per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

#correlation
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  summarize(r = cor(X3B_per_game, X2B_per_game)) %>% pull(r)



