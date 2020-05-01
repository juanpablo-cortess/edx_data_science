# UC-Berkeley admission data
library(dslabs)
library(dplyr)
library(broom)
library(tidyverse)
data(admissions)
admissions

# percent men and women accepted
admissions  %>%
  ggplot(aes(admitted,fill=gender))+geom_histogram() + facet_wrap(.~major)

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))
#result that there are not independent

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))


#assessment

data("research_funding_rates")
research_funding_rates

total_table<- research_funding_rates %>% select(discipline,applications_total,awards_total,success_rates_total)

table_research<- research_funding_rates %>% 
  select(-applications_total,-awards_total,-success_rates_total) %>%
  gather(v1,total,-discipline) %>%
  separate(v1,c('type','gender'),sep = '_',extra = 'merge') %>%
  mutate(gender=case_when(
    str_detect(gender,'women')~'women',
    T~'men'
  ))%>%
  spread(type,total,convert = F)

#men not awarded
table_research %>% group_by(gender) %>% summarise(not_awarded=sum(applications)-sum(awards),awarded=sum(awards)) %>%
  select(-gender) %>% chisq.test()


dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total") 
dat%>%
  ggplot(aes(x=discipline,y=success,fill=gender))+geom_histogram(stat = 'identity')

#discipline with best performance for men
dat %>% group_by(discipline) %>% mutate(is_higer_sucess=success-lag(success)) %>% filter(is_higer_sucess<0)

#most applicantions are for women
dat %>% group_by(discipline) %>% mutate(is_higer_app=applications-lag(applications)) %>% filter(is_higer_app>0)

#two lower overall funding rates
dat %>% group_by(discipline) %>% summarise()
