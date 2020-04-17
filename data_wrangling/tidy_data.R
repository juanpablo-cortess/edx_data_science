library(tidyverse)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

tidy_data<-gather(wide_data,'year','fertility','1960':'2015')
tidy_data$year<-as.integer(tidy_data$year)

fertility_and_life_extentancy<-list.files(path)[6]
filename <- file.path(path,  fertility_and_life_extentancy)
wide_data_fertility_life_expentancy <- read_csv(filename)

#forma mía
tidy_data_fertility_life<-gather(wide_data_fertility_life_expentancy,'temp_col','data',-country) %>%
  mutate(year=(str_split(temp_col,'_',simplify = T)[,1])) %>%
  mutate(indice=case_when(
    str_detect(temp_col,regex('fertility',ignore_case = T)) ~ 'fertility',
    T ~ 'life expentancy'
  )) %>% select('country','year','indice','data') %>% spread(indice,data)

#with separate
tidy_data_fertility_life2<-gather(wide_data_fertility_life_expentancy,'temp_col','data',-country) %>%
  separate(temp_col,c('year','variable_name'),sep = "_",extra = "merge") %>% spread(variable_name,data)

co2<-list.files(path)[4]
filename <- file.path(path,  co2)
co2_data <- read_csv(filename)


co2_tidy <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997)) %>% gather('month','co2',-year) 

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat<-admissions %>% select(-applicants)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
