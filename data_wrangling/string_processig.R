library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))
murders_raw$population<-murders_raw$population %>% parse_number()
murders_raw$total <- murders_raw$total %>% parse_number()
murders_raw$murder_rate <-as.numeric(murders_raw$murder_rate)

library(dslabs)
data("reported_heights")

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

#extract a table from pdf

library(dslabs)
data("research_funding_rates")
research_funding_rates 

#donwloading the pdf
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

#pdf already donwloaded
data("raw_data_research_funding_rates")

tab <- str_split(raw_data_research_funding_rates, "\n")[[1]]

the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)

the_names_2<-the_names_2 %>% str_trim() %>% str_split("\\s+",simplify = T)

the_name<-str_c(rep(the_names_1,each = 3 ),the_names_2[-1],sep = '_') %>%
  c(the_names_2[1],.) %>% str_to_lower() %>% str_replace_all(" ","_")

new_research_funding_rates <- tab[6:14] %>%
  str_trim() %>%
  str_split("\\s{2,}", simplify = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE) %>%
  set_names(the_name)%>%
  mutate_at(-1, parse_number)

#assessment 2 
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- (tab[[5]] %>% html_table(fill = TRUE))[-1,]
names<-c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls<-polls %>% set_names(names) %>% filter(str_detect(remain,regex('%'))) %>% parse_guess(remain)

