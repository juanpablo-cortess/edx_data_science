library(rvest)
library(stringr)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")

table<-html_table(nodes[[8]])
table<-as.data.frame(table) 

tab1<-nodes[[10]] %>% html_table() %>% setNames(c('no','Team','Payroll','Average')) %>%
  select('Team','Payroll','Average')
tab1 <-tab1[-1,]
tab2<-nodes[[19]] %>% html_table() %>% setNames(c('Team','Payroll','Average'))
tab2 <-tab2[-1,]
full_join(tab1,tab2,by='Team') %>% as_tibble()


library(tidyverse)
url2 <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h2<-read_html(url2)
nodes2<-html_nodes(h2,'table')

for (i in 1:length(nodes2)){
  col='';
  if (ncol(as.data.frame(html_table(nodes2[i],fill = T)))==9 & as.data.frame(html_table(nodes2[i],fill = T))[[1,1]]=='Date(s) conducted'){
    col=i;
    break()
  }
}

data<-as.data.frame(html_table(nodes2[i],fill = T))[-1,] %>% as_tibble()


