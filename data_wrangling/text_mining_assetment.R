library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

txt<-pdf_text(fn)
txt[[9]]
x<-str_split(txt[[9]],'\\\n') 
s<-x[[1]]
s<-s %>% str_trim()
header_index<-str_which(s,".*2015.*")[1]
header<-s[header_index]
header<-header %>% str_split('\\s{2,}',simplify = T)


month<-header[1]
header<-header[2:5]

s<-s[-(1:header_index)]
total_index<-str_which(s,".*Total*")[1]
s<-s[-(total_index:length(s))]
s<-s[str_count(s,'\\d+')>1]
s <- str_remove_all(s, "[^\\d\\s]")


s <- str_split_fixed(s, "\\s+", n = 6)[,1:5] %>% as.data.frame() 
s<-setNames(s,c(month,header))
s1 <- mutate_all(s,.funs = as.numeric) %>% order(.$SEP,decreasing = F)
s1<- s1[order(s1$SEP),]
