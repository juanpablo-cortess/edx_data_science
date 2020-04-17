library(tidyverse)
path<-system.file("extdata",package = 'dslabs')
list.files(path)
filename="murders.csv"
filename=list.files(path)[7]
fullpath=file.path(path,filename)

# copy file from dslabs package to your working directory
file.copy(fullpath,getwd())

# check if the file exists
file.exists(filename)

#leer primeras lineas
readLines('murders.csv',max(3))

data<-read.csv('murders.csv',stringsAsFactors = FALSE)
data2<-read_csv('murders.csv')

download.file("http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",'breast-cancer.csv')
read.csv('breast-cancer.csv')
