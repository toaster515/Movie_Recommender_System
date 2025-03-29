library(recommenderlab)
library(tidyverse)
library(Matrix)
setwd("")

ratings = read.csv("data/ratings.dat",sep=":",colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

head(ratings)


i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

train = Rmat[1:500, ]
test = Rmat[501, ]

rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'center', 
                                        method = 'Cosine', 
                                        nn = 25))
#Summary of model parameters
rec_UBCF@model

#Deploy Shiny App
#library(rsconnect)
#rsconnect::setAccountInfo(name='', token='', secret='')
#rsconnect::deployApp('path/to/your/app')

