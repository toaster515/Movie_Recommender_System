library(tidyverse)
library(recommenderlab)
library(shiny)
library(shinyjs)

#https://adp12.shinyapps.io/PSL_app/
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}
load_UBCF = function(train){
  rec_UBCF = Recommender(train, method = 'UBCF',
                         parameter = list(normalize = 'Z-score',
                                          weighted = TRUE,
                                          method = 'Cosine',
                                           nn = 40))
  
}
load_IBCF = function(train){
  rec_IBCF = Recommender(train, method = 'IBCF',
                         parameter = list(normalize = 'Z-score',
                                          method = 'Cosine',
                                          k = 30))
}
load_RMAT = function(ratings){
  #Sparse Matrix for Recommender
  i = paste0('u', ratings$UserID)
  j = paste0('m', ratings$MovieID)
  x = ratings$Rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

movies_adj = read.csv("https://adp12.github.io/moviedata/movies_enriched.csv?raw=true", header=TRUE)
valid.movies = filter(movies_adj,Valid==1)
valid.movies$image_url = sapply(valid.movies$MovieID, 
                                function(x) paste0(small_image_url, x, '.jpg?raw=true'))


ratings = read.csv("https://liangfgithub.github.io/MovieData/ratings.dat",sep=":",colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')


genre_list = c("Action", "Adventure", "Animation", 
               "Children", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film_Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci_Fi", 
               "Thriller", "War", "Western")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

shinyServer(function(input, output, session) {
  
  shinyjs::html(id='loading','...Loading Data')
  Rmat <- load_RMAT(ratings)
  train = Rmat[1:500, ]
  
  shinyjs::html(id='loading','...Generating UBCF Model')
  rec_UBCF <- load_UBCF(train)
  
  shinyjs::html(id='loading','...Generating IBCF Model')
  rec_IBCF <- load_IBCF(train)
  
  hide("loading_page")
  show("main_page")
  
  
  #------------------------------------------------------------------------------------------------------------------------------------
  # Genre Recommendations
  #------------------------------------------------------------------------------------------------------------------------------------
  # Create filter dataframe
  dfg <- eventReactive(input$btn_genre, {
    withBusyIndicatorServer("btn_genre", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.getElementById('box2').setAttribute('style','display: none;');
                document.getElementById('box2').parentNode.classList.add('collapsed-box');
                document.getElementById('box2').parentNode.getElementsByClassName('fas')[0].classList.remove('fa-minus');
                document.getElementById('box2').parentNode.getElementsByClassName('fas')[0].classList.add('fa-plus');"
      runjs(jsCode)
      g = toString(input$genre_sel)
      genre_data <- filter(valid.movies[with(valid.movies,order(Avg,Count,decreasing=TRUE)),],get({{g}})==1)[1:10,]
      recom_results <- data.table(Rank = 1:10,MovieID = genre_data$MovieID,Title = genre_data$Title,Predicted_rating =  genre_data$Avg)
    })
  })
  
  
  # display the recommendations
  output$results_genre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- dfg()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = paste0(small_image_url,recom_result$MovieID[(i - 1) * num_movies + j],'.jpg?raw=true'), height = 120))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            ),
            div(style="text-align:center: font-size: 50%",
                p(paste("Average Rating: ",round(recom_result$Predicted_rating[(i - 1) * num_movies + j],2)))
            )
        )        
      }))) 
    }) 
  }) 
  
  
  #------------------------------------------------------------------------------------------------------------------------------------
  # Ratings Based Recommendations
  #------------------------------------------------------------------------------------------------------------------------------------
  # Calculate recommendations 
  dfr <- eventReactive(input$btn_recmndr, {
    withBusyIndicatorServer("btn_recmndr", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.getElementById('box2').setAttribute('style','display: none;');
                document.getElementById('box2').parentNode.classList.add('collapsed-box');
                document.getElementById('box2').parentNode.getElementsByClassName('fas')[0].classList.remove('fa-minus');
                document.getElementById('box2').parentNode.getElementsByClassName('fas')[0].classList.add('fa-plus');"
      runjs(jsCode)
      jsCode <- "document.getElementById('box1').setAttribute('style','display: none;');
                document.getElementById('box1').parentNode.classList.add('collapsed-box');
                document.getElementById('box1').parentNode.getElementsByClassName('fas')[0].classList.remove('fa-minus');
                document.getElementById('box1').parentNode.getElementsByClassName('fas')[0].classList.add('fa-plus');"
      runjs(jsCode)
      
      #get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      user_ratings = as.data.frame(user_ratings)
      user_ratings = drop_na(user_ratings)
      print(user_ratings)
      
      if (nrow(user_ratings)==0){
        #no ratings, use generic recommendation
        user_predicted_ids <- 1:10
        rankings = NA
      }else{
        #check if all ratings are the same
        if (length(unique(user_ratings$Rating))==1){
          #if all the same, have to alter to avoid error
          #adjust first rating by insignificant amount for numerical stability
          user_ratings[1,2]=user_ratings[1,2]-0.1
        }
      
        movieIDs = colnames(Rmat)
        n.item = ncol(Rmat)  
        new.ratings = rep(NA, n.item)
        
        for (x in 1:nrow(user_ratings)){
          new.ratings[which(movieIDs == paste0("m",user_ratings[x,1]))]=user_ratings[x,2]
        }
        
        new.user = matrix(new.ratings, nrow=1, ncol=n.item,dimnames = list(user=paste('u0'),item=movieIDs))
        new.Rmat = as(new.user, 'realRatingMatrix')
        print(paste("Sys:",input$recmndr_sys))
        if (toString(input$recmndr_sys)=="UBCF"){
          rec_pred <- predict(rec_UBCF, new.Rmat, type = 'ratings')
        }else if (toString(input$recmndr_sys)=="IBCF"){
          rec_pred <- predict(rec_IBCF, new.Rmat, type = 'ratings')
        }
        
        pmtx = as.data.frame(t(as(rec_pred, 'matrix')))
        pmtx = drop_na(pmtx)
        pmtx <- tibble::rownames_to_column(pmtx, "ID")
        colnames(pmtx)<-c("MovieID",'Rating')

        recommend <- pmtx[with(pmtx,order(Rating,decreasing = TRUE)),][1:10,]
        
        colnames(recommend) <- c('MovieID',"Rating")
        recommend$MovieID <- as.numeric(gsub("m","",as.character(recommend$MovieID)))
        
        user_predicted_ids <- recommend[,1]
        rankings = recommend[,2]
      }
      
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = movies$MovieID[user_predicted_ids],
                                  Title = movies$Title[user_predicted_ids],
                                  Predicted_rating =  rankings)
      
    })
    
  })
  
  #Ratings panel
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # display the recommendations
  output$results_recmndr <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- dfr()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),

            div(style = "text-align:center", 
                a(img(src = paste0(small_image_url,recom_result$MovieID[(i - 1) * num_movies + j],'.jpg?raw=true'), height = 120))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            ),
            div(style="text-align:center: font-size: 50%",
                p(paste("Sys Score: ",round(recom_result$Predicted_rating[(i - 1) * num_movies + j],2)))
            )
        )        
      }))) 
    }) 
  }) 
  
  

  
}) # server function