## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(shinycssloaders)
source('helpers.R')
genre_list = c("Action", "Adventure", "Animation", 
               "Children", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film_Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci_Fi", 
               "Thriller", "War", "Western")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "PSL Recommender"),
    
    dashboardSidebar(disable = TRUE),
    
    dashboardBody(includeCSS("movies.css"),
        useShinyjs(),
        
        div(id='loading_page',
          fluidPage(
            box(width=12, status='warning',solidHeader = TRUE,
                title = "Loading",
                img(src="loading.gif",align="center",height='250px',width='500px'),
                br(),
                #uiOutput("loading"),
                htmlOutput('loading')
            )
          )
        ),
        
        hidden(
          div(id='main_page',
        
          #Genre based recommend
          fluidRow(
            useShinyjs(),
            box(
              width = 12, status = 'primary', solidHeader = TRUE,collapsible = TRUE,
              title = "System 1: Genre Based Ranking", id="box1",
              selectInput("genre_sel","Choose your favorite genre:",genre_list),
              br(),
              withBusyIndicatorUI(
                actionButton("btn_genre", "Genre Based Recommendations", class = "btn-warning", name="genre_btn")
              ),
              br(),
              tableOutput("results_genre")
            )
        ),
  
        #Rating inputs
        fluidRow(
          box(width = 12, title = "System 2: Ratings Based Ranking",id="box2",
              status = "primary", solidHeader = TRUE, collapsible = TRUE,
              "Rate Movies to train the recommender system",
              br(),
  
              div(class = "rateitems",
                  uiOutput('ratings')
              )
          )
        ),
  
        #Recommend based on ratings
        fluidRow(
          useShinyjs(),
          box(
            width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            title = "Custom Ranking",id="box3",
            selectInput("recmndr_sys","System:",c("UBCF","IBCF")),
            br(),
            withBusyIndicatorUI(
              actionButton("btn_recmndr", "Custom Recommendations", class = "btn-warning")
            ),
            br(),
            tableOutput("results_recmndr")
          )
      )
    )))
  )
) 