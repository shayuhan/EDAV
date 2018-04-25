library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

# Load data
# library(rsconnect)
movie <- read.csv("imdb_movie.csv", as.is = TRUE, header = TRUE, encoding = "UTF-8")

# Tidy genres
new_movie <- gather(movie, "genres_no", "genres", 5:7)
new_movie <- new_movie[new_movie$genres != "", ]

# For genres related topics, not related to Region and Language, below is the dataset
movie_for_genres <- new_movie %>% distinct(new_movie, tconst, title, releaseYear, runtimeMinutes, averageRating, numVotes, directorName, writerName, genres_no, genres)

# Replace "\\N" by "Others" according to the data source definition
movie_for_genres$genres[movie_for_genres$genres == "\\N"] <- "Others"

i = 0
result <- NULL
for(gen in unique(movie_for_genres$genres)){
  i <- i+1
  result[i] <- nrow(movie_for_genres[movie_for_genres$genres == gen,])
}

maxnum <- max(result)

# Define UI for application that draws a barchart and a scatterplot
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # Application title
                titlePanel("Number of Votes & Average Rating by Genres by Year"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    # Select type of Genre to plot
                    selectInput(inputId = "genreType", label = strong("Genre index"),
                                choices = unique(sort(movie_for_genres$genres)),
                                selected = "Action"),
                    
                    # Select type of Year to plot
                    selectInput(inputId = "year", label = strong("Release Year index"),
                                choices = unique(movie_for_genres$releaseYear,fromLast = TRUE),
                                selected = "2013"),
                    
                    # Input: Specify the number of observations to view ----
                    #numericInput("obs", "Number of observations to view:", 20)
                    
                    sliderInput(inputId = "obs",
                                label = "Number of observations:",
                                min = 1,
                                max = 100, # maxnum
                                value = 20)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotlyOutput("barPlot"),
                    plotlyOutput("scatPlot")
                  )
                )
)
