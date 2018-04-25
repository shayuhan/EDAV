library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

# Define server logic required to draw a histogram
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

server <- function(input, output) {
  
  output$barPlot <- renderPlotly({
    # generate data based on dataset, input$genreType, input$year and input$obs from ui.R
    subdata <- movie_for_genres %>% 
      select(title, averageRating, numVotes, releaseYear, genres) %>%
      filter(releaseYear == input$year & genres == input$genreType) %>%
      arrange(desc(numVotes))
    
    options(scipen = 200)
    n = input$obs
    
    subdata <- as.data.frame(subdata)
    subdata$title <- factor(subdata$title, levels = unique(subdata$title[order(-subdata$numVotes)]))
    
    # draw the barchart with the specified genre and year
    g1 <- ggplot(subdata[1:n, ], aes(title, numVotes)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5, face = "bold"), axis.text.y = element_text(size = 5)) +
      labs(x = "Movie Title", y = "Number of Votes", title = "Number of Votes by Genres by Year")
    
    ggplotly(g1)
  })
  
  output$scatPlot <- renderPlotly({
    # generate data based on dataset, input$genreType, input$year and input$obs from ui.R
    subdata <- movie_for_genres %>% 
      select(title, averageRating, numVotes, releaseYear, genres) %>%
      filter(releaseYear == input$year & genres == input$genreType) %>%
      arrange(desc(numVotes))
    
    options(scipen = 200)
    n = input$obs
    
    subdata <- as.data.frame(subdata)
    subdata$title <- factor(subdata$title, levels = unique(subdata$title[order(-subdata$averageRating)]))
    
    # draw the scatterplot with the specified genre and year
    g2 <- ggplot(subdata[1:n, ], aes(title, averageRating)) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5), axis.text.y = element_text(size = 7)) +
      labs(x = "Movie Title", y = "Average Rating", title = "Average Rating by Genres by Year")
    
    ggplotly(g2)
  })
}

