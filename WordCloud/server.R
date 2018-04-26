library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(wordcloud)

data2 <- read.csv('imdb_movie.csv', as.is = TRUE, header = TRUE, encoding = "UTF-8")

# Replace "\\N" by "Others" according to the data source definition
data2$genres1[data2$genres1 == "\\N"] <- "Others"

server <- function(input, output) {
  
  output$wordcloud <- renderPlot({
    # generate data based on dataset, input$genreType, input$year and input$rate from ui.R
    high_rating <- data2[data2$averageRating >= input$rate[1] & data2$averageRating <= input$rate[2],]
    
    if (input$genreType == "All"){
      Director_high <- high_rating %>% 
        select(title, directorName, averageRating, numVotes, releaseYear, genres1) %>%
        filter(releaseYear >= input$year[1] & releaseYear <= input$year[2]) %>%
        group_by(directors = directorName) %>% 
        summarize(count = n())
    } else {
      Director_high <- high_rating %>% 
        select(title, directorName, averageRating, numVotes, releaseYear, genres1) %>%
        filter(releaseYear >= input$year[1] & releaseYear <= input$year[2] & genres1 == input$genreType) %>%
        group_by(directors = directorName) %>% 
        summarize(count = n())
    }
    
    Director_high <- as.data.frame(Director_high)
    Director_high$directors <- factor(Director_high$directors)
    
    # draw the wordcloud with the specified rate range
    wordcloud(words = Director_high$directors, freq = Director_high$count, min.freq = 1,
              max.words = 100, random.order = FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
}