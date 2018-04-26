library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(wordcloud)

data2 <- read.csv('imdb_movie.csv', as.is = TRUE, header = TRUE, encoding = "UTF-8")

# Replace "\\N" by "Others" according to the data source definition
data2$genres1[data2$genres1 == "\\N"] <- "Others"

# Define UI for application that draws a barchart and a scatterplot
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # Application title
                titlePanel("Word Cloud for Director"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    # Input: Specify rate to view ----
                    #numericInput("rate", "Rating between:", 5)
                    
                    sliderInput(inputId = "rate",
                                label = "Rating above:",
                                min = 1,
                                max = 10,
                                value = c(1,5),
                                step = 0.5), 
                    
                    # Select type of Genre to plot
                    selectInput(inputId = "genreType", label = strong("Genre index"),
                                choices = c("All", unique(sort(data2$genres1))),
                                selected = "All"),
                    
                    # Select type of Year to plot
                    sliderInput(inputId = "year", 
                                label = strong("Release Year Range"),
                                min = 2013,
                                max = 2017,
                                value = c(2013,2017))
                  ),
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("wordcloud")
                  )
                )
)
