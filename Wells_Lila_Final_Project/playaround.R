# Loading Libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(dplyr)
library(tidyr)
library(scales)
library(htmltools)
library(shinyjs)

# Loading data
vgames <- read_csv('data/vgsales.csv') 

# Mutating the data 
vgsales_unique <- vgames[!duplicated(vgames$Name), ] # Getting rid of duplicate values for game names
vgsales_2010 <- subset(vgsales_unique, Year >= 2010) # Only including games released after 2010

# Parsing down the data to only include top 100 selling games 
vgsales <- vgsales_2010 %>% # Filtering for the top 100 grossing games
  arrange(desc(Global_Sales)) %>%
  slice(1:100)
  

game_data <- vgsales %>% filter(Name == "Kinect Adventures!")

comparison_var <- "Genre"