#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# Adding a sales_category column
vgsales$Sales_Category <- ifelse(vgsales$NA_Sales > 0, "North America", 
                                 ifelse(vgsales$EU_Sales > 0, "Europe", 
                                        ifelse(vgsales$JP_Sales > 0, "Japan", 
                                               ifelse(vgsales$Other_Sales > 0, "Other", 
                                                      ifelse(vgsales$Global_Sales > 0, "Global", "Unknown")))))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Video Game Sales"),
    tags$h1(),
    div("Comparative sales by region\n and game characteristics.", style = "font-size: 20px; color: grey; margin-bottom: 15px;"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "left",
        sidebarPanel(
          style = "background-color: rgba(213, 202, 198, 0.3)",
            # Input for game name
            selectInput(inputId = "game",
                        label = "Select a game:",
                        choices = unique(vgsales$Name)),
            # Input: Choose a comparison type
            radioButtons(inputId = "plot_type", 
                         label = "Choose a comparison:",
                         choices = c("Sales by game characteristics", 
                                     "Sales by region")
            ),
            
            # Conditional inputs for sales by region
            conditionalPanel(condition = "input.plot_type == 'Sales by region'",
                             # Adding an annotation
                             div("Select a region below to learn more about the sales there for the game you choose in the text below the table: ", style = "color: grey; margin-bottom: 10px;"),
                             # Input for sales region
                             radioButtons(inputId = "region",
                                          label = "Select a sales region to highlight:",
                                          choices = list("North America",
                                                         "Europe",
                                                         "Japan",
                                                         "Other",
                                                         "Global"))
            ),
            # Conditional inputs for sales by game characteristics 
            conditionalPanel(condition = "input.plot_type == 'Sales by game characteristics'",
                             # Adding an annotation
                             div("Select a comparison method below to visualize.", style = "color: grey; margin-bottom: 10px;"),
                             # Input: Choosing a comparison metricm 
                             radioButtons(inputId = "comparison", 
                                          label = "Choose a Comparison:",
                                          choices = c("Genre", 
                                                      "Year", 
                                                      "Platform", 
                                                      "Publisher"),
                                          selected = "Genre")
                             
            ),
          
          # Adding thank you-text
          div("Thanks to Gregory Smith from Kaggle, who amalgamated the ",
              HTML("<a href='https://www.kaggle.com/gregorut/videogamesales'>data</a>"),
              " used in this application.", style = "margin-bottom: 10px; color: grey;"),
          
          div('Application author: Lila Wells', style = "color: grey;")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel(
            # When the user clicks 'Sales by region'
            condition = 'input.plot_type == "Sales by region"',
            # Specifying the outputs
            plotOutput("game_sales_plot"),
            tags$h2(),
            tableOutput("game_sales_table"),
            tags$h2(),
            uiOutput("game_info"),
            tags$h2(),
            uiOutput("option1_info"),
            tags$h2()
          ),
          conditionalPanel(
            # When the user clicks 'Sales by game characteristics'
            condition = 'input.plot_type == "Sales by game characteristics"',
            plotOutput("game_characteristics_plot"),
            tags$h2(),
            uiOutput("game_characteristics"),
            tags$h2(),
            tableOutput("game_characteristics_table"),
            tags$h2(),
            uiOutput("option2_info"),
            tags$h2()
          )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Execute this block of code only if "Sales by region" is selected
  observeEvent(input$plot_type, { 
    if(input$plot_type == "Sales by region") {
      # Filter the data based on the selected game name
      game_data <- reactive({vgsales %>% filter(Name == input$game)})
      
      # Create a new column for the total sales by region
      game_data_total <- reactive({
        game_data() %>% 
          mutate(Total_Sales = NA_Sales + EU_Sales + JP_Sales + Other_Sales + Global_Sales)})
      
      # Convert columns for sales by region into a single column
      game_data_long <- reactive({
        game_data_total() %>%
          pivot_longer(cols = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"), names_to = "Region", values_to = "Sales")})
      
      # Determine the selected region 
      selected_region <- reactive({input$region})
      
      # Generate a barplot
      output$game_sales_plot <- renderPlot({
        ggplot(data = game_data_long(), aes(x = Region, y = Sales)) +
          geom_bar(stat = "identity", fill = "#ff746c") +
          # Adding the plot title and axis titles 
          labs(title = paste("Sales by Region for", input$game),
               x = NULL,
               y = "Sales (in millions of dollars)") +
          # Specifying the theme
          theme_minimal() +
          # Formatting the y axis
          scale_y_continuous(labels = dollar_format(prefix = "$")) +
          # Formatting the x axis 
          scale_x_discrete(limits = c("JP_Sales", "Other_Sales", "EU_Sales", "NA_Sales", "Global_Sales"),
                           labels = c("Japan", "Other", "Europe", "North America", "Global")) + 
          # Specifying additional plot attributes
          theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.title.x = element_text(face = "bold", size = 15, hjust = 0.5, vjust = -1.5),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13, color = "black"),
            legend.position = "none"
          ) +
          coord_flip()
      })
  
      # Filter the data based on the selected game name
      game_data <- reactive({vgsales %>% filter(Name == input$game)})
      
      # Create a new column for the total sales by region
      game_data_total <- reactive({game_data() %>% 
          mutate(Total_Sales = NA_Sales + EU_Sales + JP_Sales + Other_Sales + Global_Sales)})
      
      # Convert columns for sales by region into a single column
      game_data_long <- reactive({
        game_data_total() %>%
          pivot_longer(cols = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"), names_to = "Region", values_to = "Sales") 
      })
      
      # Determine the selected region 
      selected_region <- reactive({input$region})
      
      # Generate a table of sales by region for the selected game
      output$game_sales_table <- renderTable({
        game_data_total() %>%
          select(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales) %>%
          mutate(across(everything(), ~ paste0(dollar_format(prefix = "$")(./1), " million"))) %>%
          rename("North America" = NA_Sales, "Europe" = EU_Sales, "Japan" = JP_Sales, "Other" = Other_Sales, "Global" = Global_Sales)
      }, width = "100%")
      
      # Generate a text output that talks about the region the user highlighted 
      game_sales <- reactive({
        vgsales %>%
          filter(Name == input$game) %>%
          select(Name, NA_Sales, JP_Sales, EU_Sales, Other_Sales, Global_Sales) %>% 
          rename(`North America` = NA_Sales,
                 Japan = JP_Sales,
                 Europe = EU_Sales,
                 Other = Other_Sales,
                 Global = Global_Sales)})
      
      # Defining the output
      output$game_info <- renderUI({
        sales <- game_sales() %>% 
          filter(Name == input$game) %>% 
          pull(!!sym(input$region))
        
        global_sales <- game_sales() %>% 
          filter(Name == input$game) %>% 
          pull(Global)
        
        percentage <- round(sales / global_sales * 100, 2)
        
        HTML(paste("<div style='font-size: 16px'>The sales for the game ", 
                   tags$b(input$game), 
                   " in ", 
                   tags$b(input$region), 
                   " was",
                   tags$b("$", as.character(sales)),
                   tags$b(" million dollars."),
                   "That represents approximately ",
                   tags$b(percentage, "%"),
                   "of the total sales for this game.</div>"))}) 
      
      output$option1_info <- renderUI(
        HTML(paste("<div style='color: grey'>This facet of the Video Game Sales Shiny app gives you more detailed insights into the regional and global sales of", 
                   tags$b(input$game), 
                   "as well as where it was the most and least successful.",
                   "To operate this section of the app, choose a game in the first dropdown menu, then select a sales region to highlight. Your choices are:",
                   tags$b("'North America', 'Europe', 'Japan', 'Other', or 'Global'."),
                   "Note that choosing 'Global' will show you the game's total sales (as global sales encompass North American, European, Japanese, and other sales).",
                   tags$h2(),
                   tags$b("But what will clicking a sales region to highlight do? Let me tell you!"),
                   "The table below the bar plot updates automatically with your game choice to give you more insights into exactly how much the game sold in each of the sales regions. But what's especially interesting is that the text below the bar plot will update depending on the game of your choice, here the example is",
                   tags$b(input$game),
                   "The text will let you know now only how much the game sold in the region of your choice, but also what percentage that number is of the game's total global sales. Now, go out and have some fun with this!</div>")))

    } else if(input$plot_type == "Sales by game characteristics") {
      # Filter the data based on the selected game name
      game_data <- reactive({vgsales %>% filter(Name == input$game)})
      
      # Filtering the vgsales dataset for the category comparison visualization
      get_average_sales_by_category <- function(comparison_var) {
        vgsales %>%
          filter(vgsales[[comparison_var]] == game_data()[[comparison_var]]) %>%
          summarise(avg_sales = mean(Global_Sales)) %>%
          pull(avg_sales)
      }
      
      
      # Creating the UI output
      output$game_characteristics <- renderUI(
        HTML(paste("<div style='font-size: 16px'>Characteristics of the game ", tags$b(input$game), "</div>")))
      
      # Generating a table 
      output$game_characteristics_table <- renderTable({
        game_data() %>%
          select(Genre, Year, Platform, Publisher) %>%
          rename("Genre" = Genre, "Year Published" = Year, "Platform" = Platform, "Publisher" = Publisher)
      }, width = "100%")
     
      # Generating a visualization
      output$game_characteristics_plot <- renderPlot({
        
        # Define the reactive data frame ---- THIS MIGHT BE THE ISSUE
        reactive_df <- reactive({
          # Compute the data frame based on the input values
          tibble(Category = c("Selected Game", "Average"),
                 Global_Sales = c(game_data()$Global_Sales, get_average_sales_by_category(input$comparison))
          )})
        
        ggplot(data = reactive_df(), aes(x = Category, y = Global_Sales, fill = Category)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = dollar_format(prefix = "$")(Global_Sales), vjust = 2),
                     color = "white",
                     size = 7,
                     fontface = "bold") +
          scale_y_continuous(labels = dollar_format(prefix = "$")) +
          labs(title = paste("Global Sales of", input$game, 'Versus \n Average Global Sales for Games from the Same', input$comparison),
               x = NULL,
               y = "Sales (in millions of dollars)") +
          theme_minimal() +
          theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 13, color = "black"),
            axis.title.y = element_text(size = 15, face = "bold"),
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 11)
          ) +
          scale_fill_manual(values = c("Selected Game" = "#ff746c", 
                                       "Average" = "lightgrey"))
      })
      
      output$option2_info <- renderUI(
        HTML(paste("<div style='color: grey'>This facet of the Video Game Sales Shiny app gives you insight into how the global sales of", 
                   tags$b(input$game), 
                   "relative to the average global sales of other games from the same",
                   tags$b("genre, platform, publisher or year published."),
                   "To operate this section of the app, choose a game in the first dropdown menu, then choose a method of comparison. Your choices are:",
                   tags$b("'Same Genre', 'Same Platform', 'Same Publisher', or 'Same Year Published'."),
                   "The plot will then populate a bar for the global sales for the game you choose, as well as a bar representing the average sales for other games in that category",
                   tags$h2(),
                   tags$b("But wait! The table is also interactive!"),
                   "The table below the bar plot will also update to give you more information on the characteristics of the game you chose, here the example is",
                   tags$b(input$game),
                   "Now, go out and have some fun with this!</div>")))
        
    } else {
      print("Error!")
    }
  
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

