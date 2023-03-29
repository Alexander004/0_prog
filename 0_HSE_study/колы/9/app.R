library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Load the data
migration_Roma <- read_delim("migration_Roma.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(migration_Roma)

# Remove NAs
migration_Roma = na.omit(migration_Roma)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Foreigners in Rome"),
  # Sidebar with a select input
    selectInput("con", "Choose the continent of origin:", migration_Roma$Continente),
  
  # Show a plot
  plotOutput("Rome")
  
)

# Define server logic required to draw a bar plot
server <- function(input, output) {
  
  output$Rome <- renderPlot({
    
    # Filter by the input$con
    migration_Roma <- filter(migration_Roma, Continente == input$con)
    
    # Count the sum of Totale for every country of origin
    country <- group_by(migration_Roma, Cittadinanza) %>% 
      summarise(people = sum(Totale)) %>%
      # Arrange from the largest to the smallest
      arrange(desc(people)) %>% 
      # Show the top 10
      top_n(15)
    
    # Create a bar plot
    ggplot(country, aes(reorder(Cittadinanza, people))) +
      geom_bar(aes(weight = people), fill = "tomato3") + 
      coord_flip() +
      ggtitle("Top 15 countries") +
      xlab("Country of origin") +
      ylab("Number of people") +
      theme_bw(base_size = 16)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

#Tasks:
#1.Change the plot color to any you like most
#Reorganize the plot in the following way:
#2.1 Now the choice list consists of top 15 countries of origin
#2.2 The graph shows the number of people from this country in Municipal District (Comune)

