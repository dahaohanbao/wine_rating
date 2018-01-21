
library(shiny)
library(dplyr)
library(ggplot2)
library(gapminder)
library(stringr)

wine <- read.csv("https://raw.githubusercontent.com/rq1995/DSCI_522_milestone/master/winemag-data_first150k.csv")
#head(wine)
#unique(wine$region_1)
# region <- gapminder %>% select(country, continent) %>% unique()
# region[region$country == "United States"] <- "US"
# replace(region$country, "United States","US")
# region %>% filter(country=="United States")
# gapminder %>% filter(country== "United States")
# summary(wine)
#129970

#left_join(wine, region)

# Define UI for application that draws a plot
ui <- fluidPage(
  
  # Application title
  titlePanel("Wine Selection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("countryOutput"),
      
      
      sliderInput("priceInput",
                  "Price:",
                  min = 0,
                  max = 1000,
                  step = 10,
                  value = c(0,1000)),
      
      sliderInput("scoreInput",
                  "Score:",
                  min = 0,
                  max = 100,
                  step = 5,
                  value = c(0,100))
      
      #set check box to change continent
      
    ),
    
    # Show a plot
    mainPanel(
      plotOutput("coolPlot")
    )
  )
)

# Define server logic required to draw a graph
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput",label = "Choose a country", unique(wine$country))
  })
  
  output$coolPlot <- renderPlot({
    filtered <- wine %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             points >= input$scoreInput[1],
             points <= input$scoreInput[2],
             country == input$countryInput
      )
    
    ggplot(filtered, aes(x=points, y=price))+
      geom_point()+
      xlab("Wine Score")+
      ylab("Wine Price")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# unique(wine$country) %>% list()
# ggplot(wine,aes(points))+
#   geom_point(aes(y=price))
