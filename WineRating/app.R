library(shiny)
library(dplyr)
library(ggplot2)
library(DT)


wine <- read.csv("https://raw.githubusercontent.com/dahaohanbao/wine_rating/master/data/winemag-data_first150k%202.csv") %>% 
  select(country, points, price, variety) %>% na.omit() %>% droplevels()

# Define UI for application that draws a plot
ui <- fluidPage(
  
  # Application title
  titlePanel("Wine Selection"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      uiOutput("countryOutput"),
      
      uiOutput("varietyOutput"),
      
      
      
      sliderInput("priceInput",
                  "Price:",
                  min = 0,
                  max = 1000,
                  step = 10,
                  value = c(0,1000)),
      
      sliderInput("scoreInput",
                  "Score:",
                  min = 80,
                  max = 100,
                  step = 1,
                  value = c(80,100)),
      
      radioButtons("typeInput","Graph Type",
                   choices = c("Scatter plot", "Box plot","Score Density plot","Price Density plot"),
                   selected = "Score Density plot")
      
    ),
    
    # Show a plot
    mainPanel(
      plotOutput("coolPlot")
    )
  ),
  dataTableOutput("results")
)

# Define server logic required to draw a graph
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput",label = "Choose a country", sort(unique(wine$country)),
                selected = c("US","Canada"),multiple = TRUE)
  })
  
  output$varietyOutput <- renderUI({
    selectInput("varietyInput",label = "Choose a variety", sort(unique(wine$variety)),
                selected = c("Pinot Noir","Cabernet Sauvignon","Sangiovese","Sauvignon Blanc"),
                multiple = TRUE)
  })
  
  
  output$coolPlot <- renderPlot({
    filtered <- wine %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             points >= input$scoreInput[1],
             points <= input$scoreInput[2],
             country %in% input$countryInput,
             variety %in% input$varietyInput)
    
    if (input$typeInput == "Scatter plot"){
      return(
        ggplot(filtered, aes(x=points, y=price))+
          geom_point(aes(color = country), alpha = 0.5)+
          xlab("Wine Score")+
          ylab("Wine Price")+
          labs(title="Wine Score vs Price")
      )
    }
    
    if (input$typeInput == "Box plot"){
      return(
        ggplot(filtered, aes(x=country, y=price),group=variety)+
          geom_boxplot(aes(color = country),outlier.shape = NA)+
          scale_y_continuous(limits = quantile(filtered$price, c(0.1, 0.9)))+
          xlab("Country")+
          ylab("Wine Price")+
          labs(title="Country vs Wine Price")
      )
    }
    
    
    if (input$typeInput == "Score Density plot"){
      return(
        ggplot(filtered, aes(points))+
          geom_density(aes(fill = country),alpha = 0.5)+
          xlab("Score")+
          ylab("Density")+
          labs(title="Score Density plot")
      )
    }
    
    if (input$typeInput == "Price Density plot"){
      return(
        ggplot(filtered, aes(price))+
          geom_density(aes(fill = country),alpha = 0.5)+
          xlab("rice")+
          ylab("Density")+
          labs(title="Price Density plot")
      )
    }
    
  })
  
  output$results <- renderDataTable({
    filtered <- wine %>%
      filter(price >= input$priceInput[1],
             price <= input$priceInput[2],
             points >= input$scoreInput[1],
             points <= input$scoreInput[2],
             country %in% input$countryInput,
             variety %in% input$varietyInput)
    datatable(filtered)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


