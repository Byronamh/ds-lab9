library(shiny)
library(dplyr)

df = read.csv("./data.csv")

poolAreaMin = min(df["PoolArea"])
poolAreaMax = max(df["PoolArea"])
yearBuiltMin = min(df["YearBuilt"])
yearBuiltMax = max(df["YearBuilt"])
fireplacesMin = min(df["Fireplaces"])
fireplacesMax = max(df["Fireplaces"])
options(scipen = 5)


ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("View the price range of a house given:"),
      
      
      sliderInput("poolRange", 
                  label = "Size of pool (m2)",
                  min = poolAreaMin, max = poolAreaMax, value = c(poolAreaMin, poolAreaMax)),
      sliderInput("ageRange", 
                  label = "Year built",
                  min = yearBuiltMin, max = yearBuiltMax, value = c(yearBuiltMin, yearBuiltMax)),
      sliderInput("fireplacesRange", 
                  step = 1,
                  label = "Number of fireplaces",
                  min = fireplacesMin, max = fireplacesMax, value = c(fireplacesMin, fireplacesMax)),
      
    ),
    
    mainPanel(
      textOutput("pool_area_out"),
      textOutput("age_out"),
      textOutput("fireplace_out"),
      
      plotOutput("plotArea")
    )
  )
)

server <- function(input, output) {
  
  output$pool_area_out <- renderText({ 
    paste("You're filtering pool size from: ",
          input$poolRange[1], "to", input$poolRange[2])
  })
  output$age_out <- renderText({ 
    paste("You're filtering houses built from: ",
          input$ageRange[1], "to", input$ageRange[2])
  })
  output$fireplace_out <- renderText({ 
    paste("You're filtering houses with ",
          input$fireplacesRange[1], "to", input$fireplacesRange[2]," fireplaces") 
  })
  
  
  histogramPool <- reactive({
    df %>% filter( df$YearBuilt>input$ageRange[1] & df$YearBuilt<=input$ageRange[2]) 
  })
  
  output$plotArea <- renderPlot(
    hist(histogramPool()$SalePrice,
         main="No. of houses with price range", 
         xlab="Sell price", 
         labels = TRUE,
    ),
    
  )
}

shinyApp(ui, server)
