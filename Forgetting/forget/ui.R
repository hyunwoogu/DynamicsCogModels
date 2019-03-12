library(shiny)

fluidPage(
  titlePanel("On the Form of Forgetting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", "Choose the Data:",
                  choices = Dats),
      actionButton("update", "Change"),
      hr(),
      
      
      sliderInput("a_val",
                  "a:",
                  min = -10,  max = 10, value = 1),
      sliderInput("b_val",
                  "b",
                  min = 0,  max = 10,  value = 1)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)