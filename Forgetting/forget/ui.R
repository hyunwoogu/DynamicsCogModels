library(shiny)

fluidPage(
  titlePanel("On the Form of Forgetting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selection", "Choose the Data:",
                  choices = Dats),
      actionButton("update", "Change"),
      hr(),
      
      fluidRow(
        column(4, numericInput(inputId = 'xminmin', 'Xmin', value = 0.1)),
        column(4, numericInput(inputId = 'xmaxmax', 'Xmax', value = 15))
      ),
      
      fluidRow(
        column(4, numericInput(inputId = 'yminmin', 'Ymin', value = 0.5)),
        column(4, numericInput(inputId = 'ymaxmax', 'Ymax', value = 1))
      ),
      
      checkboxInput("fix", "Fix the X and Y Limits", FALSE),
      
      radioButtons("Form", label = h3("Select the Functional Forms:"),
                   choices = list("Linear" = "lin", 
                                  "Exponential" = "exp",
                                  "Hyperbolic" = "hyp",
                                  "Logarithmic" = "log",
                                  "Power" = "pow",
                                  "Exponential-Power", "exp_pow"), 
                   selected = "lin"),
      
      radioButtons("Asym", label = h3("Asymptote = 0.5"),
                   choices = list("No"  = "NoAsym", 
                                  "Yes" = "Asym"), 
                   selected = "NoAsym"),
      
      h3("Tune the Params:"),
      
      fluidRow(
        column(6, numericInput(inputId = 'a_val', 'a', value = 1, min=-10, max=10)),
        column(6, numericInput(inputId = 'b_val', 'b', value = 1, min=0, max=10))
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)