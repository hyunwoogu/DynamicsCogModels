library(shiny)
library(dplyr)
library(ggplot2)

function(input, output, session) {
  Dat = reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing data...")
        getData(input$selection)
      })
    })
  })
  
  plotData = reactive({

    temp= data.frame(x=Dat()[[1]], y=Dat()[[2]])

    
    temp
  })
  
  
  output$plot = renderPlot({
    
    support  = seq(input$xminmin, input$xmaxmax, length.out = 1e4)
    
    if (input$Form == "lin") {
      predicts = input$a_val - input$b_val * support
    } else if (input$Form == "exp") {
      predicts = input$a_val * exp(-input$b_val * support)
    } else if (input$Form == "hyp") {
      predicts = 1/(input$a_val + input$b_val * support)
    } else if (input$Form == "log") {
      predicts = input$a_val - input$b_val * log(support)
    } else if (input$Form == "pow") {
      predicts = input$a_val * support^(-input$b_val)
    } else if (input$Form == "exp_pow") {
      predcits = input$a_val * exp(-2 * input$b_val * sqrt(support))
    }
    
    if (input$Asym == "Asym") predicts = 1 - 1/(predicts + 2)
    
    newDF = data.frame(supp = support,
                       pred = predicts)
    
    p = ggplot(data=plotData(), aes(x=x, y=y)) + 
      geom_point(colour="black", size=2) + 
      geom_line(data=newDF, 
                aes(x=supp, y=pred), 
                color='blue', size=2) +
      ggtitle("Fitting Functions") +
      theme_classic() + 
      theme(plot.title=element_text(hjust=.5, size=20))
    
    if (input$fix){
      p = p + xlim(c(input$xminmin, input$xmaxmax)) + ylim(c(input$yminmin, input$ymaxmax)) 
    }
    p
  }) 
  
}