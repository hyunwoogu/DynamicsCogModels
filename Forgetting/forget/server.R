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
    temp= data.frame(x=Dat()[[2]], y=Dat()[[3]])
    n   = length(temp$x)  
    Sxx = sum((temp$x -mean(temp$x))^2)
    Sxy = sum(scale(temp$x, scale = FALSE)*temp$y)
    
    beta1 = Sxy/Sxx
    beta0 = mean(temp$y) - beta1 * mean(temp$x)
    MSres = (sum(temp$y^2) - n*(mean(temp$y)^2) - beta1*Sxy)/(n-2)
    temp$yhat = beta0 + beta1 * temp$x
    
    temp$CIincr = sapply(temp$x, function(x0) qt(1-(input$alph)/100/2,n-2) * 
                           sqrt(MSres * (1/n + (x0 - mean(temp$x))^2/Sxx)))
    temp$PIincr  = sapply(temp$x, function(x0) qt(1-(input$alph)/100/2,n-2) * 
                            sqrt(MSres * (1/input$future + 1/n + (x0 - mean(temp$x))^2/Sxx)))
    
    temp
  })
  
  output$plot = renderPlot({
    ggplot(data=plotData(), aes(x=x)) +
      geom_point(aes(y=y), colour="black", alpha=1) + 
      geom_line(aes(y=yhat, colour="Linear Estimator"), linetype=1, size=1, alpha=1, show.legend=TRUE) + 
      geom_ribbon(aes(ymin=yhat-PIincr, ymax=yhat+PIincr, fill="Prediction Intv"), alpha=0.4, show.legend=TRUE) +
      geom_ribbon(aes(ymin=yhat-CIincr, ymax=yhat+CIincr, fill="Confidence Intv"), alpha=0.4, show.legend=TRUE) +
      scale_colour_manual(name = "Model", values = c("Linear Estimator"="blue")) +
      scale_fill_manual(name = "Intervals",
                        values =c("Confidence Intv" = "blue", "Prediction Intv" = "red")) +
      ggtitle("Compare the Inteverals") + 
      ylab('y') + xlab('x') +
      theme_classic() + 
      theme(plot.title=element_text(hjust=.5, size=20))}) 
}