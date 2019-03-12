library(shiny)

Dats <<- list("Fig1_5s" = "Dat1", 
              "Fig1_1s" = "Dat2",
              "Fig2"    = "Dat3",
              "Fig3"    = "Dat4")

getData <<- function(Dat)
{
  if (!(Dat %in% Dats))
    stop("Unknown Data")
  
  if (Dat == "Dat1") {
    return(data.frame(x = c(2.5, 5, 10, 20, 40),
                      y = c(0.59, 0.52, 0.47, 0.41, 0.412)))
  } else if (Dat == "Dat2") {
    return(data.frame(x = c(2.5, 5, 10, 20, 40),
                      y = c(0.78, 0.69, 0.63, 0.59, 0.56)))
  } else if (Dat == "Dat3") {
    return(data.frame(x = c(0.01, 1., 7., 14.),
                      y = c(0.825, 0.78, 0.76, 0.755)))
  } else if (Dat == "Dat4") {
    return(data.frame(x = c(0.5, 1, 2, 6),
                      y = c(0.845, 0.8, 0.72, 0.65)))
  }
}
