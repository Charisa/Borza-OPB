library(shinydashboard)
library(dbplyr)
#source("auth.R")


#drv <- dbDriver("PostgreSQL")
#conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
#dbDisconnect(conn)
shinyServer(function(input, output){
  output$signUpBOOL <- eventReactive(input$signup_btn, 1) # Gumb, ce se hoce uporabnik registrirat
  outputOptions(output, 'signUpBOOL', suspendWhenHidden=FALSE)  # Da omogoca skrivanje/odkrivanje
  
  
  
  
  
}
)
