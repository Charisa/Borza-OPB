library(shinydashboard)
library(dbplyr)
#source("auth.R")

drv <- dbDriver("PostgreSQL")
source("../auth.R")
sign.up.user <- function(name, surname, address, city, country, emso, mail, username, pass){
  input <- c(name, surname, address, city, country, emso, mail, username, password)
  if(!all(input!="")){
    return(-1)  # If any value is missing
  }
  useraccount <- data.frame(name, surname, address, city, country, emso, mail, username, password=pass)
  success <- 0      # Boolean, if the insertion into the db was successful
  tryCatch({
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    dbWriteTable(conn,name="useraccount", useraccount, append=TRUE, row.names = FALSE)
    success <- success + 1
  
  }, finally = {
    dbDisconnect(conn)
    print(success)
    return(success)
  })
}

#drv <- dbDriver("PostgreSQL")
#conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
#dbDisconnect(conn)
shinyServer(function(input, output){
  output$signUpBOOL <- eventReactive(input$signup_btn, 1) # Gumb, ce se hoce uporabnik registrirat
  outputOptions(output, 'signUpBOOL', suspendWhenHidden=FALSE)  # Da omogoca skrivanje/odkrivanje
  
  # test <- validate(need(sign.up.user(input$nameSignUp, 
  #                                    input$surnameSignUp, 
  #                                    input$addressSignUp, 
  #                                   input$citySignUp, input$countrySignUp, input$emsoSignUp,
  #                                   input$mailSignUp, input$userNameSignUp, input$passwordSignUp)==(-1),"error"))
  
  test <-  observeEvent(input$signup_btnSignUp,
                   sign.up.user(input$nameSignUp, input$surnameSignUp, input$addressSignUp, 
                                input$citySignUp, input$countrySignUp, input$emsoSignUp,
                                input$mailSignUp, input$userNameSignUp, input$passwordSignUp))

}
)

#sign.up.user("name", "surname", "addres", "city", "country", "emso", "mail", "username","password")

