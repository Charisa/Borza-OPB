#drv <- dbDriver("PostgreSQL")
#conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
#dbDisconnect(conn)
shinyServer(function(input, output){
  source("../auth.R")
  source("serverFunctions.R")
  library(shiny)
  library(shinyBS)
  library(dplyr)
  library(dbplyr)
  library(DBI)
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  
  output$signUpBOOL <- eventReactive(input$signup_btn, 1) # Gumb, ce se hoce uporabnik registrirat
  outputOptions(output, 'signUpBOOL', suspendWhenHidden=FALSE)  # Da omogoca skrivanje/odkrivanje

  # Greyout of signin button
  observeEvent(c(input$userName,input$password), {
    shinyjs::toggleState("signin_btn", 
                         all(c(input$userName, input$password)!=""))
  })
  
  # Sign in protocol
  observeEvent(input$signin_btn,
               {signInReturn <- sign.in.user(input$userName, input$password)
               if(signInReturn[[1]]==1){
                 userID <- signInReturn[[2]]
                 output$signUpBOOL <- eventReactive(input$signin_btn, 2)
               }else if(signInReturn[[1]]==0){
                 showModal(modalDialog(
                   title = "Error during sign in",
                   paste0("An error seems to have occured. Please try again."),
                   easyClose = TRUE,
                   footer = NULL
                 ))
               }else{
                 showModal(modalDialog(
                   title = "Wrong Username/Password",
                   paste0("Username or/and password incorrect"),
                   easyClose = TRUE,
                   footer = NULL
                 ))
               }
               })
  
  # Greyout of signup button
  observeEvent(c(input$SignUpName, input$SignUpSurname, input$SignUpAddress, input$SignUpCity,
                  input$SignUpCountry, input$SignUpEmso, input$SignUpMail, 
                 input$SignUpUserName, input$SignUpPassword), {
                   shinyjs::toggleState("signup_btnSignUp",
                                        all(c(input$SignUpName, input$SignUpSurname, input$SignUpAddress, input$SignUpCity,
                                              input$SignUpCountry, input$SignUpEmso, input$SignUpMail, 
                                              input$SignUpUserName, input$SignUpPassword)!=""))
                 })
  
  # Sign up protocol
  observeEvent(input$signup_btnSignUp,
               {
                 success <- sign.up.user(input$SignUpName, input$SignUpSurname, input$SignUpAddress, 
                             input$SignUpCity, input$SignUpCountry, input$SignUpEmso,
                             input$SignUpMail, input$SignUpUserName, input$SignUpPassword)
                if(success==1){
                  showModal(modalDialog(
                    title = "You have successfully signed up!",
                    paste0("Now you can login as ",input$SignUpUserName,''),
                    easyClose = TRUE,
                    footer = NULL
                  ))
                  output$signUpBOOL <- eventReactive(input$signup_btnSignUp, 0) 
                }else if(success==-10){
                  showModal(modalDialog(
                    title = "Username conflict!",
                    paste0("The username ",input$SignUpUserName,' is already taken. Please,
                           chose a new one.'),
                    easyClose = TRUE,
                    footer = NULL
                  ))
                }else{
                 showModal(modalDialog(
                   title = "Error during sign up",
                   paste0("An error seems to have occured. Please try again."),
                   easyClose = TRUE,
                   footer = NULL
                 ))
                }
               })
  
  # Back button to sign in page
  observeEvent(input$signup_btnBack, output$signUpBOOL <- eventReactive(input$signup_btnBack, 0))
  
  # Seznam razpolozljivih mack, pridobljen iz baze
  macke <- reactive({
    pridobi.imena.mack()
  })  
  output$mackeSeznam <- renderUI({
    # To omogoci, da se ta select prikaze na ui, s tem, da so podatki iz baze
    selectInput("exchangeCat",
                label ="Cat",
                choices=macke(),
                selected = 2, multiple = FALSE)
  })
  
  #numbers <- reactive({
   # validate(
    #  need(is.integer(input$exchangeBuyQuantityInput), "The quantity needs to be an integer!")
    #)
  #})
  
  # Stanje v denarnici
  output$walletStatusFiat <- renderText({"10"})
  
  # Deposit/Withdrawal funkcije
  
}
)


