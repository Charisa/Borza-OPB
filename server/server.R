library(dbplyr)
#source("auth.R")

source("../auth.R")
sign.up.user <- function(name, surname, address, city, country, emso, mail, username, pass){
  # Return values:
  # 1 ... success
  # 0 ... error
  # -10 ... username exists
  success <- 0      # Boolean, if the insertion into the db was successful
  
  useraccount <- data.frame(name, surname, address, city, country, emso, mail, username, password=pass)
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    userTable <- tbl(conn, "useraccount")
    # Checks if username is already in the database
    if(0 != dim((userTable %>% filter(username == username) %>% collect()))[1]){
      success <- -10
    }
    dbWriteTable(conn,name="useraccount", useraccount, append=TRUE, row.names = FALSE)
    success <- 1
  
  }, finally = {
    dbDisconnect(conn)
    return(success)
  })
}

sign.in.user <- function(username, pass){
  # Return a list. In the first place is an indicator of success:
  # 1 ... success
  # 0 ... error
  # -10 ... wrong username
  # The second place represents the userid if the login info is correct,
  # otherwise it's NULL
  success <- 0
  uporabnikID <- NULL
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    userTable <- tbl(conn, "useraccount")
    # obstoj = 0, ce username in geslo ne obstajata,  1 ce obstaja
    obstoj <- dim((userTable %>% filter(username == username, password == pass) %>% collect()))[1]
    if(obstoj == 0){
      success <- -10
    }else{
        uporabnikID <- (userTable %>% filter(username == username, password == pass) %>%
                        select(userid) %>% collect())[[1]]
        success <- 1
    }
  },warning = function(w){
    print(w)
  },error = function(e){
    print(e)
  }, finally = {
    dbDisconnect(conn)
    return(list(success, uporabnikID))
  })
}

pridobi.imena.mack <- function(){
  # Pridobi imena mack iz baze in jih vrne
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    sqlInput<- paste("SELECT DISTINCT breed FROM cat")
    
    macke <- dbGetQuery(conn, sqlInput)
  },finally = {
    dbDisconnect(conn)
    return(unname(unlist(macke)))
  }
  )
}

execute.buy.order <- function(cat, price, quantity, userID){
  tryCatch({
      drv <- dbDriver("PostgreSQL")
      conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
      userTable <- tbl(conn, "useraccount")
      #TODO
    },warning = function(w){
      print(w)
    },error = function(e){
      print(e)
    }, finally = {
      dbDisconnect(conn)
      #TODO
  })
}
  
#drv <- dbDriver("PostgreSQL")
#conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
#dbDisconnect(conn)
shinyServer(function(input, output){
  source("../auth.R")
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
}
)

#sign.up.user("name", "surname", "addres", "city", "country", "emso", "mail", "username6","password")

