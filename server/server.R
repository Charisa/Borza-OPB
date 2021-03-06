#drv <- dbDriver("PostgreSQL")
#conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
#dbDisconnect(conn)

shinyServer(function(input, output){
  source("../auth_public.R")
  source("serverFunctions.R")
  
  drv <- dbDriver("PostgreSQL")
  
  userID <- reactiveVal()    # Placeholder za userID
  loggedIn <- reactiveVal(FALSE)    # Placeholder za logout gumb oz vrednost gumba
  
  # Gumb, ce se hoce uporabnik registrirat
  output$signUpBOOL <- eventReactive(input$signup_btn, 1)
  outputOptions(output, 'signUpBOOL', suspendWhenHidden=FALSE)  # Da omogoca skrivanje/odkrivanje
  observeEvent(input$signup_btn, output$signUpBOOL <- eventReactive(input$signup_btn, 1))

  # Greyout of signin button
  observeEvent(c(input$userName,input$password), {
    shinyjs::toggleState("signin_btn", 
                         all(c(input$userName, input$password)!=""))
  })
  
  # Sign in protocol
  observeEvent(input$signin_btn,
               {signInReturn <- sign.in.user(input$userName, input$password)
               if(signInReturn[[1]]==1){
                 userID(signInReturn[[2]])
                 output$signUpBOOL <- eventReactive(input$signin_btn, 2)
                 loggedIn(TRUE)
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
                                              input$SignUpUserName, input$SignUpPassword)!="")# & 
                                          # Preveri, ce samo latin characterji
                                        # !any(grepl("[^\x20-\x7F]",
                                        #         c(input$SignUpName, input$SignUpSurname, input$SignUpAddress, input$SignUpCity,
                                        #           input$SignUpCountry, input$SignUpEmso, input$SignUpMail, 
                                        #           input$SignUpUserName, input$SignUpPassword)))
                                        )
                 })
  
  # Sign up protocol
  observeEvent(input$signup_btnSignUp,
               {
                 if(any(grepl("[^\x20-\x7F]",
                                      c(input$SignUpName, input$SignUpSurname, input$SignUpAddress, input$SignUpCity,
                                        input$SignUpCountry, input$SignUpEmso, input$SignUpMail,
                                        input$SignUpUserName, input$SignUpPassword)))){
                   success <- -1
                 }else{
                   success <- sign.up.user(input$SignUpName, input$SignUpSurname, input$SignUpAddress, 
                               input$SignUpCity, input$SignUpCountry, input$SignUpEmso,
                               input$SignUpMail, input$SignUpUserName, input$SignUpPassword)
                 }
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
                }else if(success==-1){
                  showModal(modalDialog(
                    title = "Signup unsuccessful",
                    paste0("Only Latin characters allowed"),
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
  
  # Login/logout button in header
  observeEvent(input$dashboardLogin, {
    if(loggedIn()){
      output$signUpBOOL <- eventReactive(input$signin_btn, 0)
      userID <- reactiveVal()
    }
    loggedIn(ifelse(loggedIn(), FALSE, TRUE))
  })
  
  output$logintext <- renderText({
    if(loggedIn()) return("Logout here.")
    return("Login here")
  })
  
  output$dashboardLoggedUser <- renderText({
    if(loggedIn()) return(paste("Welcome,", pridobi.ime.uporabnika(userID())))
    return("")
  })
  
  
  # GLAVNA STRAN
  
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
  
  # Tabela cen za izbrano macko
  selectedCat <- pridobi.imena.mack()[1]
  observeEvent(input$exchangeCat, {
    # selectedCat <- input$exchangeCat
    # output$mackeCene <- renderUI({
    #   output$tabelaCenMack <- DT::renderDataTable(
    #     DT::datatable(pridobi.cene.macke(selectedCat), options = list(searching = FALSE))
    #   )
    #   DT::dataTableOutput("tabelaCenMack")
    # })
    updateCatTable()
    })
  
  # Stanje v denarnici
  updateWaletStatus <- function(ID){
    walletStatusFiatDummy <- renderText(format(ifelse(is.na(check.wallet.balance(ID)),0,check.wallet.balance(ID)), scientific = FALSE))
    
    output$walletStatusFiat <<- walletStatusFiatDummy
    output$walletStatusFiatModal1 <<- walletStatusFiatDummy
    output$walletStatusFiatModal2 <<- walletStatusFiatDummy
  }
  
  # Updajte tabele
  updateCatTable <- function(){
    selectedCat <- input$exchangeCat
    output$mackeCene <- renderUI({
      output$tabelaCenMack <- DT::renderDataTable(
        DT::datatable(pridobi.cene.macke(selectedCat), options = list(searching = FALSE))
      )
      DT::dataTableOutput("tabelaCenMack")
    })
  }
  
  # Updajta vsebino na vsakih 10 sek
  statusUpdateTimer <- reactiveTimer(10000)
  observe({
    statusUpdateTimer()
    
    updateWaletStatus(userID())
  })
  
  # Updajta vsebino na vsakih 60 sek
  statusUpdateTimer <- reactiveTimer(60000)
  observe({
    statusUpdateTimer()
    
    updateCatTable()
  })
  
  # Dodajanje sell orderja
  observeEvent(input$execute_btnSell,{
    status <- post.sell.order(userID(),
                              input$exchangeCat,
                              input$exchangeSellPriceInput,
                              input$exchangeSellQuantityInput)
    if(status == 1){
      showModal(modalDialog(
        title = "Sell order successful",
        paste0("Sell order was successfully posted"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else if(status == -1){
      showModal(modalDialog(
        title = "Sell order unsuccessful",
        paste0("Price has to be greater than 0"),
        easyClose = TRUE,
        footer = NULL
      ))
      updateCatTable()
    }else if(status == -2){
      showModal(modalDialog(
        title = "Sell order unsuccessful",
        paste0("Quantity has to be greater than 0"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else if(status == -3){
      showModal(modalDialog(
        title = "Sell order unsuccessful",
        paste0("Quantity has to be an integer"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      showModal(modalDialog(
        title = "Sell order unsuccessful",
        paste0("An error occurred. Please try again."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Buy order
  observeEvent(c(input$exchangeBuyQuantityInput), {
    shinyjs::toggleState("execute_btnBuyConfirm", 
                         input$exchangeBuyQuantityInput>0 & 
                           as.integer(input$exchangeBuyQuantityInput)==input$exchangeBuyQuantityInput)
  })
  
  observeEvent(input$execute_btnBuy, {
    output$exchangeTotalModal <- renderText(
      as.character(
        ifelse(check.total.price(input$exchangeCat, input$exchangeBuyQuantityInput)!=FALSE,
               # Preverimo se, ce quantity je integer
               ifelse(as.integer(input$exchangeBuyQuantityInput)==input$exchangeBuyQuantityInput,
               check.total.price(input$exchangeCat, input$exchangeBuyQuantityInput),
               "Quantity has to be an integer"),
               "Not enough cats on sale")))
  })
  
  observeEvent(input$execute_btnBuyConfirm, {
    if(is.numeric(check.total.price(input$exchangeCat, input$exchangeBuyQuantityInput))){
      cena <- check.total.price(input$exchangeCat, input$exchangeBuyQuantityInput)
      stanje <- check.wallet.balance(userID())
      stanje <- ifelse(is.na(stanje),0, stanje)
      
      if(cena<=stanje){
        status <- execute.buy.order(userID_buyer = userID(),cat = input$exchangeCat, 
                                    quantity = input$exchangeBuyQuantityInput)
      }else{
        status <- -1
        }
    }else{
      status <- 0
    }
      
    if(status==1){
      showModal(modalDialog(
        title = "Success",
        paste0("You have successfully bought ",input$exchangeBuyQuantityInput," ",input$exchangeCat," cats"),
        easyClose = TRUE,
        footer = NULL
      ))
      updateCatTable()
    }else if(status==-1){
      showModal(modalDialog(
        title = "Failure",
        paste0("You have insufficient funds"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else if(status==2){
      showModal(modalDialog(
        title = "Failure",
        paste0("There are not enough cats on sale"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      showModal(modalDialog(
        title = "Error",
        paste0("There seems to have been an error. Please try again."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
      
  })
  
  # WALLET
  # Deposit/Withdrawal funkcije
  observeEvent(input$execute_btnWithdrawalModal,{
    withdrawalQuantity <- input$walletWithdrawalInput
    if((is.numeric(withdrawalQuantity) | is.integer(withdrawalQuantity)) && withdrawalQuantity>0){
      if(withdrawalQuantity <= 10000000000000)
        status <- user.change.balance(userID(), withdrawalQuantity, "withdraw")
      else{
        status <- "overflow"
      }
    }else{
      status <- "error"
    }
    if(status == TRUE){
      showModal(modalDialog(
        title = "Withdrawal successful",
        paste0("Withdrawal was successful."),
        easyClose = TRUE,
        footer = NULL
      ))
      updateWaletStatus(userID())
    }else if(status == FALSE){
      showModal(modalDialog(
        title = "Withdrawal unsuccessful",
        paste0("Not enough funds"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else if(status == "overflow"){
      showModal(modalDialog(
        title = "Withdrawal unsuccessful",
        paste0("Withdrawal is limited to $10000000000000"),
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      showModal(modalDialog(
        title = "Withdrawal unsuccessful",
        paste0("An error has occurred. Please try again later."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$execute_btnDepositModal,{
    depositQuantity <- input$walletWithdrawalInput
    if((is.numeric(depositQuantity) | is.integer(depositQuantity)) && depositQuantity>0){
      if(depositQuantity <= 10000000000000)
        status <- user.change.balance(userID(), depositQuantity, "deposit")
      else{
        status <- "overflow"
      }
    }else{
      status <- "error"
    }
    if(status == TRUE){
        showModal(modalDialog(
          title = "Deposit successful",
          paste0("Deposit was successful."),
          easyClose = TRUE,
          footer = NULL
        ))
        updateWaletStatus(userID())
      }else if(status == "overflow"){
        showModal(modalDialog(
          title = "Deposit unsuccessful",
          paste0("Deposit is limited to $10000000000000"),
          easyClose = TRUE,
          footer = NULL
        ))
      }else{
        showModal(modalDialog(
          title = "Deposit unsuccessful",
          paste0("An error has occurred. Please try again later."),
          easyClose = TRUE,
          footer = NULL
        ))
      }
  })
  
  # HISTORY
  # Tabela zgodovine
  output$historyTable <- renderUI({
    output$tabelaZgodovine <- renderDataTable({
      pridobi.zgodovino.transakcij(userID())
    })
    dataTableOutput("tabelaZgodovine")
  })
  observeEvent(input$refreshHistory,{
    output$historyTable <- renderUI({
      output$tabelaZgodovine <- renderDataTable({
        pridobi.zgodovino.transakcij(userID())
      })
      dataTableOutput("tabelaZgodovine")
    })
  })
}
)


