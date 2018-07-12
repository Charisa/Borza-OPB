library(shiny)
library(shinydashboard)
library(shinyjs)


# TODO Dodaj css/html za oblikovanje

vpisniPanel <- tabPanel("SignIn", value="signIn",
                         fluidPage(
                           fluidRow(
                             column(width = 12,
                                    align = "center",
                                    #conditionalPanel(condition = "output.signUpBOOL!='1'", verbatimTextOutput("signUpBOOL")),  # ker shiny cudn
                                    textInput("userName","User name", value= ""),
                                    passwordInput("password","Password", value = ""),
                                    actionButton("signin_btn", "Sign In"),
                                    actionButton("signup_btn", "Sign Up"))
                             # tags$style(type="text/css", "#string { padding: 2000px 0;}")
                         )))

registracijaPanel <- tabPanel("SignUp", value = "signUp",
                              fluidPage(
                                fluidRow(
                                  column(width = 12,
                                         align="center",
                                         textInput("SignUpName","Name", value= ""),
                                         textInput("SignUpSurname","Surname", value= ""),
                                         textInput("SignUpAddress","Address", value= ""),
                                         textInput("SignUpCity","City", value= ""),
                                         textInput("SignUpCountry","Country", value= ""),
                                         textInput("SignUpEmso","Social ID", value= ""),
                                         textInput("SignUpMail","eMail", value= ""),
                                         textInput("SignUpUserName","Username", value= ""),
                                         passwordInput("SignUpPassword","Password", value= ""),
                                         actionButton("signup_btnBack", "Back"),
                                         actionButton("signup_btnSignUp", "Sign Up")
                                  )
                                )
                              )
                            )

# Oblika v aplikaciji


sidebar <- dashboardSidebar(hr(),
  sidebarMenu(id="exchangeId",
              menuItem("Exchange", tabName = "exchange", icon=icon("line-chart"), selected = TRUE)),
  sidebarMenu(id="walletId",
              menuItem("Wallet",tabName = "wallet", icon=icon("usd")))
)

body <- dashboardBody(
  tabItems(
    # Okno z orderbookom in narocili
    tabItem(tabName = "exchange",
            fluidRow(
              # Input za buy/sell orderje
              column(width=6,
                     uiOutput("mackeSeznam"),
                     tabBox(id = "exchangeAction", title = "Trading box", width=12,
                            tabPanel("Buy",
                                     numericInput("exchangeBuyPriceInput", label="Price",
                                                  min = 0, value = 0),   #TODO
                                     numericInput("exchangeBuyQuantityInput", label="Quantity",
                                                  min = 0, value = 0, step = 1), 
                                     actionButton("execute_btnBuy", "Execute Order")
                                     ),
                            tabPanel("Sell",
                                     numericInput("exchangeSellPriceInput", label="Price",
                                                  min = 0, value = 0),   #TODO
                                     numericInput("exchangeSellQuantityInput", label="Quantity",
                                                  min = 0, value = 0),
                                     actionButton("execute_btnSell", "Execute Order")
                                     )
                
              ))
            )),
    # Denarnica
    tabItem(tabName = "wallet",
            fluidRow(
              # Stanje denarnice in moznost nalaganja novih sredstev
              column(width = 8,
                     tabBox(id = "walletStatus", title = "Walet status", width = 12,
                            tabPanel("Funds",
                                     verbatimTextOutput("walletStatusFiat"),
                                     actionButton("execute_btnWithdrawal", "Withdraw"),
                                     bsModal("walletWithdrawModal", "Withdrawal of funds",
                                             "execute_btnWithdrawal", size = "small"),
                                     actionButton("execute_btnDeposit", "Deposit")
                                     )

                     )

              )
            ))
  )
)
fluidPage(useShinyjs(),
  conditionalPanel(condition = "output.signUpBOOL!='1' && output.signUpBOOL!='2' && false", 
                   vpisniPanel),       # UI panel za vpis
  conditionalPanel(condition = "output.signUpBOOL=='1'", registracijaPanel),  # UI panel registracija
  conditionalPanel(condition = "true",#"output.signUpBOOL=='2'",    # Panel, ko si ze vpisan
                   dashboardPage(dashboardHeader(title = "Borza mack"),
                                 sidebar,
                                 body)),
  theme="bootstrap.css"
)