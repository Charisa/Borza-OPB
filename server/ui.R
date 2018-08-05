library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgreSQL)
library(bcrypt)
library(DT)


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
                                         textInput("SignUpName","* Name", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpSurname","* Surname", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpAddress","* Address", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpCity","* City", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpCountry","* Country", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpEmso","* Social ID", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpMail","* eMail", value= "", placeholder = "Only Latin characters."),
                                         textInput("SignUpUserName","* Username", value= "", placeholder = "Only Latin characters."),
                                         passwordInput("SignUpPassword","* Password", value= "", placeholder = "Only Latin characters."),
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
              menuItem("Wallet",tabName = "wallet", icon=icon("usd"))),
  sidebarMenu(id="historyId", 
              menuItem("Transaction history", tabName = "history", icon=icon("calendar")))
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
                                     numericInput("exchangeBuyQuantityInput", label="Quantity",
                                                  min = 0, value = 0, step = 1), 
                                     actionButton("execute_btnBuy", "Buy")
                                     ),
                            bsModal("exchangeBuyModal", "Buy cats",
                                    "execute_btnBuy", size = "small",
                                    verbatimTextOutput("exchangeTotalModal"),
                                    actionButton("execute_btnBuyConfirm", "Confirm Buy order")
                            ),
                            tabPanel("Sell",
                                     numericInput("exchangeSellPriceInput", label="Price",
                                                  min = 0, value = 0),
                                     numericInput("exchangeSellQuantityInput", label="Quantity",
                                                  min = 0, value = 0),
                                     actionButton("execute_btnSell", "Sell")
                                     )
                
              )), column(width=6,
                         uiOutput("mackeCene")
                         )
            )),
    # Denarnica
    tabItem(tabName = "wallet",
            fluidRow(
              # Stanje denarnice in moznost nalaganja novih sredstev
              column(width = 8,
                     tabBox(id = "walletStatus", title = "Wallet status", width = 12,
                            tabPanel("Funds",
                                     verbatimTextOutput("walletStatusFiat"),
                                     actionButton("execute_btnWithdrawal", "Withdraw"),
                                     # Popup za withdrawal
                                     bsModal("walletWithdrawModal", "Withdrawal of funds",
                                             "execute_btnWithdrawal", size = "small",
                                              verbatimTextOutput("walletStatusFiatModal1"),
                                              numericInput("walletWithdrawalInput", label = "Amount",
                                                           min = 0, value = 0), 
                                              actionButton("execute_btnWithdrawalModal", "Withdraw")
                                             ),
                                     actionButton("execute_btnDeposit", "Deposit"),
                                     # Popup za deposit
                                     bsModal("walletDepositModal", "Deposit of funds",
                                             "execute_btnDeposit", size = "small",
                                             verbatimTextOutput("walletStatusFiatModal2"),
                                             numericInput("walletDepositInput", label = "Amount",
                                                          min = 0, value = 0), 
                                             actionButton("execute_btnDepositModal", "Deposit")
                                     )
                                     )

                     )

              )
            )),
    # Zgodovina
    tabItem(tabName = "history",
                     uiOutput("historyTable"),
            actionButton("refreshHistory", "Refresh")
            )
  )
)
fluidPage(useShinyjs(),
          # dashboardHeader(title = "Borza mack",
          #                 tags$li(class = "dropdown",
          #                         tags$li(class = "dropdown", textOutput("dashboardLoggedUser"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
          #                         tags$li(class = "dropdown", actionLink("dashboardLogin", textOutput("logintext")))
          #                 )),
  conditionalPanel(condition = "output.signUpBOOL!='1' && output.signUpBOOL!='2'",#&& false", 
                   vpisniPanel),       # UI panel za vpis
  conditionalPanel(condition = "output.signUpBOOL=='1'", registracijaPanel),  # UI panel registracija
  # TODO sprememba stanja ob spremembi na bazi oz na vsakih 10 sekund
  conditionalPanel(condition = "output.signUpBOOL=='2'",    # Panel, ko si ze vpisan
                   dashboardPage(#dashboardHeader(disable=T),
                     dashboardHeader(title = "Borza mack",
                                                 tags$li(class = "dropdown",
                                                         tags$li(class = "dropdown", textOutput("dashboardLoggedUser"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
                                                         tags$li(class = "dropdown", actionLink("dashboardLogin", textOutput("logintext")))
                                                         )),
                                 sidebar,
                                 body,
                                 skin = "blue")),
  theme="bootstrap.css"
)