library(shinydashboard)

sidebar <- dashboardSidebar(hr(),
  sidebarMenu(id="tabs",
              menuItem("User", tabName = "user", icon=icon("line-chart"), selected = TRUE)),
  conditionalPanel(condition = "output.signUpBOOL!='1'",sidebarMenu(id="test",
              menuItem("tst2",tabName = "Test")))
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "user",
            fluidRow(
              column(width = 10,
                     #conditionalPanel(condition = "output.signUpBOOL!='1'", verbatimTextOutput("signUpBOOL")),  # ker shiny cudn
                     conditionalPanel(condition = "output.signUpBOOL!='1'",
                        textInput("userName","User name", value= ""),
                        textInput("password","Password", value = ""),
                        actionButton("signin_btn", "Sign In"),actionButton("signup_btn", "Sign Up"))),
              
              column(width = 12,
                     conditionalPanel(condition = "output.signUpBOOL=='1'",
                     textInput("nameSignUp","Name", value= ""),
                     textInput("surnameSignUp","Surname", value= ""),
                     textInput("addressSignUp","Address", value= ""),
                     textInput("citySignUp","City", value= ""),
                     textInput("countrySignUp","Country", value= ""),
                     textInput("emsoSignUp","Social ID", value= ""),
                     textInput("mailSignUp","eMail", value= ""),
                     textInput("userNameSignUp","Username", value= ""),
                     passwordInput("passwordSignUp","Password", value= ""),
                     actionButton("signup_btnSignUp", "Sign Up")
                     ))       
            ))
  )
)

dashboardPage(
  dashboardHeader(title = "Borza mack"),
  sidebar,
  body
)
