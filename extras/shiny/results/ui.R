shiny::shinyUI(
  shinydashboardPlus::dashboardPage(
    skin                 = "black",
    title                = "Simulations",
    shinydashboard::dashboardHeader(
      title = "Simulations"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "menu1",
        shinydashboard::menuItem(
          tabName = "home",
          text    = "Home",
          icon    = icon("home")
        ),
        shinydashboard::menuItem(
          tabName = "settings",
          text    = "Settings",
          icon    = icon("cogs")
        )
      ),
      shinydashboard::sidebarMenu(
        id = "menu2",
        shiny::conditionalPanel(
          condition = "input.menu1 == 'settings'",
          shiny::selectInput(
            inputId  = "base",
            label    = "Base",
            choices  = c(
              "absent",
              "moderate",
              "high",
              "interaction"
            ),
            selected = "absent"
          ),
          shiny::uiOutput("typeInput"),
          shiny::uiOutput("toggleNPatients"),
          shiny::uiOutput("togglePredictionPerformance"),
          shiny::selectInput(
            inputId  = "harm",
            label    = "Constant harm",
            choices  = c(
              "absent",
              "moderate-positive",
              "strong-positive",
              "negative"
            ),
            selected = "absent"
          )
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "home",
          shiny::includeHTML(
            "html/abstract.html"
          )
        ),
        shinydashboard::tabItem(
          tabName = "settings",
          shiny::tabsetPanel(
            id = "results",
            shiny::tabPanel(
              title = "Description"
            ),
            shiny::tabPanel(
              title = "RMSE",
              plotly::plotlyOutput(
                "rmsePlot",
                height = "600px",
                width = "1000px"
              )
            ),
            shiny::tabPanel(
              title = "Discrimination",
              plotly::plotlyOutput(
                "discriminationPlot",
                height = "600px",
                width = "1000px"
              )
            ),
            shiny::tabPanel(
              title = "Calibration",
              plotly::plotlyOutput(
                "calibrationPlot",
                height = "600px",
                width = "1000px"
              )
            )
          )
        )
      )
    )
  )
)
