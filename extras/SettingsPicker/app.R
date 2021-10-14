#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Non-monotonic settings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            numericInput(
                inputId =  "b1",
                label = "First intersection with the diagonal (logit scale)",
                value = -5,
                min = -7,
                max = 10,
                step = .01
            ),
            numericInput(
                inputId =  "b2",
                label = "Second intersection with the diagonal (bigger!!)",
                value = 0,
                min = -7,
                max = 10,
                step = .01
            ),
            numericInput(
                inputId =  "m",
                label = "Maximum distance from the diagonal within (b1, b2)",
                value = 1,
                min = -7,
                max = 10,
                step = .01
            ),
            numericInput(
                inputId =  "harm",
                label = "Constant treatment harm (probability)",
                value = 0,
                min = 0,
                max = 1,
                step = .01
            )
        ),
    
    
        # Show a plot of the generated distribution
        mainPanel(
            width = 9,
            fluidRow(
                splitLayout(
                   cellWidths = c("50%", "50%"),
                   plotOutput("distPlot"),
                   plotOutput("distPlot2")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # source("../../code/helpers/PlotGammas.R")
    
    # ---- Functions ----
    
    logit <- function(x) log(x / (1 - x))
    
    calcSquare <- function(x, g0 = 0, g1 = 0, g2 = 0, l = 0) {
        ret <- g0 + g1 * (x - l) + g2 * (x - l)**2
        return(ret)
    }
    
    # b1 -> first intersection with diagonal
    # b2 -> second intersection with diagonal
    # m  -> maximum distance from the diagonal in (b1, b2)
    # c is set to 0 !!
    calculateGammas <- function(b1, b2, m) {
        
        g2 <- -m / (b1 * b2 - (b1+b2)**2 / 2)
        g1 <- 1 - g2 * (b1 + b2)
        g0 <- -m + g2 * (b1 + b2)**2 / 2
        c  <- 0
        
        return(
            list(
                g0 = g0,
                g1 = g1,
                g2 = g2,
                c = c
            )
        )
    }
    
    plotRelativeConstant <- function(gamma, harm = 0, label = "constant", ...) {
        prob <- function(p, gamma) {
            (exp(logit(p) + gamma)) / (1 + exp(logit(p) + gamma)) + harm
        }
        
        ggplot2::stat_function(
            data = data.frame(x = c(0, .95)),
            ggplot2::aes(x = x, color = label),
            fun = prob,
            args = list(gamma = gamma),
            ...
        )
        
    }
    
    plotRelativeRiskOld <- function(gammas, harm = 0, label, ...) {
        
        g0 <- gammas$g0
        g1 <- gammas$g1
        g2 <- gammas$g2
        c  <- gammas$c
        
        calcRiskTreatment <- function(p, g0, g1, g2, c, harm) {
            logit <- function(p) log(p / (1 - p))
            f <- function(x, g0, g1, g2, c) {
                g0 + g1 * (x - c) + g2 * (x - c)**2
            }
            u <- exp(f(x = logit(p), g0 = g0, g1 = g1, g2 = g2, c = c)) 
            u / (1 + u) + harm
        }
        ggplot2::stat_function(
            data = data.frame(x = c(0, .95)),
            ggplot2::aes(x = x, color = label),
            fun = calcRiskTreatment,
            args = list(
                g0   = g0,
                g1   = g1,
                g2   = g2,
                c    = c,
                harm = harm
            ),
            ...
        )
    }
    
    plotRelativeBenefitGomp <- function(fun, harm = .01, label, ...) {
        
        ggplot2::stat_function(
            data = data.frame(x = c(0, .95)),
            ggplot2::aes(x = x, color = label),
            fun = function(x, f, harm) x^exp(-f(-log(-log(x)))) + harm,
            args = list(f = fun, harm = harm),
            ...
        )
        
    }
    
    f <- function(a, b, c) {
        return(
            function(x) a + b * (x - c)
        )
    }
    
    plotAbsoluteBenefit <- function(g0 = 0, g1 = 1, g2 = 0, l = 0, harm = 0, label) {
        ggplot2::stat_function(
            data = data.frame(x = c(.05, .95)),
            ggplot2::aes(x = x, color = label),
            fun = calcAbsoluteBenefit,
            args = list(g0 = g0, g1 = g1, g2 = g2, l = l, harm = harm)
        )
    }
    
    calcAbsoluteBenefit <- function(p, g0 = 0, g1 = 0, g2 = 0, l = 0, harm = 0) {
        x <- log(p / (1 - p))
        sq <- calcSquare(x, g0, g1, g2, l)
        benefit <- plogis(x) - plogis(sq) - harm
        return(benefit)
    }
    
    plotAbsoluteBenefitGomp <- function(fun, harm = .01, label, ...) {
        
        ggplot2::stat_function(
            data = data.frame(x = c(0, .95)),
            ggplot2::aes(x = x, color = label),
            fun = function(x, f, harm) x - (x^exp(-f(-log(-log(x)))) + harm),
            args = list(f = fun, harm = harm),
            ...
        )
        
    }
    
    # ---- End of functions ----
    
    
    analysisIds <- read_csv("analysisIds.csv")
    scenarios <- c(46, 55)
    
    gammas <- list()
    for (i in seq_along(scenarios)) {
        idSettings <- analysisIds %>%
            filter(scenario == scenarios[i])
        gammas[[i]] <- list(
            g0 = idSettings$g0,
            g1 = idSettings$g1,
            g2 = idSettings$g2,
            c  = idSettings$c
        )
    }
    
    gammasNonMonotonic <- reactive({
        calculateGammas(
        b1 = input$b1,
        b2 = input$b2,
        m  = input$m
    )
    })
    
    output$distPlot <- renderPlot({
        ggplot() +
            plotRelativeConstant(0, label = "absent", linetype = "longdash", size = 1.2) +
            plotRelativeConstant(log(.8), harm = input$harm, label = "base-case") +
            plotRelativeBenefitGomp(f(-.26, .2, -.8), harm = input$harm, label = "gompertz") +
            plotRelativeRiskOld(gammas = gammas[[1]], harm = input$harm, label = "quadratic-moderate") +
            plotRelativeRiskOld(gammas = gammas[[2]], harm = input$harm, label = "quadratic-high") +
            plotRelativeRiskOld(gammas = gammasNonMonotonic(), harm = input$harm, label = "non-monotonic") +
            xlim(0, .5) +
            theme_classic() +
            xlab("Baseline risk") +
            ylab("Risk with treatment") +
            scale_color_manual(
                values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"),
                breaks = c("absent", "base-case", "quadratic-moderate", "linear-high", "quadratic-high", "gompertz", "non-monotonic")
            ) +
            theme(
                axis.text    = element_text(size = 19),
                axis.title   = element_text(size = 21),
                legend.text  = element_text(size = 15),
                legend.title = element_blank()
            )
        
        
    })
    
    scenarios <- c(10, 28, 37, 55)
    
    gammas <- list()
    for (i in seq_along(scenarios)) {
        idSettings <- analysisIds %>%
            filter(scenario == scenarios[i])
        gammas[[i]] <- list(
            g0 = idSettings$g0,
            g1 = idSettings$g1,
            g2 = idSettings$g2,
            c  = idSettings$c
        )
    }
    
    
    output$distPlot2 <- renderPlot({
        ggplot() +
            plotAbsoluteBenefitGomp(f(-.26, .2, -.8), harm = input$harm, label = "gompertz") +
            plotAbsoluteBenefit(
                g0    = gammas[[1]]$g0,
                g1    = gammas[[1]]$g1,
                g2    = gammas[[1]]$g2,
                l     = gammas[[1]]$c,
                harm  = input$harm,
                label = "base-case"
            ) +
            plotAbsoluteBenefit(
                g0    = gammas[[2]]$g0,
                g1    = gammas[[2]]$g1,
                g2    = gammas[[2]]$g2,
                l     = gammas[[2]]$c,
                harm  = input$harm,
                label = "linear-moderate"
            ) +
            plotAbsoluteBenefit(
                g0    = gammas[[3]]$g0,
                g1    = gammas[[3]]$g1,
                g2    = gammas[[3]]$g2,
                l     = gammas[[3]]$c,
                harm  = input$harm,
                label = "linear-high"
            ) +
            plotAbsoluteBenefit(
                g0    = gammas[[4]]$g0,
                g1    = gammas[[4]]$g1,
                g2    = gammas[[4]]$g2,
                l     = gammas[[4]]$c,
                harm  = input$harm,
                label = "quadratic-high"
            ) +
            plotAbsoluteBenefit(
                g0    = gammasNonMonotonic()$g0,
                g1    = gammasNonMonotonic()$g1,
                g2    = gammasNonMonotonic()$g2,
                l     = gammasNonMonotonic()$c,
                harm  = input$harm,
                label = "non-monotonic"
            ) +
            xlim(0, .5) +
            xlab("Baseline risk") +
            ylab("Absolute benefit") +
            theme_classic() +
            scale_color_manual(
                values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"),
                breaks = c("absent", "base-case", "linear-moderate", "linear-high", "quadratic-high", "gompertz", "non-monotonic")
            ) +
            theme(
                axis.text    = element_text(size = 19),
                axis.title   = element_text(size = 21),
                legend.text  = element_text(size = 15),
                legend.title = element_blank()
            )
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
