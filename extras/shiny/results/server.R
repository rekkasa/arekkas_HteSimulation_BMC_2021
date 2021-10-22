shiny::shinyServer(
  function(
    input,
    output,
    session
  ) {
    
    selectedVals                       <- shiny::reactiveValues()
    selectedVals$base                  <- "absent"
    selectedVals$type                  <- "constant"
    selectedVals$nPatients             <- 4250
    selectedVals$predictionPerformance <- .75
    selectedVals$harm                  <- "absent"
    
    observeEvent(
      input$base, 
      {
        req(
          input$type, 
          input$nPatients, 
          input$predictionPerformance,
          input$harm
        )
        
        selectedVals$type                  <- input$type
        selectedVals$nPatients             <- input$nPatients
        selectedVals$predictionPerformance <- input$predictionPerformance
        selectedVals$harm                  <- input$harm
      }
    )
    
    
    
    output$typeInput <- shiny::renderUI(
      {
        if (input$base != "interaction") {
          shiny::selectInput(
            inputId  = "type",
            label    = "Type",
            choices  = c(
              "constant",
              "linear-moderate",
              "linear-high",
              "quadratic-moderate",
              "quadratic-high",
              "non-monotonic"
            ),
            selected = selectedVals$effect
          )
        } else {
          shiny::selectInput(
            inputId  = "effect",
            label    = "Effect",
            choices  = c(
              "weak",
              "strong",
              "mixed"
            ),
            selected = "weak"
          )
        }
      }
    )
    
    output$toggleNPatients <- shiny::renderUI(
      {
        if (input$base != "interaction") {
          shiny::selectInput(
            inputId  = "nPatients",
            label    = "Number of patients",
            choices  = c(
              1063,
              4250,
              17000
            ),
            selected = selectedVals$nPatients
          )
        }
      }
    )
    
    output$togglePredictionPerformance <- shiny::renderUI(
      {
        if (input$base != "interaction") {
          shiny::selectInput(
            inputId  = "predictionPerformance",
            label    = "Prediction model AUC",
            choices  = c(
              .65,
              .75,
              .85
            ),
            selected = selectedVals$predictionPerformance
          )
        }
      }
    )
    
    output$toggleHarmInput <- shiny::renderUI(
      {
        if (input$base != "interaction") {
          shiny::selectInput(
            inputId  = "harm",
            label    = "Constant harm",
            choices  = c(
              "absent",
              "positive",
              "negative"
            ),
            selected = selectedVals$harm
          )
        }
      }
    )
    
    currentScenario <- reactive(
      {
        if (input$base != "interaction") {
          analysisIds %>%
            filter(
              base       == input$base,
              type       == input$type,
              sampleSize == input$nPatients,
              auc        == input$predictionPerformance,
              harm       == input$harm
            ) %>%
            pull(scenario)
        } else {
          analysisIds %>%
            filter(effectSize == input$effect) %>%
            pull(scenario)
        }
      }
    )
    
    rmseSubset <- shiny::reactive(
      {
        rmse %>%
          filter(scenarioId == currentScenario())
      }
    )
    
    discriminationSubset <- shiny::reactive(
      {
        discrimination %>%
          filter(scenarioId == currentScenario())
      }
    )
    
    calibrationSubset <- shiny::reactive(
      {
        calibration %>%
          filter(scenarioId == currentScenario())
      }
    )
    
    
    output$rmsePlot <- plotly::renderPlotly(
      {
        tmp <- rmseSubset()
        tmp %>%
          select(-scenarioId) %>%
          createPlot2()
          # plotly::layout(
          #   yaxis = list(
          #     range = c(0, .15)
          #   )
          # )
      }
    )
    
    output$discriminationPlot <- plotly::renderPlotly(
      {
        tmp <- discriminationSubset()
        tmp %>%
          select(-scenarioId) %>%
          createPlot2() %>%
          plotly::layout(
            yaxis = list(
              range = c(.45, .65)
            )
          )
      }
    )
    
    output$calibrationPlot <- plotly::renderPlotly(
      {
        tmp <- calibrationSubset()
        tmp %>%
          select(-scenarioId) %>%
          createPlot2() %>%
          plotly::layout(
            yaxis = list(
              range = c(0, .2)
            )
          )
      }
    )
  })