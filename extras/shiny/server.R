shiny::shinyServer(
  function(
    input,
    output,
    session
  ) {
    
    selectedVals                       <- shiny::reactiveValues()
    selectedVals$effect                <- "moderate"
    selectedVals$nPatients             <- 4250
    selectedVals$predictionPerformance <- 75
    
    observeEvent(
      input$type,
      {
        req(
          input$type, 
          input$effect, 
          input$nPatients, 
          input$predictionPerformance
        )
        
        selectedVals$effect                <- input$effect
        selectedVals$nPatients             <- input$nPatients
        selectedVals$predictionPerformance <- input$predictionPerformance
      }
    )
    
    output$effectInput <- shiny::renderUI(
      {
        if (input$type != "interaction") {
          shiny::selectInput(
            inputId  = "effect",
            label    = "Effect",
            choices  = c(
              "absent",
              "moderate",
              "high"
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
        if (input$type != "interaction") {
          shiny::selectInput(
            inputId  = "nPatients",
            label    = "Number of patients",
            choices  = c(
              1064,
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
        if (input$type != "interaction") {
          shiny::selectInput(
            inputId  = "predictionPerformance",
            label    = "Prediction model AUC",
            choices  = c(
              65,
              75,
              85
            ),
            selected = selectedVals$predictionPerformance
          )
        }
      }
    )
    
    currentScenario <- reactive(
      {
        if (input$type != "interaction") {
          analysisIds %>%
            filter(
              type       == input$type,
              effectSize == input$effect,
              sampleSize == input$nPatients,
              auc        == input$predictionPerformance
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
          createPlot2() %>%
          plotly::layout(
            yaxis = list(
              range = c(0, .15)
            )
          )
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