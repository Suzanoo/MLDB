library(shiny)
library(shinydashboard)
library(tidyverse)
library(GGally)
library(shinyjs)
library(bs4Dash)
library(ISLR2)
attach(Boston)

## to prevent cross over from old runs
rm(list = ls(), envir = globalenv()) 
shinyjs::useShinyjs()
source('ui.R')
source('utils.R')

#---------------------------------------------------
server <- function(input, output, session){
  #upload csv file
  data <- reactive({
    file <- input$f_input
    data <- if(!is.null(file)){
      read.csv(file$datapath)
    }else{
      Boston #sample 
    }
    data %>%
      mutate(id = row_number())
  })

  #render RAW table
  output$view_table <- DT::renderDataTable(
    DT::datatable(data(),
                  options = (list(scrollX = TRUE))
    )
  )
  ###---------------------------------------------------------------------------------
  ### CORRELATION PLOT
  # user cannot press button if the variable in box is NULL or blank
  # in other word app cannot render graph(Error was managed) until user click
  observe({
    shinyjs::toggleState("button1", !is.null(input$var) && input$var != "")
  })
  
  # reset input to initial(NULL) if new file have been upload .
  # app cannot render graph(Error was managed)
  observeEvent(input$f_input, {
    shinyjs::reset("var")
    shinyjs::reset("formula")
  })
  
  # We use input-ui type and update it in server instead of outputUI for speed up app
  observeEvent(input$f_input, {
    req(data())
    CHOICES = names(data())
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = 'var',
      label = 'Select to generated correlation matrix',
      choices = CHOICES,
      options = list(`actions-box` = TRUE)
    )
  })
  
  # calculate and listening button1 click
  corr_plot <- eventReactive(input$button1, {
    req(data())
    corr_(data() %>%
            select(matches(input$var)), input$corr)
  })
  
  # render plot
  output$corr_plot <- renderPlot({
    corr_plot()
  })
  
  ###---------------------------------------------------------------------------------
  ### FIT MODEL
  observe({
    shinyjs::toggleState("button2", !is.null(input$formula) && input$formula != "")
  })
  onclick('button3', reset('formula'))
  
  # switch to tab Order66 when click run model
  observeEvent(input$button2, {
    updateTabsetPanel(session, "tabs",
                      selected = "ds")
  })
  
  ## input filter
  # check formula input
  formula <- eventReactive(input$button2, {
    valid_formula(data(), input$formula)
  })
  
  # update input
  observeEvent(input$button2, {
    req(formula())
    i <- input$data_split
    formula <- formula()
    
    updateSliderInput(session, 'data_split', value = i)
    updateTextInput(session, 'formula', value = formula)
  })
  
  #initial split data
  i_split <- reactive({
    init(data(), input$data_split)
  })
 
  #create model
  model <- eventReactive(input$button2, {
    req(formula())
    
    #index train data by id
    subset <- rsample::training(i_split())$id
    model <- model_fit(formula(), data(), subset)
    
    if(is.null(model)){
      model = NULL
    }else{
      model = model
    }
    model
  })
  
  summ <- eventReactive(input$button2, {
    req(model())
    summary(model())
  })
  
  # render on click button2
  output$summary <- renderPrint({
    req(model())
    summ()
  })
  
  ###---------------------------------------------------------------------------------
  ### PREDICTORS INPUT PROCESS
  ## User defined formula ex. y ~ X1 + X2 and we get it as "y ~ X1 + X2".
  ## We need retrieve "y ~ X1 + X2" to list of characters [y, X1, X2, ...] 
  var <- eventReactive(input$button2, {
    req(formula())
    variables(data(), formula())
  })

  # we have [y, X1, X2, ...] from var().
  # we get rid off outcome y ,and
  # we will create input box of X1, X2, X3, ... for prediction
  Xi <- reactive({
    req(var())
    predictors(data(), formula())
  })
  
  # create UI of predictors input
  pred_input <- eventReactive(input$button2, {
    req(model())
    if(!is.null(model())){
      req(Xi())
      a <- Xi()
      list <- list()
      for (i in c(1:length(a))){
        list[[i]] <- textInput(paste0('id', i),
                               a[i],
                               placeholder = a[i])
      }
      list
    }else{
      NULL
    }
    
  })

  # render on click button2
  output$predictors <- renderUI(
    div(id = 'Xi', pred_input())
  )
  
  # get values from UI of predictors input
  x <- eventReactive(input$button4, {
    req(pred_input())
    a <- pred_input()
    value <- list()
    value <- map(1:length(a), function(i){
      # check valid "numeric" -- invalid "char"
      if(!is.na(as.numeric(input[[paste0("id",i)]]))){
        value[[i]] = as.numeric(input[[paste0("id",i)]])
      }else{
        value[[i]] = NA
      }
    })
    value
  })
 
  ##--------------------------
  ### PREDICT NEW DATA
  observe({
    # input$id1, input$id2,... from pred_input() we use input$id1 to control
    shinyjs::toggleState("button4", !is.null(input$id1) && input$id1 != "")
  })
  onclick('button2', reset('predict_result'))
  
  #calculate y_pred
  y_pred <-  eventReactive(input$button4, {
    req(x())
    if(!anyNA(x())){
      outcome(model(), x(), Xi())
    }else{
      shinyjs::alert("Wrong Input")
    }
  })
  
  # Render value box
  output$predict_result <- renderValueBox({
    if(!anyNA(x())){
      value <- sprintf(y_pred(), fmt = '%#.4f')[1]
      valueBox(
        subtitle = 'Predict Result',
        value = value,
        color = "success",
        icon = icon("check-circle"),
      )
    }else{
      valueBox(
        subtitle = 'Wrong Input',
        value = "Error",
        color = "warning",
        icon = icon("times"),
      )
    }
  })
  
  ##--------------------------
  #coeff plot
  coeff_plt <- eventReactive(input$button2, {
    req(model())
    if(!is.null(model())){
      model() %>%
        broom::tidy() %>%
        ggplot(aes(x = term, y = estimate, color = p.value <= 0.05)) +
        geom_segment(aes(xend = term, yend = 0)) +
        geom_point() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste0("Coefficients for predicting "),
             subtitle = paste0('Formula : ', input$formula))
    }else{
      shinyjs::delay(500, alert("Try Again!"))
    }
  })
  
  output$explore_plot <- renderPlot({
    coeff_plt()
  })

  #residual plot
  resid_plt <- eventReactive(input$button2, {
    if(!is.null(model())){
      train_data <- rsample::training(i_split())
      lm.fit <- model()
      train_data %>%
        mutate(
          fitted.values = pluck(lm.fit$fitted.values),
          residuals = pluck(lm.fit$residuals)
        ) %>%
        as_data_frame() %>%
        ggplot(aes(fitted.values, residuals, color = residuals))+
        geom_point()+
        geom_smooth(color="green")+
        geom_hline(yintercept=0, linetype="dashed", color="blue")+
        labs(x = "Fitted Values", y = "Residual")
    }else{
      shinyjs::delay(500, alert("Try Again!"))
    }
  })
  
  output$model_plot <- renderPlot({
    resid_plt()
  })
}

shinyApp(ui = ui, server = server)