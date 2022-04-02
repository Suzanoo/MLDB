library(tidyverse)
library(shiny)
library(shinyjs)
library(shinyalert)
library(GGally)
library(scales)
library(bs4Dash)
library(fresh)
library(ISLR2)

## to prevent cross over from old runs
rm(list = ls(), envir = globalenv()) 
source('ui.R')
source('utils.R')
attach(Smarket)

### Guides to speed up app and reduce used memory
# https://appsilon.com/r-shiny-faster-updateinput-css-javascript/
# We use JavaScript allows for keeping an action’s logic inside the browser
# rather than sending the action trigger to the server and slowing down the app.
# JavaScript can use instead of try-except block for reduce memory use
# ex.use Javascript observe event on button click

# We use input-ui type and update it in server instead of outputUI for speed up app
###---------------------------------------------------------------------------------
server <- function(input, output, session){
  ### RAW MATERIALS
  #upload file
  data <- reactive({
    file <- input$f_input
    data <- if(!is.null(file)){
        read.csv(file$datapath)
      }else{
        Smarket #sample 
      }
    data %>%
      mutate(id = row_number(),
             across(where(is.character),
                    as.factor))
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
    shinyjs::toggleState("button2", !is.null(formula) && input$formula != "")
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

  ## User defined formula ex. y ~ X1 + X2 and we get it as "y ~ X1 + X2".
  ## We need retrieve "y ~ X1 + X2" to list of characters [y, X1, X2, ...] 
  var <- eventReactive(input$button2, {
    req(formula())
    words <- names(data())
    pattern_words  <- paste0('\\b', words, '\\b', collapse = "|")
    
    #extract Xi from formula
    input$formula %>% 
      str_match_all(pattern_words)%>%
      flatten_chr() %>%
      unique() # [y, X1, X2, ...]
  })

  # update input
  observeEvent(input$button2, {
    req(formula())
    label <- input$label
    i <- input$data_split
    formula <- formula()

    updateRadioButtons(session, 'label', selected = label)
    updateSliderInput(session, 'data_split', value = i)
    updateTextInput(session, 'formula', value = formula)
  })

  #initial split data
  i_split <- reactive({
    init(data(), input$data_split)
  })
  
  ## calculate and listening button2 click
  model <-eventReactive(input$button2, {
    req(formula())
    
    #index train data by id
    subset <- rsample::training(i_split())$id
    
    #fit model
    model <- model_fit(input$label, formula() , data(), subset )
    
    if(is.null(model)){
      model = NULL
    }else{
      model = model
    }
  })
  
  ## calculate and listening button2 click
  summary <- eventReactive(input$button2, {
    if(!is.null(model())){
      summ(input$label, model())
    }
  })
  
  # render on click button2
  output$summary <- renderPrint({
    summary()
  })
  
  ###---------------------------------------------------------------------------------
  ### VARIABLE PREPROCESS
  ## list of y_test
  y_test <- reactive({
    req(var())
    #retrieve response(y) from formula
    y_test_var <- var()[1]
    
    # pull response(y_test) from test data
    rsample::testing(i_split()) %>%
      select(matches(y_test_var)) %>%
      pull() #[y]
    
  })
  
  ## list of y_predict
  y_pred <- reactive({
    req(model())
    new_data <- rsample::testing(i_split())
    model_predict(input$label, model(), new_data, y_test())
    
  })
  
  ###---------------------------------------------------------------------------------
  ### CONFUSION MATRIX
  # calculate and listening button2 click
  conf <- eventReactive(input$button2, {
    if(!is.null(model())){
      cfm <- caret::confusionMatrix(y_pred(), y_test())
      ggplotConfusionMatrix(cfm)
    }else{
      shinyjs::delay(500, alert("Try Again"))
    }
  })
  
  # render on click button2
  output$conf.matrix <- renderPlot({
    req(formula())
    conf()
  })

  ###---------------------------------------------------------------------------------
  ### PREDICTORS INPUT PROCESS
  # we have [y, X1, X2, ...] from var().
  # we will create input box of X1, X2, X3, ... for prediction
  Xi <- reactive({
    req(var())
    words <- names(data())
    var <- var() # [y, X1, X2, ...]
    #cut response y
    if (length(var) == 1){ #if formula (y ~ .)
      words[!(words %in% var)] #[X1, X2, ...]
    }else{
      var[2:length(var)] #[X1, X2, ...]
    }
  })
  
  #create dynamic predictors textInput UI
  pred_input <- eventReactive(input$button2, {
    if(!is.null(model())){
      Xi <- Xi() #[X1, X2, ...]
      list <- list()
      for (i in c(1:length(Xi))){
        list[[i]] <- textInput(paste0('id', i),
                               Xi[i],
                               placeholder = Xi[i])
      }
      list
    }
    
  })
  
  # render on click button2
  output$predictors <- renderUI(
    div(id = 'Xi', pred_input())
  )
  
  # get values of all predictors Xi from user input
  x <- eventReactive(input$button4, {
    value <- list()
    value <- map(1:length(pred_input()), function(i){
      # check valid "numeric" -- invalid "char"
      if(!is.na(as.numeric(input[[paste0("id",i)]]))){
        value[[i]] = as.numeric(input[[paste0("id",i)]])
      }else{
        value[[i]] = NA
      }
    })
    value
 
  })
  
  ###---------------------------------------------------------------------------------
  ### PREDICT NEW DATA
  # user cannot press button if the variable in box is NULL or blank
  observe({
    # input$id1, input$id2,... from pred_input() we use input$id1 to control
    shinyjs::toggleState("button4", !is.null(input$id1) && input$id1 != "")
  })
  
  # If formula change so clear all output
  onclick('button2', reset('predict_result'))
  
  value <- eventReactive(input$button4, {
    print(is.null(x()))
    
    if(!anyNA(x())){
      label <- input$label
      model <- model()
      new_data <- x() %>% 
        set_names(Xi()) %>% 
        as.data.frame() 
      model_predict(label, model, new_data, y_test())
      
    }else{
      shinyjs::alert("Wrong Input")
    }
  })
  
  output$predict_result <- renderValueBox({
    if(!anyNA(x())){
      value <- value()
      valueBox(
        subtitle = 'Predict Result',
        value = value,
        color = "success",
        icon = icon("cogs"),
      )
    }else{
      valueBox(
        subtitle = 'Wrong Input',
        value = "Error",
        color = "danger",
        icon = icon("cogs"),
      )
    }
  })

  ##---------------------------------------------------------------------------------
  ## 2-Dimension GRAPH
  # render scatter plot
  # we only render for 2 variables
  # calculate and listening button2 click
  plt <- eventReactive(input$button2, {
    if(!is.null(model()) & length(Xi()) == 2){
      model_plot(input$label,
                 formula(),
                 Xi(),
                 rsample::testing(i_split()),
                 model(),
                 y_test(),
                 y_pred())
    }else{
      NULL
    }
    
  })

  output$graph  <- renderPlot({
    plt()
  })
  
}
##---------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)






