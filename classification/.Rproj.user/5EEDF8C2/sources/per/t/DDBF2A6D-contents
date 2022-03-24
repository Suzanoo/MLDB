library(tidyverse)
library(shiny)
library(shinydashboard)
library(ISLR2)
library(GGally)
library(scales)

source('ui.R')
source('utils.R')
###------------------------------------------------------
server <- function(input, output, session){
  ### RAW MATERIALS
  #upload csv file
  data <- reactive({
    file <- input$f_input 
    data <- if(!is.null(file)){
      read.csv(file$datapath)
    } %>%
      as_tibble()%>%
      mutate(id = row_number(),
             across(where(is.character),
                    as.factor))
  })
  
  #render RAW table
  output$view_table <- DT::renderDataTable(
    data(),
    options = (list(scrollX = TRUE))
  )
  ###-----------------------------------------
  ### CORRELATION
  #user select Xi to generate correlation plot
  output$corre <- renderUI({
    data <- data()
    if(!is.null(data)){
      CHOICES <- names(data)
      shinyWidgets::pickerInput('var',
                                label = 'Select to generated correlation matrix',
                                choices = CHOICES,
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE)
    }
  })
  
  #filter table matched selection from pickerInput
  df <- eventReactive(input$button1, {
    data <- data()
    if(!is.null(data)){
      data %>%
        select(dplyr::matches(input$var))
    }
  })
  
  #corr plot
  output$corr_plot <-renderPlot({
    df() %>%
      ggcorr()
  })
  ###-----------------------------------------
  ### TRAIN-TEST DATA SPLIT
  i <- eventReactive(input$button2, {
    set.seed(112)
    data() %>%
      rsample::initial_split(prop = as.numeric(input$data_split)/100)
  })

  ###-----------------------------------------
  ### FIT MODEL
  model <- eventReactive(input$button2, {
    formula <- as.formula(paste0(input$formula))
    data <- data()
    subset <-  rsample::training(i()) %>%
      select(id)
    
    ##Logistic Regression
    model_fit('GLM', formula, data, subset)

  })
  
  output$summary <- renderPrint({
    if(!is.null(model())){
      summary(model())
    }
  })
  ###-----------------------------------------
  ### VARIABLE PREPROCESS
  ### retrieve [y, X1, X2, ...] from user defined
  var <- eventReactive(input$button2, {
    words <- names(data())
    pattern_words  <- paste0('\\b', words, '\\b', collapse = "|")
    #extract Xi from formula
    input$formula %>% # y ~ X1 + X2, ...
      str_match_all(pattern_words)%>%
      flatten_chr() %>%
      unique() # [y, X1, X2, ...]
  })
  
  ### list of y_test
  y_test <- reactive({
    if(!is.null(var())){
      #retrieve response(y) from formula
      y_test_var <- var()[1]
      
      # pull response(y_test) from test data
      rsample::testing(i()) %>%
        select(matches(y_test_var)) %>%
        pull() #[y]
    }
  })
  
  ### list of y_predict
  y_pred <- reactive({
    if(!is.null(model())){
      new_data <- rsample::testing(i())
      glm_predict(model(), new_data, y_test())
    }
  })

  ###-----------------------------------------
  ### CONFUSION MATRIX
  output$conf.matrix <- renderPlot({
    cfm <- caret::confusionMatrix(as.factor(y_pred()), as.factor(y_test()))
    ggplotConfusionMatrix(cfm)
    
  })
  ###-----------------------------------------
  ### PREDICTORS INPUT PROCESS
  # we have [y, X1, X2, ...] from var(), we will create input of [X1, X2, X3, ...]
  Xi <- eventReactive(input$button2, {
    words <- names(data())
    var <- var()
    #cut response (y) 
    if (length(var) == 1){ #if formula (y ~.)
      words[!(words %in% var)] #[X1, X2, ...]
    }else{
      var[2:length(var)] #[X1, X2, ...]
    } 
  })
  
  #create dynamic predictors textInput UI
  pred_input <- eventReactive(input$button2, {
    Xi <- Xi()
    list <- list()
    for (i in c(1:length(Xi))){
      list[[i]] <- textInput(paste0('id', i),
                             Xi[i],
                             placeholder = Xi[i])
    }
    list
  })
  
  #render predictors textInput
  output$predictors <- renderUI(pred_input())
  
  #get values of all predictors Xi from user
  x <- eventReactive(input$button3, {
    list <- list()
    lapply(1:length(pred_input()), function(i){
      list[i] <- as.numeric(input[[paste0("id",i)]])
    })
  })

  ###-----------------------------------------
  ### PREDICT NEW DATA
  output$predict_result <- renderValueBox({
    if(!is.null(x())){
      new_data <- x() %>% as.data.frame()
      names(new_data) <- Xi()

      value <- glm_predict(model(), new_data, y_test())
      valueBox(
        value,
        'Predict Result',
        icon = icon('desktop')
      )
    }
  })
  
  ###-----------------------------------------
  ### 2-Dimension GRAPH
  output$graph <- renderPlot({
    Xi <- Xi() #get predictors [X1, X2]
    
    ##We only render 2-dimension graph
    if(length(Xi) == 2){
      model <- model()
      test_data <- rsample::testing(i())
      y_test <- y_test()

      test_data <- test_data %>%
        mutate(Class = y_pred()) %>%
        select(matches(Xi[1]), matches(Xi[2]), Class)
      names(test_data) <- c('x', 'y', 'Class')
      
      #Call plot function
      plot_glm(Xi, test_data, model, y_test)

    }
  })
}

shinyApp(ui = ui, server = server)


