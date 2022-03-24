
library(shiny)
library(shinydashboard)
library(tidyverse)
library(GGally)

library(ISLR2)

#---------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = 'Linear Regression'),
  dashboardSidebar(width = '275',
    sidebarMenu(
      menuItem('Instruction', tabName = 'inst', icon = icon('hammer', lib = 'font-awesome')),
      menuItem('Table & Correlation', tabName = 'table', icon = icon('table', lib = 'font-awesome')),
      menuItem('DS', tabName = 'ds', icon = icon('desktop', lib = 'font-awesome')),
      
      fileInput(inputId = 'f_input', label = 'Upload File'),
      div(class='col-lg-12 col-md-12',
          hr()),
      
      # Correlation
      uiOutput(outputId = 'corre'),
      actionButton(inputId = 'button3', label = 'Run Corr'),
      div(class='col-lg-12 col-md-12',
          hr()),
      
      # Fit model
      uiOutput(outputId = 'response'),
      actionButton(inputId = 'button', label = 'Run Model'),
      
      div(class='col-lg-12 col-md-12',
          hr(),
      ),
      # Predic
      uiOutput(outputId = 'Observer'),
      actionButton(inputId = 'button2', label = 'Run Predict'),
      div(class='col-lg-12 col-md-12',
          valueBoxOutput("pred",width = '100%'),
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'inst',
              strong('Instruction'),
              br(),br(),
              p('User upload file first, then the app will render raw table.'),
              p('User can see raw table and select observer to generate coorelation matrix.'),
              p('According to correlation user create formula to fit model, then app generated coefficient plot and residual plot.'),
              tags$ol(
                tags$li('Linear Regression: y ~ X1'),
                tags$li('Multiple Linear Regression: y ~ X1 + X2 + ...'),
                tags$li('Interaction Terms: y ~ X1 + X2 + X1X2 + ...'),
                tags$li('Non-Linear: y ~ X + I(X^2) + ...'),
                tags$li('Polynomial: y ~ poly(X, 4)'),
                ),
              p('App will generate observer input for prediction.')
      ),
      tabItem(tabName = 'ds',
              fluidRow(
                verbatimTextOutput('summary')
              ),
              fluidRow(
                box(plotOutput('explore_plot')),
                box(plotOutput('model_plot'))
              )),
      tabItem(tabName = 'table',
              fluidRow(
                DT::dataTableOutput('view_table')
              ),
              fluidRow(
                plotOutput('corr_plot')
              )
              )
    ),
  )
)

#---------------------------------------------------
server <- function(input, output, session){
  #upload csv file
  data <- reactive({
    file <- input$f_input 
    data <- if(!is.null(file)){
      read.csv(file$datapath)
      } %>%
      as_tibble() 
      # select_if(is.numeric)
  })
  
  #render RAW table
  output$view_table <- DT::renderDataTable(
    data(),
    options = (list(scrollX = TRUE))
  )
  
  #user select Xi to generate correlation plot
  output$corre <- renderUI({
    data <- data()
    if(!is.null(data)){
      CHOICES <- names(data)
      shinyWidgets::pickerInput('var',
                  label = 'Select to generated correlation of variables',
                  choices = CHOICES,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE)
    }
  })
  
  #filter table matched selection from pickerInput
  df <- eventReactive(input$button3, {
    data <- data()
    print(input$var)
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
  
  #user define formula
  output$response <- renderUI({
    textInput(inputId = 'formula',
              label = 'Creat formula',
              placeholder = 'y ~ X1 + X2 + ...')
  })

  #create model
  lm_fit <- eventReactive(input$button, {
    data <- data()
    lm.fit <- lm(as.formula(paste0(input$formula)), data = data)
    lm.fit
  })
  
  #--------------------------
  ## We have formula which contain predictors Xi
  ## We extract those Xi and then create input of those Xi for prediction
  Xi <- eventReactive(input$button, {
    data <- data()
    

    pattern_words  <- paste0('\\b', words, '\\b', collapse = "|")
    
    #extract Xi from formula
    y <- input$formula %>% # y ~ X1 + X2, ...
      str_match_all(pattern_words)%>%
      flatten_chr() %>%
      unique() #[y, X1, X2, ...]
    
    #cut response (y) 
    if (length(y) == 1){ #if formula (y ~.)
      Xi <- words[!(words %in% y)] #[X1, X2, ...]
    }else{
      Xi <- y[2:length(y)] #[X1, X2, ...]
    } 
  })
  
  #create dynamic textInput
  pred_input <- reactive({
    Xi <- Xi()
    list <- list()
    for (i in c(1:length(Xi))){
      list[[i]] <- textInput(paste0('id', i),
                          Xi[i],
                          placeholder = Xi[i])
    }
    list
  })

  #render textInput
  output$Observer <- renderUI(pred_input())
  
  #get values of all observation Xi from user
  x <- reactive({
    list <- list()
    lapply(1:length(pred_input()), function(i){
      list[i] <- as.numeric(input[[paste0("id",i)]])
    })
  })
  
  #calculate y_pred
  y_pred <-  eventReactive(input$button2, {
    lm.fit <- lm_fit()
    
    new_data <- data.frame(x()) #get values of all observation Xi
    names(new_data) <- Xi()
    
    predict(lm.fit, newdata = new_data, interval = "confidence")[1]
  })
  
  output$pred <- renderValueBox({
    valueBox(sprintf(y_pred(), fmt = '%#.4f') , "PREDICTS", icon = icon("users"),
             color = "orange"
    )
  })

  #--------------------------
  #render summary of model
  output$summary <- renderPrint({
    summary(lm_fit())
  })
  
  #coeff plot
  output$explore_plot <- renderPlot({
    #ggduo(data)
    lm_fit() %>%
      broom::tidy() %>%
      ggplot(aes(x = term, y = estimate, color = p.value <= 0.05)) + 
      geom_segment(aes(xend = term, yend = 0)) + 
      geom_point() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste0("Coefficients for predicting ", input$response),
           subtitle = paste0('Formula : ', input$formula))
  })
  
  #residual plot
  output$model_plot <- renderPlot({
    data <- data()
    lm.fit <- lm_fit()
    data %>%
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
  })
}

shinyApp(ui = ui, server = server)