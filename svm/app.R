# Tutorials
# https://appsilon.github.io/shiny.fluent/articles/st-sales-reps-dashboard.html

# Official docs
# https://developer.microsoft.com/en-us/fluentui#/controls/web

# icon
# https://uifabricicons.azurewebsites.net/

library(shiny)
library(dplyr)
library(ggplot2)
# library(glue)
# library(plotly)
# library(leaflet)
# library(sass)
library(shiny.fluent)
library(shiny.router)
library(ISLR2)

library(e1071)

rm(list = ls(), envir = globalenv())

source("constant.R")
source("function.R")
source("modules/table.R")
source("modules/corr.R")
source("modules/confMatrix.R")
source("modules/svm.R")
source("modules/vbox.R")
source("layout.R")
source("page.R")

attach(Smarket)
#---------------------
# define the available routes:
#---------------------
router <- make_router(
  route("/", home_page),
  route("data", data_page),
  route("other", analysis_page)
  )

# Add shiny.router dependencies manually:
# they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

#---------------------
# UI:
#---------------------
ui <- fluentPage(
  layout(router$ui),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    shiny_router_script_tag
))

#---------------------
# server:
#---------------------
server <- function(input, output, session) {
  router$server(input, output, session)
  
  #---------------------
  # FILE UPLOAD:
  #---------------------
  data <- reactive({
    file <- input$file_input
    data <- 
      if(!is.null(file)){
        read.csv(file$datapath) %>% na.omit()
          
      }else{
        Smarket
      }
    data %>% 
      mutate(across(where(is.character),
                    as.factor))
  })

  # render table
  observeEvent(input$file_input, {
    srv_table(id = "table1", data = data())
  })
 
  #---------------------
  # CORRELATION PLOT:
  #---------------------
  # update input
  observeEvent(input$button2, {
    x <- input$corr
    updateChoiceGroup.shinyInput(
      session = shiny::getDefaultReactiveDomain(),
      "corr",
      label = ("Select Correlation Model"),
      options = list(
        list(key = "CORR", text = "CORR"),
        list(key = "PAIR", text = "PAIR")),
      value = x)
  })
  
  observeEvent(input$button2, {
    srv_corr(id = "corr1", data = data(), input$corr)
  })
  
  #---------------------
  # FIT MODEL:
  #---------------------
  # update input
  observeEvent(input$file_input, {
    req(data())
    
    y <- input$outcome
    updateComboBox.shinyInput(
      session = session,
      inputId = 'outcome',
      options = options_(names(data())),
      value = y 
    )
  })
  
  # update input
  observeEvent(input$button1, {
    
    i <- input$split
    updateSlider.shinyInput(
      session = shiny::getDefaultReactiveDomain(),
      "split",
      value = i
    )
  })
  
  # create test-train data
  split <- eventReactive(input$button1, {
    init_split(data(), input$split)
  })
  
  # outcome
  outcome <- reactive({
    input$outcome$text
  })
  
  # tune model
  tune.out <- eventReactive(input$button1, {
    y <- outcome()
    formula <- paste0(y, " ~ .")
    data = rsample::training(split())
    svm.fit(formula, data)
  })
  
  #---------------------
  # output:
  #---------------------
  # show best cost, best gamma
  summ <- eventReactive(input$button1, {
    cat(paste("Best Cost : ", 
              tune.out()$best.parameters$cost, 
              "Best Gamma : ",
              tune.out()$best.parameters$gamma ,sep="\n"))
  })

  output$summary <- renderPrint({
    summ()
  })
  
  # -----
  # plot confusion matrix
  observeEvent(input$button1, {
    y_test <- rsample::testing(split()) %>%
      select(matches(outcome())) %>% # fetch y from [y, X1, X2, ...] 
      pull() # [y]
    y_hat <- model_predict(tune.out()$best.model, rsample::testing(split()))
    
    srv_confMatrix("conf1", y_hat, y_test)
  })
  
  # -----
  # create ui input for graph render
  output$svm1 <- renderUI({
    data <- rsample::training(split()) %>% 
      select(-matches(outcome()))
    choices <- names(data)
    
    cost_ <- tune.out()$best.parameters$cost
    gamma_ <- tune.out()$best.parameters$gamma
    
    clusterUI("cluster1", choices, cost_, gamma_)
  })
  
  # update input
  observeEvent(input$button3, {
    data <- rsample::training(split()) %>% 
      select(-matches(outcome()))
    choices <- names(data)
    
    x <- input$x
    y <- input$y
    updateComboBox.shinyInput(session, 'x',
      options = options_(choices),
      value = x )
    updateComboBox.shinyInput(session, 'y',
      options = options_(choices),
      value = y )
  })
  
  # update input
  observe({
    c <- input$cost
    updateSlider.shinyInput(
      session = shiny::getDefaultReactiveDomain(),
      "cost",
      value = c
    )
    
    g <- input$gamma
    updateSlider.shinyInput(
      session = shiny::getDefaultReactiveDomain(),
      "gamma",
      value = g
    )
    
    # k <- input$kernel
    # updateChoiceGroup.shinyInput(
    #   session = shiny::getDefaultReactiveDomain(),
    #   "kernel",
    #   label = (""),
    #   options = list(
    #     list(key = "linear", text = "linear"),
    #     list(key = "polynomial", text = "polynomial"),
    #     list(key = "radial", text = "radial"),
    #     list(key = "sigmoid", text = "sigmoid")
    #     ),
    #   value = k)
  })

  # plot graph
  observeEvent(input$button3, {
    formula <- paste(outcome(), " ~ .")
    Xi <- c(input$x$text, input$y$text, outcome())
    
    data <- rsample::training(split()) %>%
      select(matches(Xi))

    print(outcome())
    print(input$x)
    print(input$y)

    kernel <- "radial"
    cost_ <- input$cost/100 # /100 because setting up in svm.R
    gamma_ <- input$gamma/10 # /10 because settig up in svm.R

    # cluster modules
    cluster("cluster1", formula, data, kernel, cost_, gamma_)

  })
  
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)