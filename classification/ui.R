#https://rinterface.github.io/bs4Dash/reference/dashboardPage.html
ui <- dashboardPage(
  dark = FALSE,
  freshTheme = create_theme(
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
    )
  ),
  
  dashboardHeader(title = dashboardBrand(
    title = "Classification",
    color = "primary",
    href = "https://www.statlearning.com/",
    image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
  )
  ),
  dashboardSidebar(width = '275',
                   sidebarMenu(id = "tabs",
                     menuItem('Instruction', tabName = 'instruction', icon = icon('list-ul')),
                     menuItem('Table & Correlation', tabName = 'table', icon = icon('table')),
                     menuItem('Order 66', tabName = 'ds', icon = icon("vote-yea")),
                     hr(),
                     div(class = 'col-lg-12 col-md-12 col-sm-12',
                         radioButtons("label", label = ("Select Model"),
                                      choices = list("GLM", "LDA", "QDA", "NAIVE"),
                                      selected = "GLM",
                                      inline = TRUE)
                     ),
                     div(class = 'col-lg-12 col-md-12 col-sm-12',
                         sliderInput("data_split", label = "Percent Train", min = 0, # Train-Test split
                                     max = 100, value = 70, step = 5)
                     ),
                     div(class = 'col-lg-12 col-md-12 col-sm-12', 
                         textInput(inputId = 'formula',
                                   label = 'Creat formula',
                                   value = NULL,
                                   placeholder = 'y ~ X1 + X2 + X3')
                     ),
                     div(class = 'col-lg-12 col-md-12 col-sm-12', id='run',
                         actionButton(inputId = 'button2', label = 'Run Model', class = 'bg-primary'),
                         actionButton(inputId = "button3", label = "Reset", class = 'bg-primary')
                     )
                   )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = 'instruction',
              box(id = 'instruction',
                  title = 'Instruction',
                  solidHeader = FALSE,
                  width = 12,
                  status = "info",
                  icon = icon("vote-yea"),
                  div(
                    p('CREDIT : This app follow the book "An Introduction to Statistical Learning" you can read as link : ',
                      a(href = "https://www.statlearning.com/", "https://www.statlearning.com/")),
                    p('TABLE TAB : user can see table sample, then user can select variables to render the correlation plot.'),
                    p('- User can upload new file with 5MB maximum size.'),
                    p('- Sample file from the book "An Introduction to Statistical Learning"', a(href = "https://www.statlearning.com/resources-second-edition", "Click here")),
                    
                    p('CONTROL PANAL :'),
                    p('- The first part is varity of model selection, user can select one,'),
                    tags$ol(
                      tags$li('"GLM" for Logistic Regression'),
                      tags$li('"LDA" for Linear Discriminant Analysis'),
                      tags$li('"QDA" for Quadratic Discriminant Analysis'),
                      tags$li('"NAIVE" for Naive Bayes'),
                    ),
                    p('-The second part is percent train provided by slide bar.'),
                    p('-The third part is formula provided, I decide for user can type flexible of formula such as'),
                    tags$ol(
                      tags$li('Linear Terms: y ~ X1'),
                      tags$li('Multiple Terms: y ~ X1 + X2 + ...'),
                      tags$li('Interaction Terms: y ~ X1 + X1*X2 + X2...'),
                      tags$li('Non-Linear: y ~ X + I(X^2) + ...'),
                      tags$li('Polynomial: y ~ poly(X, 4)')
                    ),
                    p('After user created the formula and click "Run Model", the summary, confusion matrix, and graph were render on tab Order 66.'),
                    p('And the prediction box was render too, now user can input value of predictors, and the outcome perform.'),

                    p('NOTE: Graph will render for only 2 variables following by user formula, if predictor more than 2 , graph will not render.'),
                    br(),
                    
                  )
              )
              ),
      tabItem(tabName = 'table',
              div(id = 'RAW',
                  fileInput(inputId = 'f_input', label = 'Upload File', placeholder = "5 MB Max"),
                  fluidRow(DT::dataTableOutput('view_table')),
                  hr(),
                  radioButtons("corr", label = ("Select Correlation Render"),
                               choices = list("CORR","PAIR"),
                               selected = "CORR",
                               inline = TRUE),
                  
                  # set default Xi for correlation plot
                  # ui don't know variable in server it only communicate with server,
                  # then we cannot use CHOICES = names(Smarket), we must define new.

                  # We use input-ui and update it in server instead of outputUI for speed up app
                  # see https://appsilon.com/r-shiny-faster-updateinput-css-javascript/
                  shinyWidgets::pickerInput('var',
                                            label = 'Select to render correlation',
                                            choices = c("Year", "Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Today", "Direction"),
                                            selected = NULL,
                                            options = list(`actions-box` = TRUE),
                                            multiple = TRUE),
                  
                  actionButton(inputId = 'button1', label = 'Run Correlation', class = 'bg-primary'),
                  br(),br(),
                  fluidRow(plotOutput('corr_plot'))
                  )
              ),
      tabItem(tabName = 'ds',
              fluidRow(
                box(title = 'Model summary',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('info'),
                    fluidRow(
                      verbatimTextOutput('summary')
                    )
                ),
                box(title = 'Confusion matrix',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('bars'),
                    collapsed = FALSE,
                    fluidRow(
                      plotOutput('conf.matrix')
                    )
                )
              ),
              fluidRow(
                box(title = 'Graph render',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('chart-area'),
                    collapsed = FALSE,
                    fluidRow(
                      plotOutput('graph')
                    )
                ),
                box(title = 'Predict',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('calculator'),
                    
                    fluidRow(
                      div(class = 'col-lg-6 col-md-6 col-sm-12',
                          uiOutput(outputId = 'predictors'),
                          actionButton(inputId = 'button4', label = 'Predict', class = 'bg-success'),
                          br(), br(),
                          valueBoxOutput("predict_result", width = '100%')
                      )
                    )
                )
              )
              )
    )
  ),
  controlbar = dashboardControlbar(
    collapsed = TRUE,
    div(id = 'control',
        # Don't use this part
    )
  ),
  dashboardFooter(
    h6('This app follow the book "An Introduction to Statistical Learning", you can read as link : ',
       a(href = "https://www.statlearning.com/", "https://www.statlearning.com/"),
    h6('©2022 Suzanoo: highwaynumber12@gmail.com')
  )
  ) 
)
