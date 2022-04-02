ui <- dashboardPage(
  freshTheme = fresh::create_theme(
    fresh::bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
    )
  ),
  dashboardHeader(title = 'Linear Regression'),
  dashboardSidebar(width = '275',
                   sidebarMenu(id = "tabs",
                     menuItem('Instruction', tabName = 'inst', icon = icon('hammer', lib = 'font-awesome')),
                     menuItem('Table & Correlation', tabName = 'table', icon = icon('table', lib = 'font-awesome')),
                     menuItem('Order 66', tabName = 'ds', icon = icon('desktop', lib = 'font-awesome')),
                     
                     div(class='col-lg-12 col-md-12',hr()),

                     # Fit model
                     sliderInput("data_split", label = "Percent Train", min = 0, # Train-Test split
                                 max = 100, value = 70, step = 5),
                     textInput(inputId = 'formula',
                               label = 'Creat formula',
                               value = NULL,
                               placeholder = 'y ~ X1 + X2 + X3'),
                     actionButton(inputId = 'button2', label = 'Run Model', class = 'bg-primary'),
                     actionButton(inputId = "button3", label = "Reset", class = 'bg-primary'),
                     
                     div(class='col-lg-12 col-md-12',
                         hr(),
                     )
                   )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = 'inst',
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
                    p('-The first part is percent train provided by slide bar.'),
                    p('-The second part is formula provided, I decide for user can type flexible of formula such as'),
                    tags$ol(
                      tags$li('Linear Terms: y ~ X1'),
                      tags$li('Multiple Terms: y ~ X1 + X2 + ...'),
                      tags$li('Interaction Terms: y ~ X1 + X1*X2 + X2...'),
                      tags$li('Non-Linear: y ~ X + I(X^2) + ...'),
                      tags$li('Polynomial: y ~ poly(X, 4)')
                    ),
                    p('After user created the formula and click "Run Model", the summary, explore plot, and residual plot were render on tab Order 66.'),
                    p('And the prediction box was render too, now user can input value of predictors, and the outcome perform.'),
 
                  )
              )
      ),
      tabItem(tabName = 'ds',
              fluidRow(
                box(title = 'Model summary',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('info', lib = 'font-awesome'),
                    fluidRow(
                      verbatimTextOutput('summary')
                    )
                ),
                box(title = 'Explore Plot',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('chart-area', lib = 'font-awesome'),
                    collapsed = FALSE,
                    fluidRow(
                      plotOutput('explore_plot')
                    )
                )
              ),

              fluidRow(
                box(title = 'Residual Plot',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('chart-area', lib = 'font-awesome'),
                    collapsed = FALSE,
                    fluidRow(
                      plotOutput('model_plot')
                    )
                ),
                box(title = 'Predict',
                    solidHeader = FALSE,
                    width = 6,
                    status = "info",
                    icon = icon('calculator', lib = 'font-awesome'),
                    
                    fluidRow(
                      div(class = 'col-lg-6 col-md-6 col-sm-12',
                          uiOutput(outputId = 'predictors'),
                          actionButton(inputId = 'button4', label = 'Predict', class = 'bg-success'),
                          br(), br(),
                          valueBoxOutput("predict_result", width = '100%')
                      )
                    )
                )
              )),
      tabItem(tabName = 'table',
              fileInput(inputId = 'f_input', label = 'Upload File'),
              fluidRow(DT::dataTableOutput('view_table')),
              hr(),
              radioButtons("corr", label = ("Select Correlation Render"),
                           choices = list("CORR","PAIR"),
                           selected = "CORR",
                           inline = TRUE),
              
              # Initail input update later
              shinyWidgets::pickerInput('var',
                                        label = 'Select to render correlation',
                                        choices = c("crim", "zn", "indus", "chas", "nox", "rm", "age",
                                                    "dis", "rad", "tax", "ptratio", "lstat", "medv"),
                                        selected = NULL,
                                        options = list(`actions-box` = TRUE),
                                        multiple = TRUE),
              
              actionButton(inputId = 'button1', label = 'Run Correlation', class = 'bg-primary'),
              fluidRow(
                plotOutput('corr_plot')
              )
      )
    )
  ),
  controlbar = dashboardControlbar(),
  dashboardFooter(
    h6('This app follow the book "An Introduction to Statistical Learning", you can read as link : ',
       a(href = "https://www.statlearning.com/", "https://www.statlearning.com/"),
       h6('©2022 Suzanoo: highwaynumber12@gmail.com')
    )
  )
)