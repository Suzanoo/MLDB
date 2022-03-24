ui <- dashboardPage(
  dashboardHeader(title = 'Classification'),
  dashboardSidebar(width = '275',
                   sidebarMenu(
                     menuItem('Instruction', tabName = 'instruction', icon = icon('hammer', lib = 'font-awesome')),
                     menuItem('Table & Correlation', tabName = 'table', icon = icon('table', lib = 'font-awesome')),
                     menuItem('DS', tabName = 'ds', icon = icon('desktop', lib = 'font-awesome')),
                     
                     fileInput(inputId = 'f_input', label = 'Upload File'),
                     div(class='col-lg-12 col-md-12',
                         hr()),
                     
                     # Correlation
                     uiOutput(outputId = 'corre'),
                     actionButton(inputId = 'button1', label = 'Correlation'),
                     div(class='col-lg-12 col-md-12',
                         hr()),
                     
                     # Train-Test
                     sliderInput("data_split", label = "Percent Train", min = 0,
                                 max = 100, value = 70, step = 5),
                     
                     # Fit Model
                     textInput(inputId = 'formula',
                               label = 'Creat formula',
                               placeholder = 'y ~ X1 + X2 + ...'),
                     actionButton(inputId = 'button2', label = 'Run Model'),
                     div(class='col-lg-12 col-md-12',
                         hr())
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'instruction'),
      tabItem(tabName = 'table',
              fluidRow(
                div(class = 'col-lg-12 col-md-12col-sm-12', id='table',
                    strong('RAW Data : '),
                    DT::dataTableOutput('view_table')
                ),
              ),
              fluidRow(
                div(class = 'col-lg-12 col-md-12col-sm-12', id='correlation',
                    strong('Correlation : '),
                    plotOutput('corr_plot')
                ),
              )),
      tabItem(tabName = 'ds',
              fluidRow(
                div(class = 'col-lg-6 col-md-6 col-sm-12', id='summary',
                    strong('Model summary : '),
                    verbatimTextOutput('summary')
                ),
                div(class = 'col-lg-6 col-md-6 col-sm-12', id='conf.matrix',
                    strong('Confusion Matrix : '),
                    plotOutput('conf.matrix')
                ),
              ),
              fluidRow(
                div(class = 'col-lg-6 col-md-6 col-sm-12', id='predictors',
                    strong('Enter to predict : '),
                    uiOutput(outputId = 'predictors'),
                    actionButton(inputId = 'button3', label = 'Predict'),
                    br(), br(),
                    valueBoxOutput("predict_result", width = '100%')
                ),
                div(class = 'col-lg-6 col-md-6 col-sm-12', id='graph',
                    strong('Graph Render||| : '),
                    plotOutput('graph')
                    
                )
              )
      )
    )
  )
)