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
                     
                     
                     # Fit Model 
                     radioButtons("label", label = ("Select Model"),
                                  choices = list("GLM", "LDA", "QDA", "NAIVE"), 
                                  selected = "GLM",
                                  inline = TRUE),
                     sliderInput("data_split", label = "Percent Train", min = 0, # Train-Test split
                                 max = 100, value = 70, step = 5),
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
      tabItem(tabName = 'instruction',
              strong('Instruction'),
              br(),br(),
              p('User upload file first, then the app will render raw table.'),
              p('User can see raw table and select observer to generate coorelation matrix.'),
              p('User create formula to fit model, then app generated confusion matrix, graph and input for prediction follwing formula.'),
              p('Graph render only 2 dimension following by user formula "Response ~ Predictor1 + Predictor2", if predictor more than 2 graph will not render.'),
              p('')
              
              ),
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
                div(class = 'col-lg-6 col-md-6 col-sm-12', id='graph',
                    strong('Graph Render||| : '),
                    plotOutput('graph')
                ),
                div(class = 'col-lg-6 col-md-6 col-sm-12', id='predictors',
                    strong('Enter to predict : '),
                    uiOutput(outputId = 'predictors'),
                    actionButton(inputId = 'button3', label = 'Predict'),
                    br(), br(),
                    valueBoxOutput("predict_result", width = '100%')
                )
              )
      )
    )
  )
)
