#---------------------
# create home page:
#---------------------
card1 <- makeCard(
  "Data Panel",
  div(
    tags$ol(
      tags$li('Upload file first, default file is "Smarket" from ISLR2 library', 
              a(href = "https://www.statlearning.com/", " Visit Here")),
      tags$li("You can execute correlation if you want.")
    )
  ))

card2 <- makeCard(
  "Analysis Panel",
  div(
    tags$ol(
      tags$li("Select out come from list."),
      tags$li("Adjust % train."),
      tags$li("Click tune model, app tuned model with kernel = radial, cost = [.001, .01, .1, 1, 5, 10, 100], and gamma = [.5, 1, 2, 3, 4]."),
      tags$li("App will execute confusion metrix, you can see accuracy here."),
      tags$li("At Summmary panel app will show best cost and best gamma from tunning model."),
      tags$li("At Cluster you can select pair of variables to render graph which used best cost and best gamma to execution."),
      tags$li("Now you can adjust kernel, cost , gamma if you want."),
    ),
    ))

home_page <- makePage(
  "Support Vector Machine Demo",
  "This app use shiny.fluent",
  div(card1, card2)
)

#---------------------
# create data page:
#---------------------
data_page <- makePage(
  "DATA",
  "",
  
  div(
    ui_table("table1"),
    ui_corr("corr1")
  )
)

#---------------------
# create analysis page:
#---------------------
ctrl <- Stack(
  token = list(childrenGap = 10),
  
  h4("Select Outcome"),
  h6("Note : Outcome must be qualitative variable"),
  ComboBox.shinyInput(inputId = "outcome", options = options_(CHOICES)),
  br(),
  h4("% Train"),
  Slider.shinyInput("split", value = 70, min = 20, max = 80, step = 5),
  
  hr(),
  PrimaryButton.shinyInput("button1", "Tune Model")
)

analysis_page <- makePage(
  "SVM FIT",
  "support vector machine demo",
  div(
    fluidRow(
      div(class = 'col-lg-4 col-md-4 col-sm-12',
         makeCard("", ctrl)  
      ),
      div(class = 'col-lg-8 col-md-8 col-sm-12',
         makeCard("Confusion Matrix", ui_confMatrix("conf1"), style = "background-color : lightgrey;")
      )
    ),
    fluidRow(
      div(class = 'col-lg-4 col-md-4 col-sm-12',
         makeCard("Summary", verbatimTextOutput("summary"), style = "background-color : lightgrey;"),
         makeCard("Graph Control", uiOutput("svm2"), style = "background-color : lightgrey;"),
      ),
      div(class = 'col-lg-8 col-md-8 col-sm-12',
         makeCard("Cluster", uiOutput("svm1"), style = "background-color : lightgrey;")
      )
    )
  )
    
    # Stack(
    #   horizontal = TRUE,
    #   makeCard("", ctrl, size = 4),
    #   makeCard("Confusion Matrix", ui_confMatrix("conf1"), size = 8, style = "background-color : lightgrey;"),
    #  
    # ),
    # Stack(
    #   horizontal = TRUE,
    #   makeCard("Summary", verbatimTextOutput("summary"), size = 4, style = "background-color : lightgrey;"),
    #   makeCard("Cluster", uiOutput("svm1"), size = 8, style = "background-color : lightgrey;")
    # )
)
