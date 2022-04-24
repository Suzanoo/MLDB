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
    Stack(ui_table("table1")),
    Stack(ui_corr("corr1"))
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
    # Stack(
    #   horizontal = TRUE,
    #   tokens = list(childrenGap = 10),
    #   makeCard("Title", vbox_UI("vb1"), size = 3, style = "background-color : orange;"),
    #   makeCard("Title", vbox_UI("vb2"), size = 3, style = "background-color : green;"),
    #   makeCard("Title", vbox_UI("vb3"), size = 3, style = "background-color : #279ce6;"),
    #   makeCard("Title", vbox_UI("vb4"), size = 3, style = "background-color : #d5a3e3;"),
    # ),
    Stack(
      horizontal = TRUE,
      makeCard("", ctrl, size = 4),
      makeCard("Confusion Matrix", ui_confMatrix("conf1"), size = 8, style = "background-color : lightgrey;"),
     
    ),
    Stack(
      horizontal = TRUE,
      makeCard("Summary", verbatimTextOutput("summary"), size = 4, style = "background-color : lightgrey;"),
      makeCard("Cluster", uiOutput("svm1"), size = 8, style = "background-color : lightgrey;")
    )
  )
)
