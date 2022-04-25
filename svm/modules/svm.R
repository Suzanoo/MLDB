# https://beta.rstudioconnect.com/seanlopp/ShinyModulesDemo/
#---------------------
# Module Code
#---------------------
ui_cluster <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    hr(),
  )
}

ui_ctrl <- function(id, choices, cost_, gamma_){
  CHOICE <- choices
  ns <- NS(id)
  tagList(
    h5('Kernel : see ', a(href = "https://www.rdocumentation.org/packages/e1071/versions/1.7-9/topics/svm", "Detail Here")),
    h5("Note : Default is radial other is not effect in this app now."),
    ChoiceGroup.shinyInput("kernel", label = "",
                           options = list(
                             list(key = "linear", text = "linear"),
                             list(key = "polynomial", text = "polynomial"),
                             list(key = "radial", text = "radial"),
                             list(key = "sigmoid", text = "sigmoid")
                             ),
                           value = "radial"),

    h5("Cost x 100 : "),
    Slider.shinyInput("cost", value = cost_*100, min = 0, max = 10000, step = 500),

    h5("Gamma x 10  : "),
    Slider.shinyInput("gamma", value = gamma_*10, min = 0, max = 100, step = 5),

    h5("Y Variable"),
    ComboBox.shinyInput(inputId = "x", options = options_(CHOICE)),

    h5("X Variable"),
    ComboBox.shinyInput(inputId = "y", options = options_(CHOICE)),
    br(),
    PrimaryButton.shinyInput(inputId = "button3", text = "Plot")
  )
}

cluster <- function(id, formula, data, kernel, cost, gamma){
  moduleServer(
    id,
    function(input, output, session){

      # fit with best parameter and plot 
      output$plot <- renderPlot({
        
        svmfit <- svm(as.formula(formula), data = data, kernel = kernel,
                      gamma = gamma, cost = cost)
        plot(svmfit, data)
      })
    }
  )
}

