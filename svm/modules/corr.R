#---------------------
# UI :
#---------------------
ui_corr <- function(id){
  corr_fill <- Stack(
    token = list(childrenGap = 10),

    h4("Correlation :"),
    ChoiceGroup.shinyInput("corr", label = "Select Correlation Model",
                           options = list(
                             list(key = "CORR", text = "CORR"),
                             list(key = "PAIR", text = "PAIR")),
                           value = "CORR"),
    br(),
    PrimaryButton.shinyInput(inputId = "button2", text = "Run Correlation")

  )

  ns <- shiny::NS(id)
  div(
    # Stack(
    #   token = list(childrenGap = 10),
    #   horizontal = TRUE,
    #   makeCard("", corr_fill, size = 4),
    #   makeCard("Plot Correlation Matrix", plotOutput(ns("corr_plot")), size = 8, style = " background-color : lightgrey;")
    # )
    fluidRow(
      div(class = 'col-lg-4 col-md-4 col-sm-12',
        makeCard("", corr_fill)),
      div(class = 'col-lg-8 col-md-8 col-sm-12',
        makeCard("Plot Correlation Matrix", plotOutput(ns("corr_plot")), style = " background-color : lightgrey;"))
    )
  )

}
#---------------------
# SERVER :
#---------------------

srv_corr <- function(id, data, label){
  moduleServer(
    id,
    function(input, output, session){
      output$corr_plot <- renderPlot(
        corr_(data, label)
      )
    }
  )
}

#---------------------
# HOW TO :
#---------------------
# ui <- fluentPage(
#   Stack(ui_corr("corr1"))
# )
# 
# server <- function(input, output, session){
#   
#   observeEvent(input$button2, {
#     x <- input$corr
#     updateChoiceGroup.shinyInput(
#       session = shiny::getDefaultReactiveDomain(),
#       "corr",
#       label = ("Select Correlation Model"),
#       options = list(
#         list(key = "CORR", text = "CORR"),
#         list(key = "PAIR", text = "PAIR")),
#       value = x)
#   })
#   
#   observeEvent(input$button2, {
#     srv_corr(id = "corr1", data = data, input$corr)
#   })
# }



