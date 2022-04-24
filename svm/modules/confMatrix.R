library(shiny)
library(DT)

ggplotConfusionMatrix <- function(m){
  plot <- tryCatch(
    expr = {
      mytitle <- paste("Accuracy", scales::percent_format()(m$overall[1]),
                       "  ",
                       "Kappa", scales::percent_format()(m$overall[2]))

      ggplot(data = as.data.frame(m$table) ,
             aes(x = Reference, y = Prediction)) +
        geom_tile(aes(fill = log(Freq)), colour = "white") +
        scale_fill_gradient(low = "white", high = "steelblue") +
        geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
        theme(legend.position = "none") +
        ggtitle(mytitle)
    },
    error = function(e){
      shinyalert::shinyalert("ConfusionMatrix Not Allow!", "Try Again", type = "error")
    }
  )
  plot
}

ui_confMatrix <- function(id) {
  ns <- shiny::NS(id)
  plotOutput(ns("conf_matrix"))
}

srv_confMatrix <- function(id, y_hat, y_test){
  moduleServer(
    id,
    function(input, output, session){
      
      output$conf_matrix <- renderPlot({
        cfm <- caret::confusionMatrix(y_hat, y_test)
        ggplotConfusionMatrix(cfm)

      })

    }
  )
}


#---------------------
# HOW TO :
#---------------------
# ui <- fluentPage(
#   ui_confMatrix("conf1")
# )
# 
# server <- function(input, output, session){
#   observeEvent(input$button1, {
#     # confusion matrix modules
#     srv_confMatrix(id = "conf1", y_hat = y_hat(), y_test = y_test())
#   })
# }