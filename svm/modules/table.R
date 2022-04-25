library(shiny)
library(DT)

ui_table<- function(id) {
  
  ns <- shiny::NS(id)

  div(
    fluidRow(
      column(width = 4,
              shiny::fileInput(inputId = 'file_input', label = 'Upload File', placeholder = "5 MB Max"))
    ),
    fluidRow(
      makeCard("Table", DTOutput(ns("table")), style = " background-color : lightgrey;")
    )
  )
}

srv_table <- function(id, data){
  
  moduleServer(
    id,
    function(input, output, session){
      output$table <- DT::renderDataTable(
        DT::datatable(data,
                      options = (list(scrollX = TRUE)) %>% 
                        head()
        )
      )
    }
  )
}

#---------------------
# HOW TO :
#---------------------
# ui <- fluidPage(
#   ui_table("table11")
# )
# 
# server <- function(input, output, session){
# 
#   data <- reactive({
#     file <- input$file_input
#     data <-
#       if(!is.null(file)){
#         read.csv(file$datapath) %>% na.omit()
# 
#       }else{
#         Smarket
#       }
#     data %>%
#       mutate(across(where(is.character),
#                     as.factor))
#   })
# 
#   # render table
#   observeEvent(input$file_input, {
#     srv_table(id = "table1", data = data())
#   })
# }
# 
# shinyApp(ui, server)