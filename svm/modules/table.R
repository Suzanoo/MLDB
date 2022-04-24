library(shiny)
library(DT)

ui_table<- function(id) {
  
  table_fill <-
    div(
      Stack(
        token = list(childrenGap = 10),
        shiny::fileInput(inputId = 'file_input', label = 'Upload File', placeholder = "5 MB Max"),
    )
  )
  ns <- shiny::NS(id)
  div(
    Stack(
      horizontal = TRUE,
      makeCard("", table_fill, size = 4),
      makeCard("Table",  DTOutput(ns("table")), size = 8, style = " background-color : lightgrey;")
      
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
# ui <- fluentPage(
#   Stack(ui_table("table11"))
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