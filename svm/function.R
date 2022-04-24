makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

# create option of ComboBox.shinyInput
options_ <- function(data){
  data %>% 
    as_tibble() %>% 
    setNames("key") %>% 
    mutate(text = data)
}

# -----------------
### TRAIN-TEST DATA SPLIT
init_split <- function(data, percent){
  set.seed(112)
  data %>%
    rsample::initial_split(prop = as.numeric(percent)/100)
}

## User defined formula ex. y ~ X1 + X2 and we get it as "y ~ X1 + X2".
## We need retrieve "y ~ X1 + X2" to list of characters [y, X1, X2, ...] 
# variables <-function(data, formula){
#   words <- names(data)
#   pattern_words  <- paste0('\\b', words, '\\b', collapse = "|")
#   #extract Xi from formula
#   var <- formula %>% 
#     stringr::str_match_all(pattern_words)%>%
#     purrr::flatten_chr() %>%
#     unique() # [y, X1, X2, ...]
#   if (length(var == 1)){
#     var = c(var, words[!(words %in% var)])
#   }else{
#     var = var
#   }
# }

## correlation plot
corr_ <- function(data, label){
  if(label == "CORR"){
    data %>% GGally::ggcorr()
  }else{
    data %>% GGally::ggpairs()
  }
}

svm.fit <- function(formula, data, kernel = "radial", 
                    cost = c(.001, .01, .1, 1, 5, 10, 100),
                    gamma = c(.5, 1, 2, 3, 4)){
  set.seed(112)
  tune.out <- e1071::tune(
    svm,
    as.formula(formula),
    data = data,
    kernel = kernel,
    ranges = list(
      cost = cost,
      gamma = gamma
    ))
}


### prediction
model_predict <- function(model, new_data){
  pred <- tryCatch(
    expr = {
      predict(model, new_data)
    },
    error = function(e){
      shinyalert::shinyalert("Predict Not Allow!", "Wrong Formula.", type = "error")
      # plot("Not Allow")
    }
  )
  pred
}

#Function to plot graph
model_plot <- function(Xi, data, model){
  plot <- tryCatch(
    expr = {
      data <- data %>%
        purrr::set_names(c('x', 'y', 'Class'))

      # create dummy data
      # dummy <- dummy(Xi, data, model)

      # plot graph
      ggplot()+
        geom_point(data = data, #plot
                   aes(x = x, y = y,
                       shape = Class, color = Class,
                       size = 0.01))+
        # geom_point(data = dummy, #plot dummy
        #            aes(x = x, y = y,
        #                shape = 'circle', color = Class,
        #                alpha = 0.01))+
        ggtitle(paste0('Fit', ":  "))+
        labs(y= Xi[2], x = Xi[1])+
        theme(
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
        )+
        guides(alpha = "none")+
        guides(size = "none")+
        guides(shape = "none")
    },
    error = function(e){
      shinyalert::shinyalert("Plot Not Allow!", "Wrong Variables Selected", type = "error")
      # plot("Not Allow")
    }
  )
  plot
}

# model_plot <- function(formula, Xi, data, model, y_hat){
# 
#     data <- data %>%
#       mutate(Class = y_hat) %>%
#       select(matches(Xi[1]), matches(Xi[2]), Class) %>%
#       purrr::set_names(c('x', 'y', 'Class'))
#     
#     # create dummy data
#     # dummy <- dummy(Xi, data, model)
#     
#     # plot graph
#     plot <- ggplot()+
#       geom_point(data = data, #plot
#                  aes(x = x, y = y,
#                      shape = Class, color = Class,
#                      size = 0.01))+
#       # geom_point(data = dummy, #plot dummy
#       #            aes(x = x, y = y,
#       #                shape = 'circle', color = Class,
#       #                alpha = 0.01))+
#       ggtitle(paste0('Fit', ":  ", formula))+
#       labs(y= Xi[2], x = Xi[1])+
#       theme(
#         legend.position = c(.95, .95),
#         legend.justification = c("right", "top"),
#         legend.box.just = "right",
#         legend.margin = margin(6, 6, 6, 6)
#       )+
#       guides(alpha = "none")+
#       guides(size = "none")+
#       guides(shape = "none")
#     
#     return(plot)
# }

## Function to create dummy data for plot.
## We create 1000 dummy point and plot them to visual classification pattern
dummy <- function(Xi, data, model){
  # x-y boundary
  max <- data %>% select(x) %>% max() %>% ceiling()
  min <- data %>% select(x) %>% min() %>% floor()
  
  # uniform random point
  set.seed((112))
  x_dummy1 <- runif(1000, min, max)
  x_dummy2 <- runif(1000, min, max)
  
  # convert to table
  new_data <- tibble(x_dummy1, x_dummy2) %>% 
    purrr::set_names(Xi)
  
  # Call prediction
  y_hat <- model_predict(model, new_data)
  
  # save all into dataframe, ready to plot
  dummy <- new_data %>%
    bind_cols(y_hat) %>%
    purrr::set_names(c('x', 'y', 'Class'))
  
  dummy
}












