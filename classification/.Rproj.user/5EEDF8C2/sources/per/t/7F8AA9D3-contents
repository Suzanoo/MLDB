##Function to plot confusion matrix
ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", scales::percent_format()(m$overall[1]),
                   "Kappa", scales::percent_format()(m$overall[2]))
  
  p <- ggplot(data = as.data.frame(m$table) ,
              aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  p
}

###-----------------------------------------
## Function to create dummy data for plot.
## We create 1000 dummy point and plot them to visual classification anatomy.
dummy <- function(Xi, test_data, model, y_test){
  # x-y boundary
  max <- test_data %>% select(x) %>% max() %>% ceiling()
  min <- test_data %>% select(x) %>% min() %>% floor()
  
  # uniform random point
  set.seed((112))
  x_dummy1 <- runif(1000, min, max)
  x_dummy2 <- runif(1000, min, max)
  
  # convert to table
  new_data <- tibble(x_dummy1, x_dummy2)
  names(new_data) <- Xi #call model require original name
  
  # Call glm prediction
  y_pred <- glm_predict(model, new_data, y_test)
 
  # save all into dataframe, ready to plot
  dummy <- new_data %>%
    bind_cols(y_pred) %>%
    set_names(c('x', 'y', 'Class'))
  
  dummy
}

###-----------------------------------------
##Function to render logistic regression
plot_glm <- function(Xi, test_data, model, y_test){
  
  # create dummy data
  dummy <- dummy(Xi, test_data, model, y_test)
  
  # plot graph
  ggplot()+
    geom_point(data = test_data, #plot 
               aes(x = x, y = y,
                   shape = Class, color = Class,
                   size = 0.01))+
    geom_point(data = dummy, #plot dummy
               aes(x = x, y = y,
                   shape = 'circle', color = Class, alpha = 0.01))+
    ggtitle("GLM fit")+
    labs(y= Xi[2], x = Xi[1])+
    theme(legend.position = "none")
}

###-----------------------------------------
model_fit <- function(model, formula, data, subset){
  
  if (model == 'GLM'){
    ##Logistic Regression
    glm(formula = formula,
        data = data,
        family = binomial,
        subset = subset) #parse index of train
  }
  
}

###-----------------------------------------
glm_predict <- function(model, new_data, y_test){
  # prediction of dummy point
  probability <- predict(model,
                         new_data,
                         type="response")
  
  levels <- unique(y_test)# %>% as.factor()
  y_pred <- rep(levels[1], length(probability))
  y_pred[probability < 0.5] = levels[2]
  
  y_pred
}









