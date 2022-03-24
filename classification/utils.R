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

##Function to plot graph
model_plot <- function(label, Xi, data, model, y_test, y_pred){
  data <- data %>%
    mutate(Class = y_pred) %>%
    select(matches(Xi[1]), matches(Xi[2]), Class)
  names(data) <- c('x', 'y', 'Class')
  
  # create dummy data
  dummy <- dummy(label, Xi, data, model, y_test)
  
  # plot graph
  ggplot()+
    geom_point(data = data, #plot 
               aes(x = x, y = y,
                   shape = Class, color = Class,
                   size = 0.01))+
    geom_point(data = dummy, #plot dummy
               aes(x = x, y = y,
                   shape = 'circle', color = Class, alpha = 0.01))+
    ggtitle(paste0(label, ' Fit'))+
    labs(y= Xi[2], x = Xi[1])+
    theme(legend.position = "none")
}

## Function to create dummy data for plot.
## We create 1000 dummy point and plot them to visual classification anatomy.
dummy <- function(label, Xi, data, model, y_test){
  # x-y boundary
  max <- data %>% select(x) %>% max() %>% ceiling()
  min <- data %>% select(x) %>% min() %>% floor()

  # uniform random point
  set.seed((112))
  x_dummy1 <- runif(1000, min, max)
  x_dummy2 <- runif(1000, min, max)
  
  # convert to table
  new_data <- tibble(x_dummy1, x_dummy2)
  names(new_data) <- Xi #call model require original name
  
  # Call prediction
  y_pred <- model_predict(label, model, new_data, y_test)
 
  # save all into dataframe, ready to plot
  dummy <- new_data %>%
    bind_cols(y_pred) %>%
    set_names(c('x', 'y', 'Class'))
  
  dummy
}

###-----------------------------------------
### Fit model
model_fit <- function(label, formula, data, subset){
  # Logistic Regression
  if (label == 'GLM'){
    glm(formula = as.formula(paste0(formula)),
        data = data,
        family = binomial,
        subset = subset)
  }else if(label == 'LDA'){
    MASS::lda(formula = as.formula(paste0(formula)),
              data = data, 
              subset = subset)
  }else if(label == 'QDA'){
    MASS::qda(formula = as.formula(paste0(formula)),
              data = data, 
              subset = subset)
  }else if(label == 'NAIVE'){
    e1071::naiveBayes(formula = as.formula(paste0(formula)),
                      data = data,
                      subset = subset)
  }else{
    NULL
  }
}

### prediction
model_predict <- function(label, model, new_data, y_test){
  if(label == 'GLM'){
    probability <- predict(model,
                           new_data,
                           type="response")
    
    levels <- unique(y_test)# %>% as.factor()
    y_pred <- rep(levels[1], length(probability))
    y_pred[probability < 0.5] = levels[2]
    y_pred
    
  }else if(label == 'LDA'){
    lda.fit.pred <- predict(model, new_data, type="response")
    lda.fit.pred$class

  }else if(label == 'QDA'){
    qda.fit.pred <- predict(model, new_data, type="response")
    qda.fit.pred$class
    
  }else if(label == 'NAIVE'){
    nb.fit.pred  <- predict(model, new_data)

  }else{
    print(paste0("I am in NULL block"))
    NULL
    }
}

### model summary  
summ <- function(label, model){
  if(label == 'GLM'){
    summary(model)
  }else if(label == 'LDA' | label == 'QDA' | label == 'NAIVE'){
    model
  }else{
    NULL
  }
}









