##Function to plot confusion matrix
ggplotConfusionMatrix <- function(m){
  plot <- tryCatch(
    expr = {
      mytitle <- paste("Accuracy", scales::percent_format()(m$overall[1]),
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
      shinyalert::shinyalert("ConfusionMatrix Not Allow!", "Wrong Formula.", type = "error")
    }
  ) 
  plot
}

##Function to plot graph
# Graph will render only 2 dimension following by user formula ex."Response ~ Predictor1 + Predictor2",
# If you provided predictor more than 2 , graph will not render
model_plot <- function(label, formula, Xi, data, model, y_test, y_pred){
  plot <- tryCatch(
    expr = {
      data <- data %>%
        mutate(Class = y_pred) %>%
        select(matches(Xi[1]), matches(Xi[2]), Class) %>% 
        set_names(c('x', 'y', 'Class'))
        # `colnames<-`(c('x', 'y', 'Class'))

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
                       shape = 'circle', color = Class,
                       alpha = 0.01))+
        ggtitle(paste0(label, ' Fit', ":  ", formula))+
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
      
      
        # theme(legend.position = "bottom")
        # scale_color_manual(name = "Class", # or name = element_blank()
        #                    labels = unique(Class),
        #                    values = colors)
    },
    error = function(e){
      # shinyalert::shinyalert("Plot Not Allow!", "Please provide formula first.", type = "error")
    }
  )
  plot
}

## Function to create dummy data for plot.
## We create 1000 dummy point and plot them to visual classification pattern
dummy <- function(label, Xi, data, model, y_test){
  # x-y boundary
  max <- data %>% select(x) %>% max() %>% ceiling()
  min <- data %>% select(x) %>% min() %>% floor()
  
  # uniform random point
  set.seed((112))
  x_dummy1 <- runif(1000, min, max)
  x_dummy2 <- runif(1000, min, max)
  
  # convert to table
  new_data <- tibble(x_dummy1, x_dummy2) %>% 
    set_names(Xi)
    # `colnames<-`(Xi)
  # names(new_data) <- Xi #call model require original name
  
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
  fit <- tryCatch(
    expr = {
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
    },
    error = function(e){
      shinyalert::shinyalert("Fit Not Allow!", "Wrong Formula.", type = "error")
    }
  )
  fit
}

### prediction
model_predict <- function(label, model, new_data, y_test){
  pred <- tryCatch(
    expr = {
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
        print(paste0("You are in NULL block"))
        NULL
      }
    },
    error = function(e){
      shinyalert::shinyalert("Predict Not Allow!", "Wrong Formula.", type = "error")
    }
  )
  pred
}

### model summary  
summ <- function(label, model){
  if(label == 'GLM'){
    summary(model)
  }else if(label == 'LDA' | label == 'QDA' | label == 'NAIVE'){
    print(model)
  }else{
    NULL
  }
}

### TRAIN-TEST DATA SPLIT
init <- function(data, percent){
  set.seed(112)
  data %>%
    rsample::initial_split(prop = as.numeric(percent)/100)
}

### Valid user defined formulla
valid_formula <- function(data, formula){
  words <- names(data)
  pattern1 <- "[\\d\\w]( ~|~| ~ |~ )[\\d\\w]" #check operand
  pattern2 <- ("[^a-zA-Z0-9]") #cut all special characters
  
  #TODO this method made me stupid and not perfect because there a lot of function name not only 3
  func <- c(""," ", "poly", "log", "exp", "e", "sqrt", c(0:9)#cut white space and function name
         %>% as.character())
  
  #check ~, + operand
  opr <- str_match(formula, pattern1) %>%
    str_detect("NA") %>% 
    unique()
  
  #check not include of predictor variable
  other <- stringr::str_split(formula, pattern2) %>% 
    map(., function(x){
      x[!x%in%func & !x%in%words] 
    }) %>% flatten_chr() %>%  unique()
  
  if(!is.na(opr) & is_empty(other)){
    formula = formula
  }else{
    shinyjs::alert("Wrong Formula!")
    formula = NULL
  }
  formula
} 

## correlation plot
corr_ <- function(data, label){
  if(label == "CORR"){
    data %>% GGally::ggcorr()
  }else{
    data %>% GGally::ggpairs()
  }
}





