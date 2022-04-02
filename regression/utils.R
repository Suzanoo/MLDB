###-----------------------------------------
## correlation plot
corr_ <- function(data, label){
  if(label == "CORR"){
    data %>% GGally::ggcorr()
  }else{
    data %>% GGally::ggpairs()
  }
}
###-----------------------------------------
### Valid formula
valid_formula <- function(data, formula){
  words <- names(data)
  pattern1 <- "[\\d\\w ]~[ \\d\\w.{ .}]" #check operand
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

# Retrieve variables from formula
variables <- function(data, formula){
  words <- names(data)
  pattern_words  <- paste0('\\b', words, '\\b', collapse = "|")
  #extract Xi from formula
  formula %>% 
    str_match_all(pattern_words)%>%
    flatten_chr() %>%
    unique() # [y, X1, X2, ...]
}

# Retrieve predictors from formula
predictors <- function(data, formula){
  words <- names(data)
  variables <- variables(data, formula) # [y, X1, X2, ...]
  
  if (length(variables) == 1){ #if formula (y ~ .)
    words[!(words %in% variables)] #[X1, X2, ...]
  }else{
    variables[2:length(variables)] #[X1, X2, ...]
  }
}

###-----------------------------------------
### TRAIN-TEST DATA SPLIT
init <- function(data, percent){
  set.seed(112)
  data %>%
    rsample::initial_split(prop = as.numeric(percent)/100)
}

###-----------------------------------------
### Fit model
model_fit <- function(formula, data, subset){
  fit <- tryCatch(
    expr = {
      stats::lm(stats::as.formula(paste0(formula)), data=data, subset=subset)
    },
    error = function(e){
      shinyalert::shinyalert("Fit Not Allow!", "Wrong Formula.", type = "error")
      NULL
    }
  )
  fit
}

## predict
outcome <-  function(model, new_data, name){
  new_data <- new_data %>% 
    as_tibble(.name_repair = "unique") %>% 
    set_names(name)
  
  predict(model,
          new_data,
          interval = "confidence")
}

###-----------------------------------------





