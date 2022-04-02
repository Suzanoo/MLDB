#---------------------------------
#Valid formula
valid_formular <- function(data, formula){
  words <- names(data)
  regx1 <- "[\\d\\w ]~[ \\d\\w.{ .}]" #check operand
  regx2 <- ("[^a-zA-Z0-9]") #cut all special characters
  
  #TODO this method made me stupid and not perfect because there a lot of function name not only 3
  func <- c(""," ", "poly", "log", "exp", "e", "sqrt", c(0:9)#cut white space and function name
            %>% as.character())
  
  #check ~, + operand
  opr <- str_match(formula, regx1) %>%
    str_detect("NA") %>% 
    unique()
  
  #check other words
  other <- stringr::str_split(formula, regx2) %>% 
    map(., function(x){
      x[!x%in%func & !x%in%words] 
    }) %>% flatten_chr() %>%  unique()
  
  if(!is.na(opr) & is_empty(other)){
    print("Valid Formula!")
    formula = formula
  }else{
    print("Wrong Formula!")
    formula = NULL
  }
}

variables <- function(data, formula){
  words <- names(data)
  pattern_words  <- paste0('\\b', words, '\\b', collapse = "|")
  #extract Xi from formula
  formula %>% 
    str_match_all(pattern_words)%>%
    flatten_chr() %>%
    unique() # [y, X1, X2, ...]
}

predictors <- function(data, formula){
  words <- names(data)
  variables <- variables(data, formula) # [y, X1, X2, ...]
  
  if (length(variables) == 1){ #if formula (y ~ .)
    words[!(words %in% variables)] #[X1, X2, ...]
  }else{
    variables[2:length(variables)] #[X1, X2, ...]
  }
}

#---------------------------------
data <- Boston
words <- names(data)
pattern <- ("[^a-zA-Z0-9]")
func <- c(""," ", "poly", "log", "exp", "e", "sqrt", c(0:9)#cut white space and function name
          %>% as.character())

formula <- "medv ~."
formula <- "medv ~ . "
formula <- "medv ~.-c(age, id)" #TODO this formula need regx to check
formula <- "medv !~. -c(age, id)"
formula <- "medv ~. !-c(age, id)"
formula <- "medv ~ lstat + age"

rx1 <- "[\\d\\w ]~[ \\d\\w.{ .}]" # y ~. ...
rx2 <- "[\\d\\w]( ~|~| ~ |~ )[\\d\\w|(\\ .|\\.)]"

str_view_all(formula, rx1)
str_detect(formula, rx1)
str_match(formula, rx1)

str_view_all(formula, rx2)
str_detect(formula, rx2)
str_match(formula, rx2)

valid_formular(data, formula)
variables(data, formula)
predictors(data, formula)

#---------------------------------
data <- Boston %>%
  mutate(id = row_number())

i_split <- init(data, 70)
subset <- rsample::training(i_split)$id

formula <- "medv ~ lstat"
valid_formula(data, formula)

variables(data, formula)#[y, X1, X2,...]
Xi <- predictors(data, formula)#[X1, X2, X3,...]
  
lm.fit <- model_fit(formula, data, subset)
# lm.fit <- lm(medv ~ lstat, data=Boston, subset)
summary(lm.fit)

v = c(2,7)
new_data <- v %>% 
  as_tibble(.name_repair = "unique") %>% 
  set_names(Xi)

predict(lm.fit,
        new_data,
        interval = "confidence")

y <- outcome(lm.fit, v, Xi)
value <- print(y)


