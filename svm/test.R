library(e1071)

data <- Smarket %>% 
  mutate(across(where(is.character),
                as.factor))

split <- init_split(data, 70)
y <- "Direction"

formula <- paste0(y, " ~ .")

tune.out <- e1071::tune(
  svm,
  as.formula(formula),
  data = rsample::training(split),
  kernel = "radial",
  ranges = list(
    cost = c(.001, .01, .1, 1, 5, 10, 100),
    gamma = c(.5, 1, 2, 3, 4)
  ))

summary(tune.out)[1:2]

y_test <- rsample::testing(split) %>%
    select(matches(y)) %>% # fetch y from [y, X1, X2, ...] 
    pull() # [y]


y_hat <- predict(tune.out$best.model, rsample::training(split))

cfm <- caret::confusionMatrix(y_hat, y_test)
ggplotConfusionMatrix(cfm)

#-------------
data <- rsample::training(split)

Xi <- c("Lag1", "Lag2", y)
gamma_ <- tune.out$best.parameters$gamma
cost_ <- tune.out$best.parameters$cost

data <- data %>% 
  select(matches(Xi))

# svmfit <- svm(as.formula(formula), data = data, kernel = "radial",
#               gamma = gamma_, cost = cost_)

svm.fit(formula, data, 
                    cost = cost_,
                    gamma = gamma_)

y_hat <- model_predict(svmfit, data)



plot(svmfit, data)

model_plot(Xi, data, svmfit)





