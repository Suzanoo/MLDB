library(ISLR2)
library(tidyverse)

attach(Smarket)

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", scales::percent_format()(m$overall[1]),
                   ', ',
                   "Kappa", scales::percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

data <- Smarket %>%
  as_tibble()%>%
   mutate(id = row_number())

i <- data %>%
  rsample::initial_split(prop = 0.7)

train_data <- rsample::training(i)
test_data <- rsample::testing(i)

#############################################
##Logistic Regression
s <- 'Direction ~ Lag1+ Lag2'
model <- glm(as.formula(paste0(s)),
               data = Smarket,
               family = binomial,
               subset = train_data$id)

summary(model)

#calculate probability of y_predict from test data
y_probs <- predict(model, test_data, type = "response")

# retrieve response(y) from formula
y_test <- 'Direction'

# pull response(y_test) from test data
y_test <- test_data %>%
  select(matches(y_test)) %>%
  pull()

# create labels(+, -)
levels <- unique(y_test)
y_pred <- rep(levels[1], length(y_probs))
y_pred[y_probs > 0.5] = levels[2]

#render confusion matrix
cfm <- caret::confusionMatrix(y_pred, y_test)
ggplotConfusionMatrix(cfm)

####
label <- y_pred %>% unlist()

df1 <- test_data %>%
  mutate(Class = y_pred)

p1 <- ggplot(data = df1,
             aes(x=Lag1, y=Lag2))+
  geom_point(aes(color = Class,
                 shape = Class,
                 alpha = .5))+
  ggtitle("GLM fit")+
  theme(legend.position = "none")
p1

x <- c(0, 1) %>% as.factor()

