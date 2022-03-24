attach(Boston)
data <- Boston
head(Boston)

x <- c('medv', 'lstat', 'age')
data %>%
  select(matches(x)) %>%
        ggcorr()

