rm(list=ls())
# library(ISLR2)
# library(tidyverse)

attach(Smarket)
names(Smarket) #see header

head(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[,-9]) #create matrix(data frame) exclude qualitative column

plot(Volume)

#############################################
train <- (Year <  2005) #index
Smarket.2005 <- Smarket[!train, ] #X_test
Direction.2005 <- Direction[!train] #y_test

df <- data.frame(Smarket.2005[c('Lag1', 'Lag2')], Direction.2005)
names(df) <- c('Lag1', 'Lag2', 'Class')

#############################################
##Logistic Regression
glm.fit <- glm(Direction ~ Lag1+ Lag2,
               data = Smarket,
               family = binomial,
               subset = train)
summary(model)

glm.probs <- predict(glm.fit, Smarket.2005, type = "response") #y_predict

#transform numerical labels to strings labels
glm.pred <- rep("Down", 252)#create labels vector
glm.pred[glm.probs > 0.5] = "Up"#transforms to "Up" if match prob>0.5

#Confusion matrix
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

#
predict(glm.fit,
        newdata = data.frame(
          Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8), type="response"
        ))

#
df1 <- mutate(Smarket.2005,  "Class"=glm.pred)
p1 <- ggplot(data = df1,
             aes(x=Lag1, y=Lag2))+
  geom_point(aes(color = Class,
                 shape = Class,
                 alpha = .5))+
  ggtitle("GLM fit")+
  theme(legend.position = "none")
p1

#############################################
##LDA
# library(MASS)
lda.fit = MASS::lda(Direction ~ Lag1+ Lag2, data = Smarket, subset = train)
lda.fit

lda.fit.pred <- predict(lda.fit, Smarket.2005)
names(lda.fit.pred)

lda.fit.class <- lda.fit.pred$class

table(lda.fit.class, Direction.2005)
mean(lda.fit.class == Direction.2005)

#Apply threshold 50%, 80%
sum(lda.fit.pred$posterior[, 1] >= 0.5)
sum(lda.fit.pred$posterior[, 1] < 0.5)
sum(lda.fit.pred$posterior[, 1] >= 0.8)

lda.fit.pred$posterior[1:20, 1]
lda.fit.pred$class[1:20]

plot(lda.fit.pred$posterior[,1])

predict(lda.fit,
        newdata = data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type="response"
)

df2 <- mutate(Smarket.2005, "Probs"=lda.fit.pred$posterior[, 2], "Class"=lda.fit.pred$class)
p2 <- ggplot(data = df2,
             aes(x=Lag1, y=Lag2))+
  geom_point(aes(color = Class,
                 shape = Class,
                 alpha = .5))+
  ggtitle("LDA")+
  theme(legend.position = "none")
p2

#############################################
##QDA 
qda.fit <- MASS::qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.fit.pred <- predict(qda.fit, Smarket.2005)
qda.class <- qda.fit.pred$class

table(qda.fit.class, Direction.2005)
mean(qda.fit.class == Direction.2005)

predict(qda.fit,
        newdata = data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type="response"
)

df3 <- mutate(Smarket.2005, "Probs"=qda.fit.pred$posterior[, 2], "Class"=qda.fit.pred$class)
p3 <- ggplot(data = df3,
             aes(x=Lag1, y=Lag2))+
  geom_point(aes(color = Class,
                 shape = Class,
                 alpha = .5))+
  ggtitle("QDA")+
  theme(legend.position = "none")
p3

library(patchwork)

p1|p2|p3 

#############################################
##Naive Bayes
# library(e1071)
nb.fit <- e1071::naiveBayes(Direction ~ Lag1+ Lag2, data = Smarket, subset = train)
nb.fit

nb.fit.pred <- predict(nb.fit, Smarket.2005)

table(nb.fit.pred, Direction.2005)
mean(nb.fit.pred == Direction.2005)

nb.fit.pred2 <- predict(nb.fit, Smarket.2005, type = "raw")
nb.fit.pred2[1:5, ]

predict(nb.fit,
        newdata = data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type = "raw")

df4 = mutate(Smarket.2005, "Class"=nb.fit.pred )

#--------------------------------------------------
#New data
set.seed((112))
xx1 <- runif(1000, -5, 5)
xx2 <- runif(1000, -5, 5)

yy <- predict(nb.fit,
              newdata = data.frame(Lag1=xx1, Lag2=xx2),
              type = "raw")

lb <- rep("Down", 1000)#create labels vector placeholder
lb[yy[, 2] > 0.5] = "Up"#transforms to "Up" if match prob>0.5

df5 <- data.frame(xx1, xx2, lb)
names(df5) <- c('Lag1', 'Lag2', 'Class')

#--------------------------------------------------
p4 <- ggplot()+
  #train data
  geom_point(data = df,
             aes(x = Lag1, y = Lag2,
                 shape = 'circle',
                 color = Class))+
  #test data
  # geom_point(data = df4,
  #            aes(x = Lag1, y = Lag2,
  #                shape = 'circle',
  #                color = Class))+
  #new data to see curve
  geom_point(data = df5,
             aes(x = Lag1, y = Lag2,
                 shape = 'circle',
                 color = Class,
                 alpha = 0.01))+
  ggtitle("Naive Bayes")+
  theme(legend.position = "none")

p4

#--------------------------------------------------
(p1|p2)/(p3|p4) +
  plot_annotation(
    title = "Classification fit",
    
  )

#############################################
##K-Nearest Neighbors
X_train <- cbind(Lag1, Lag2)[train, ]
X_test <- cbind(Lag1, Lag2)[!train, ]
y_train <- Direction[train]
y_test <- Direction[!train]

plotx <- function(x, y, labels, title){
  df <- data.frame("Lag1" = x,
                   "Lag2" = y,
                   "Class" = labels)
  ggplot(data = df,
         aes(x=Lag1, y=Lag2))+
    geom_point(aes(color = Class,
                   shape = Class,
                   size = 6, alpha = .5))+
    ggtitle(title)
}

#Random observation to protect nearest neighbors tied effect
set.seed(1)

# library(class)
knn.pred <- class::knn(X_train, X_test, y_train, k=1)
table(knn.pred, y_test)
mean(knn.pred == y_test)

p5 <- plotx(X_test[, 1], X_test[, 2], knn.pred, "KNN: k=1")

#---------------------------------------------
knn.pred3 <- class::knn(X_train, X_test, y_train, k=3)
table(knn.pred3, y_test)
mean(knn.pred3 == y_test)

p6 <- plotx(X_test[, 1], X_test[, 2], knn.pred3, "KNN: k=3")

#---------------------------------------------
knn.pred10 <- class::knn(X_train, X_test, y_train, k=10)
table(knn.pred10, y_test)
mean(knn.pred10 == y_test)

p7 <- plotx(X_test[, 1], X_test[, 2], knn.pred10, "KNN: k=10")

p5/p6/p7

#############################################
set.seed((112))
xx1 <- runif(1000, -5, 5)
xx2 <- runif(1000, -5, 5)

yy <- predict(nb.fit,
              newdata = data.frame(Lag1=xx1, Lag2=xx2),
              type = "raw")
lb <- rep("Down", 1000)#create labels vector
lb[yy[, 2] > 0.5] = "Up"#transforms to "Up" if match prob>0.5

df <- data.frame(xx1, xx2, lb)
names(df) <- c('Lag1', 'Lag2', 'Class')

p4 <- ggplot(data = df,
             aes(x=Lag1, y=Lag2,
                 color = Class))+
  geom_point(aes(shape = Class,
                 alpha = .5))+
  ggtitle("Naive Bayes")+
  theme(legend.position = "none")

p4


set.seed((112))
x_dummy1 <- runif(1000, -5, 5)
x_dummy2 <- runif(1000, -5, 5)

new_data <- tibble(x_dummy1, x_dummy2)
y_dummy <- rep('Up', 1000)

new_data %>%
  bind_cols(y_dummy)%>%
  set_names(c('x', 'y', 'Class'))



