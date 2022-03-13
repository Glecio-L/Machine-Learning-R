library(dplyr) # for data wrangling
library(ggplot2) # for awesome plotting
library(rsample) # for data splitting
# Modeling packages
library(caret) # for logistic regression modeling
library(vip) # variable importance

# Access data
library(modeldata)

data("attrition")

df <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

set.seed(123) # for reproducibility
data_split <- initial_split(df, prop = .7, strata = "Attrition")
data_train <- training(data_split)
data_test <- testing(data_split)

set.seed(123)
cv_model1 <- train(
  Attrition ~ MonthlyIncome,
  data = data_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)

set.seed(123)
cv_model2 <- train(
  Attrition ~ MonthlyIncome + OverTime,
  data = data_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
set.seed(123)
cv_model3 <- train(
  Attrition ~ .,
  data = data_train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = cv_model1,
      model2 = cv_model2,
      model3 = cv_model3
    )
  )
)$statistics$Accuracy # Model 3 com a melhor média


pred_class <- predict(cv_model3, data_train)
# create confusion matrix
confusionMatrix(
  data = relevel(pred_class, ref = "Yes"),
  reference = relevel(data_train$Attrition, ref = "Yes")
)

library(ROCR)

m3_prob <- predict(cv_model3, data_train, type = "prob")$Yes

perf <- prediction(m3_prob, data_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")

plot(perf, col = "black", lty = 2) #curva ROC
legend(0.8, 0.2, legend = c("cv_model3"),
       col = "black", lty = 2:1, cex = 0.6)

vip(cv_model3, num_features = 20) # Variáveis que mais contribuiram para o modelo
