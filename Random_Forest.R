## packages

library(dplyr)    
library(ggplot2)
library(rsample)
library(caret)
library(ranger)   

# Access data
library(modeldata)

data("attrition")

atrit <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

set.seed(123)  # for reproducibility
atrit_split <- initial_split(atrit, prop = .7, strata = "Attrition")
atrit_train <- training(atrit_split)
atrit_test  <- testing(atrit_split)

# number of features
n_features <- length(setdiff(names(atrit_train), "Attrition"))

# train a default random forest model
atrit_rf1 <- ranger(
  Attrition ~ ., 
  data = atrit_train,
  mtry = floor(sqrt(n_features)),
  respect.unordered.factors = "order",
  seed = 123
)

(default_gini <- (atrit_rf1$prediction.error))

# create hyperparameter grid
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .10, .18, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8,.9),                       
  gini = NA                                               
)

# execute grid search
for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = Attrition ~ ., 
    data            = atrit_train, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order'
  )
  hyper_grid$gini[i] <- fit$prediction.error
}

# top 10 models
hyper_grid %>%
  arrange(gini) %>%
  mutate(perc_gain = (default_gini - gini)/default_gini * 100) %>%
  head(10)

atrit_rf1_grid <- ranger(
  formula         = Attrition ~ ., 
  data            = atrit_train, 
  num.trees       = n_features * 10,
  mtry            = 9,
  min.node.size   = 10,
  replace         = TRUE,
  sample.fraction = 0.9,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order'
) #hyperparametros que apresentaram o menor Gini no HYper_grid

atrit_rf2_grid <- ranger(
  formula         = Attrition ~ ., 
  data            = atrit_train, 
  num.trees       = n_features * 10,
  mtry            = 7,
  min.node.size   = 5,
  replace         = FALSE,
  sample.fraction = 0.5,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order'
)#hyperparametros que apresentaram o segundo menor Gini no HYper_grid

# predict class with data test
pred_class1 <- predict(atrit_rf1, atrit_test)
pred_class2 <- predict(atrit_rf1_grid, atrit_test)
pred_class3 <- predict(atrit_rf2_grid, atrit_test)

# create confusion matrix with data test
confusionMatrix(
  data = relevel(factor(pred_class1$predictions), ref = "Yes"), 
  reference = relevel(atrit_test$Attrition, ref = "Yes")
)  

confusionMatrix(
  data = relevel(factor(pred_class2$predictions), ref = "Yes"), 
  reference = relevel(atrit_test$Attrition, ref = "Yes")
)

confusionMatrix(
  data = relevel(factor(pred_class3$predictions), ref = "Yes"), 
  reference = relevel(atrit_test$Attrition, ref = "Yes")
)

#### Compare Accuracy
cbind(confusionMatrix(
  data = relevel(factor(pred_class1$predictions), ref = "Yes"), 
  reference = relevel(atrit_test$Attrition, ref = "Yes")
)$overall["Accuracy"],
confusionMatrix(
  data = relevel(factor(pred_class2$predictions), ref = "Yes"), 
  reference = relevel(atrit_test$Attrition, ref = "Yes")
)$overall["Accuracy"],
confusionMatrix(
  data = relevel(factor(pred_class3$predictions), ref = "Yes"), 
  reference = relevel(atrit_test$Attrition, ref = "Yes")
)$overall["Accuracy"]) #model 1 and 3 

#### Curva ROC
library(ROCR)

atrit_rf1_prob <- ranger(
  Attrition ~ ., 
  data = atrit_train,
  mtry = floor(sqrt(n_features)),
  respect.unordered.factors = "order",
  seed = 123,
  probability = TRUE
)

atrit_rf1_grid_prob <- ranger(
  formula         = Attrition ~ ., 
  data            = atrit_train, 
  num.trees       = n_features * 10,
  mtry            = 9,
  min.node.size   = 10,
  replace         = TRUE,
  sample.fraction = 0.9,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order',
  probability = TRUE
)
atrit_rf2_grid_prob <- ranger(
  formula         = Attrition ~ ., 
  data            = atrit_train, 
  num.trees       = n_features * 10,
  mtry            = 7,
  min.node.size   = 5,
  replace         = FALSE,
  sample.fraction = 0.5,
  verbose         = FALSE,
  seed            = 123,
  respect.unordered.factors = 'order',
  probability = TRUE
)

# Compute AUC metrics 
perf1 <- prediction(atrit_rf1_prob$predictions[,2], atrit_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf2 <- prediction(atrit_rf1_grid_prob$predictions[,2], atrit_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf3 <- prediction(atrit_rf2_grid_prob$predictions[,2], atrit_train$Attrition) %>%
  performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves 
plot(perf1, col = "black", lty = 3)
plot(perf2, add = TRUE, col = "blue",lty=2)
plot(perf3,add=TRUE,col="red")
legend(0.6, 0.5, legend = c("atrit_rf1", "atrit_rf1_grid",
                            "atrit_rf2_grid"),
       col = c("black", "blue","red"), lty = 3:1, cex = 0.6)
#Curva ROC semelhantes dos três modelos, por fim com os dados de treinamento 
# os modelos apresentaram metricas de desempenho semelhantes.
# Já com os dados de teste o modelo 1 e 3 apresentaram uma acurácia um pouco maior que o modelo 2.