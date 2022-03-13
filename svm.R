#packages
library(dplyr) # for data wrangling
library(ggplot2) # for awesome graphics
library(rsample) # for data splitting
# Modeling packages
library(caret) # for classification and regression training
library(kernlab) # for fitting SVMs
# Model interpretability packages
library(pdp) # for partial dependence plots, etc.
library(vip) # for variable importance plots


# Access data
library(modeldata)

data("attrition")

atrit <- attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

set.seed(123)  # for reproducibility
atrit_split <- initial_split(atrit, prop = .7, strata = "Attrition")
atrit_train <- training(atrit_split)
atrit_test  <- testing(atrit_split)



# Control params for SVM
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary # also needed for AUC/ROC
)
# Tune an SVM
set.seed(5628) # for reproducibility
atrit_svm_auc <- train(
  Attrition ~ .,
  data = atrit_train,
  method = "svmRadial",
  preProcess = c("center", "scale"),
  metric = "ROC", # area under ROC curve (AUC)
  trControl = ctrl,
  tuneLength = 10
)

atrit_svm_auc$results

confusionMatrix(atrit_svm_auc)

prob_yes <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, "Yes"]
}

# Variable importance plot
set.seed(2827) # for reproducibility
vip(atrit_svm_auc, method = "permute", nsim = 5, train = atrit_train,
    target = "Attrition", metric = "auc", reference_class = "Yes",
    pred_wrapper = prob_yes)

## Agora, vamos interpretar as quatro variaveis que mais contribuiram para o modelo, com base no VIP.

features <- c("OverTime", "EducationField",
               "BusinessTravel", "JobRole")
pdps <- lapply(features, function(x) {
  partial(atrit_svm_auc, pred.var = x, which.class = 2,
          prob = TRUE, plot = TRUE, plot.engine = "ggplot2") +
    coord_flip()
})
grid.arrange(grobs = pdps, ncol = 2)

## Analisando essas variáveis percebe-se que os pontos mais proximo a esquerda do gráfico
## são aqueles que tem uma maior probabilidade de deixar o emprego, por exemplo, na variável
## BuninessTravel quem viaja frequentemente tem a maior probabilidade de deixar o emprego.