### Package

library(rsample) # for resampling procedures
library(caret) # for cross-validation, etc.
library(dplyr) # for data manipulation

###### Regression method
# Access data
ames = AmesHousing::make_ames()

## Sampling with the rsample package

set.seed(123) # for reproducibility
split = initial_split(ames, prop = 0.7, strata = "Sale_Price")

ames_train = training(split)
ames_test = testing(split)

## Train model using 10-fold cross-validation

set.seed(123) # for reproducibility
(cv_model1 = train(
  form = Sale_Price ~ Gr_Liv_Area,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))



# model 2 CV
set.seed(123) # for reproducibility

(cv_model2 = train(
  form = Sale_Price ~ Gr_Liv_Area + Year_Built,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

# model 3 - include all possible

set.seed(123) # for reproducibility
(cv_model3 <- train(
  Sale_Price ~ .,
  data = ames_train,
  method = "lm",
  trControl = trainControl(method = 'cv', number = 10)
))




summary(resamples(list(model1=cv_model1, model2=cv_model2, model3=cv_model3)))

# Extraindo os resultados para cada modelo, vemos que ao adicionar mais 
# informações por meio de mais preditores, podemos melhorar a performace.
# Especificamente, o RMSE que foi diminuindo conforme aumentava as preditoras e
# assim temos o model 3 com o RMSE mais baixo entre os treinados.


#Algumas medidas de diagnostico

# a função broom ajuda na análise dos valores previstos com os seus residuos
# vamos comparar o model 1 com o model 3.

df1 = broom::augment(cv_model1$finalModel, data = ames_train)

p1 = ggplot(df1, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Sale_Price ~ Gr_Liv_Area") + theme_minimal()

df2 = broom::augment(cv_model3$finalModel, data = ames_train)

p2 = ggplot(df2, aes(.fitted, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Sale_Price ~ .") + theme_minimal()


gridExtra::grid.arrange(p1, p2, nrow = 1)

# A regressão linear assume variância constante entre os resíduos.
# O model 3 parece ter variância constante, diferente do model 1.

# Autocorrelação

df1 <- mutate(df1, id = row_number())

df2 <- mutate(df2, id = row_number())

p1 <- ggplot(df1, aes(id, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.") + theme_minimal()

p2 <- ggplot(df2, aes(id, .std.resid)) +
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Uncorrelated residuals.") + theme_minimal()

gridExtra::grid.arrange(p1, p2, nrow = 1)

# A regressão linear assume erros não correlacionados.
# model3 não tem sinais de autocorrelação.

predicts = predict(cv_model3, newdata = ames_test) #aplicando nos dados de teste

data.gg = as.data.frame(cbind(x = 1:length(ames_test$Sale_Price),
                              predicts = predicts,
                              test = ames_test$Sale_Price))

# Gráfico dos valores previstos (linha vermelha) com os valores reais
options(scipen = 999)
ggplot(data=data.gg, aes(x=x, y=test)) +
  geom_line(col="#006666") +
  ylab("Preços de Vendas") + xlab("") +
  geom_line(aes(x=x,y=predicts), col="red") + theme_minimal()





