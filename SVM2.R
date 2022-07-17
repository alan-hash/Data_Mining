df <- read.csv('C:\\Users\\109141\\Desktop\\social.csv')
str(df)
sum(is.na(df$Purchase))
library(ggplot2)

ggplot(data = df, aes(x = Age, y = ..count.., fill = EstimatedSalary)) +
  geom_bar() +
  labs(title = "SVM TRAINING SET") +
  scale_fill_manual(values = c("green3", "deepskyblue1"), 
                    labels = c(" Male", "Famele")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

table(df$Purchase)
library(dplyr)
prop.table(table(df$Purchase)) %>% round(digits = 2)


library(caret)

# Índices observaciones de entrenamiento
set.seed(123)
train <- createDataPartition(y = df$Purchase, p = 0.8, list = FALSE, times = 1)

# Datos entrenamiento
datosdf_train <- df[train, ]
dim(datosdf_train)

# Datos test
datosdf_test <- df[-train, ]
dim(datosdf_test)

library(e1071)

# Optimización de hiperparámetros mediante validación cruzada 10-fold
set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosdf_train, 
               kernel = "linear", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20)), 
               scale = TRUE)
summary(tuning)
names(tuning)

ggplot(data = tuning$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))




# Almacenamos el modelo optimo obtenido y accedemos a su información
modelo_svc <- tuning$best.model
summary(modelo_svc)
head(modelo_svc$index)

modelo_svc <- svm(Purchase ~ ., data = datosdf_train, 
                  kernel = "linear", 
                  cost = 15, 
                  scale = TRUE)

plot(modelo_svc, datosdf_test, Age ~ EstimatedSalary)
plot(modelo_svc, datosdf_test, Age ~ EstimatedSalary)


predicciones = predict(modelo_svc, datosdf_test)
table(prediccion = predicciones, real = datosdf_test$Purchase)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(datosdf_test$Purchase != predicciones) %>% 
        round(digits = 4), "%")

# Configuración del proceso de selección del modelo
fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           classProbs = TRUE, 
                           search = "grid")

# Parametros del modelo disponibles
getModelInfo(model = "svmLinear")[[2]]$parameters



grid_C <- data.frame(C = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20))

# Entrenamiento del SVM con un kernel lineal y optimización del hiperparámetro C
set.seed(325) # misma semilla que en el ejemplo con el paquete e1071
modelo_svc <- train(Purchase ~ ., data = datosdf_train, 
                    method = "svmLinear", 
                    trControl = fitControl, 
                    preProc = c("center", "scale"), #estandarizacion de los datos
                    tuneGrid = grid_C)

# Resultado del entrenamiento
modelo_svc

plot(modelo_svc)
confusionMatrix(predict(modelo_svc, datosdf_test), datosdf_test$Purchase)


set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosdf_train, 
               kernel = "polynomial", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15), 
                             degree = c(2, 3)), 
               scale = TRUE)

summary(tuning)

ggplot(data = tuning$performances, aes(x = cost, y = error, col = as.factor(degree))) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C y polinomio") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(legend.position = "bottom")



# Modelo SVM kernel polinómico
modelo_svmP <- svm(Purchase ~ ., data = datosdf_train, 
                   kernel = "polynomial", 
                   cost = 15, 
                   degree = 2, 
                   scale = TRUE)

summary(modelo_svmP)

paste("Observaciones de test mal clasificadas:", 
      100 * mean(datosdf_test$Purchase != predict(modelo_svmP, datosdf_test)) %>%
        round(digits = 4), "%")


set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosdf_train, 
               kernel = "radial", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15), 
                             gamma = c(0.01, 0.1, 1, 5, 10)), 
               scale = TRUE)

summary(tuning)


ggplot(data = tuning$performances, aes(x = cost, y = error, color = factor(gamma))) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C y gamma") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")
