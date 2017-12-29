library(tidyverse)
library(RCurl)

#' Let's grab some modelling functions!
multinom <- nnet::multinom
rpart <- rpart::rpart
randomForest <- randomForest::randomForest
svm <- e1071::svm
nb <- e1071::naiveBayes
lda <- MASS::lda

wine_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- wine_url %>% read.csv(header = FALSE)
colnames(wine) <- c(
    "cultivar", "alcohol", "malic_acid", "ash", "alcalinity_of_ash", 
    "magnesium", "total_phenols", "flavanoids", "nonflavanoid_phenols",
    "proanthocyanins", "color_intensity", "hue", "protein_concentration",
    "proline"  
)

wine <- wine %>% mutate(cultivar = cultivar %>% as.factor)

train <- sample_frac(wine, 0.8)
test <- setdiff(wine, train)

#' Train the models in the non-purrr way
model_logit <- nnet::multinom(
    formula = cultivar ~ alcohol + hue + color_intensity, 
    data = train
)
model_rf <- randomForest(
    formula = cultivar ~ alcohol + hue + color_intensity, 
    data = train
)
model_svm <- e1071::svm(
    formula = cultivar ~ alcohol + hue + color_intensity, 
    data = train
)
model_tree <- rpart::rpart(
    formula = cultivar ~ alcohol + hue + color_intensity, 
    data = train
)
model_lda <- MASS::lda(
    formula = cultivar ~ alcohol + hue + color_intensity, 
    data = train
)
model_nb <- e1071::naiveBayes(
    formula = cultivar ~ alcohol + hue + color_intensity, 
    data = train 
)

# training error
train_error_logit <- mean(
    predict(model_logit, newdata = train) != train$cultivar
)
train_error_rf <- mean(
    predict(model_rf, newdata = train) != train$cultivar
)
train_error_svm <- mean(
    predict(model_svm, newdata = train) != train$cultivar
)
train_error_tree <- mean(
    predict(model_tree, newdata = train, type = "class") != train$cultivar
)
train_error_lda <- mean(
    predict(model_lda, newdata = train)$class != train$cultivar
)
train_error_nb <- mean(
    predict(model_nb, newdata = train) != train$cultivar
)

# validation error
val_error_logit <- mean(
    predict(model_logit, newdata = test) != test$cultivar
)
val_error_rf <- mean(
    predict(model_rf, newdata = test) != test$cultivar
)
val_error_svm <- mean(
    predict(model_svm, newdata = test) != test$cultivar
)
val_error_tree <- mean(
    predict(model_tree, newdata = test, type = "class") != test$cultivar
)
val_error_lda <- mean(
    predict(model_lda, newdata = test)$class != test$cultivar
)
val_error_nb <- mean(
    predict(model_nb, newdata = test) != test$cultivar
)

#' purrr approach
#' The braces around the `invoke_map` prevent the tribble from being used as the
#' first argument of invoke_map. We instead refer to the tribble with a `.`
common_params <- list(
    formula = cultivar ~ alcohol + hue + color_intensity,
    data = train
)

models <- tribble(
    ~func, ~params,
    "multinom", common_params,
    "randomForest", common_params,
    "svm", common_params,
    "rpart", common_params,
    "lda", common_params,
    "nb", common_params
) %>% {invoke_map(.$func, .$params)}

