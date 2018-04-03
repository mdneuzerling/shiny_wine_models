library(shiny)
library(dplyr)

#' Let's grab some modelling functions!
multinom <- nnet::multinom
rpart <- rpart::rpart
randomForest <- randomForest::randomForest
svm <- e1071::svm
nb <- e1071::naiveBayes
lda <- MASS::lda

wine <- rename_all(readRDS("data/wine.rds"), tolower)

wine$cultivar <- as.factor(wine$cultivar) 

shinyServer(function(input, output) {

train <- sample_frac(wine, 0.8)
test <- setdiff(wine, train)

# training error
error <- function(model, evaluation_data, special_case = "") {
    if (special_case == "rpart") {
        mean(
            predict(model, newdata = evaluation_data, type = "class") != 
                evaluation_data$cultivar
        )
    } else if (special_case == "lda") {
        mean(
            predict(model, newdata = evaluation_data)$class != 
                evaluation_data$cultivar
        )
    } else {
        mean(
            predict(model, newdata = evaluation_data) != 
                evaluation_data$cultivar
        )
    } 
}

construct_models <- function(var1, var2 = "<none>", var3 = "<none>") {
    form <- paste0("cultivar ~ ", var1)
    if (var2 != "<none>") {
        form <- form %>% paste0(" + ", var2)
    }
    if (var3 != "<none>") {
        form <- form %>% paste0(" + ", var3)
    }
    
    model_logit <- nnet::multinom(
        formula = as.formula(form), 
        data = train
    )
    model_rf <- randomForest(
        formula = as.formula(form), 
        data = train
    )
    model_svm <- e1071::svm(
        formula = as.formula(form), 
        data = train
    )
    model_tree <- rpart::rpart(
        formula = as.formula(form), 
        data = train
    )
    model_lda <- MASS::lda(
        formula = as.formula(form), 
        data = train
    )
    model_nb <- e1071::naiveBayes(
        formula = as.formula(form), 
        data = train 
    )

    errors <- data.frame(
        "error" = c("training", "validation"),
        "logistic" = c(error(model_logit, train), error(model_logit, test)),
        "random_forest" = c(error(model_rf, train), error(model_rf, test)),
        "svm" = c(error(model_svm, train), error(model_svm, test)),
        "decision_tree" = c(error(model_tree, train, "rpart"), error(model_tree, test, "rpart")),
        "lda" = c(error(model_lda, train, "lda"), error(model_lda, test, "lda")),
        "naive_bayes" = c(error(model_nb, train), error(model_nb, test))
    )

    return(errors)
}
 
    output$errors_a <- renderTable(
        construct_models(input$var_a1, input$var_a2, input$var_a3)
    )
    
    output$errors_b <- renderTable(
        construct_models(input$var_b1, input$var_b2, input$var_b3)
    )

    output$errors_c <- renderTable(
        construct_models(input$var_c1, input$var_c2, input$var_c3)
    )

    output$errors_d <- renderTable(
        construct_models(input$var_d1, input$var_d2, input$var_d3)
    )
    
}
)