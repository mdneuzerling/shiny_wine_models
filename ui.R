library(shiny)

selections <- c(
    "alcohol", "malic_acid", "ash", "alcalinity_of_ash", 
    "magnesium", "total_phenols", "flavanoids", "nonflavanoid_phenols",
    "proanthocyanins", "color_intensity", "hue", "protein_concentration",
    "proline"
)

selection_row <- function(label) {
        fluidRow(
        column(4,
            selectInput(
                paste0("var_", label, "1"), label = NULL, 
                choices = selections, 
                selected = "alcohol"
            ),
            selectInput(paste0("var_", label, "2"), 
                label = NULL, 
                choices = c("<none>", selections), 
                selected = "hue"
            ),
            selectInput(
                paste0("var_", label, "3"), 
                label = NULL, 
                choices = c("<none>", selections), 
                selected = "color_intensity"  
            )
        ),
        
        column(8,
          tableOutput(paste0("errors_", label))
        )
    )
}

shinyUI(fluidPage(

    titlePanel("Wine models"),
    fluidRow(
        column(12,
            p("Results of a chemical analysis of wines
              grown in the same region in Italy but from
              three different cultivars. Data from the " ,
              a(href = "https://archive.ics.uci.edu/ml/datasets/Wine", 
                "UCI Machine Learning Repository.")),
            
            p("Select between 1 and 3 variables to use in training 
              the models, and compare the training and validation accuracy of 
              various common models. In practice, we would compare not only 
              different models, but different parameters used to adjust 
              those models.")
        )
    ),

    selection_row("a"), 
        hr(),
    selection_row("b"),
        hr(),
    selection_row("c"),
        hr(),
    selection_row("d")

))
    
    
    
