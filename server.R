library(shiny)
library(clue)

# You can use kmeans for simplicity in this example
set.seed(123)
# Load the k-means model from th46e file
loaded_model <- readRDS("kmeans_result.rds")

# Helper function to generate error messages with styling
generateErrorUI <- function(errorMessage) {
  if (!is.null(errorMessage)) {
    div(
      style = "color: #990000; font-size: 12px; ",
      errorMessage
    )
  }
}

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$submit, {
    # Initialize error messages
    error_age <- NULL
    error_income <- NULL
    error_spending <- NULL
    
    # Check if inputs are within the specified range
    if (input$age < 18 || input$age > 66) {
      error_age <- "Error: Age must be between 18 and 66."
    }
    
    if (input$income < 15 || input$income > 200) {
      error_income <- "Error: Annual Income must be between 15 and 200 (k$)."
    }
    
    if (input$spending < 1 || input$spending > 100) {
      error_spending <- "Error: Spending Score must be between 1 and 100."
    }
    
    # Display error messages with custom styling
    output$error_age <- renderUI({
      generateErrorUI(error_age)
    })
    
    output$error_income <- renderUI({
      generateErrorUI(error_income)
    })
    
    output$error_spending <- renderUI({
      generateErrorUI(error_spending)
    })
    
    # If there are no errors, proceed with predictions
    if (is.null(error_age) && is.null(error_income) && is.null(error_spending)) {
      # Get user inputs
      user_data <- data.frame(Age = input$age, Annual_Income = input$income, Spending_Score = input$spending)
      
      # Predict clusters for the new data using cl_predict
      user_data$Cluster <- cl_predict(loaded_model, user_data)
      
      # Display the cluster result with custom styling
      output$result <- renderUI({
        tags$p(
          "Predicted Cluster: ",
          tags$strong(user_data$Cluster),
          style = "font-weight: bold; font-size: 18px; margin-top: 10px;"
        )
      })
    } else {
      # Clear the result if there are errors
      output$result <- renderUI({
        NULL
      })
    }
  })
}


