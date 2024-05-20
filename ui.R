library(shiny)
library(clue)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Cluster Profiling App", windowTitle = "Cluster Software"),
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #f4f4f4;
          font-family: 'Arial', sans-serif;
        }
        .btn-primary {
          background-color: #4CAF50;
          border: none;
          color: white;
          padding: 10px 20px;
          text-align: center;
          text-decoration: none;
          display: inline-block;
          font-size: 16px;
          margin: 4px 2px;
          margin-top: 15px;
          cursor: pointer;
          border-radius: 5px;
        }
        .form-group{
        margin-top: 10px;
        margin-bottom: 5px;
        }
      ")
    )
  ),
  numericInput("age", "Enter Age:", value = 0, min = 18, max = 66),
  uiOutput("error_age"),
  numericInput("income", "Enter Annual Income (k$):", value = 0, min = 15, max = 200),
  uiOutput("error_income"),
  numericInput("spending", "Enter Spending Score (1-100):", value = 0, min = 1, max = 100),
  uiOutput("error_spending"),
  actionButton("submit", "Submit", class = "btn-primary"),
  uiOutput("result")
)