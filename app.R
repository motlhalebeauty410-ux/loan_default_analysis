# LOAN DEFAULT RISK DASHBOARD

# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)

# Load CLEANED data 
stopifnot(file.exists("data/loan_cleaned.csv"))

loan <- read.csv("data/loan_cleaned.csv", stringsAsFactors = FALSE)

# Convert key variables to factors (safety)
loan$default   <- as.factor(loan$default)
loan$gender    <- as.factor(loan$gender)
loan$education <- as.factor(loan$education)
loan$age_group <- as.factor(loan$age_group)
loan$loan_size <- as.factor(loan$loan_size)

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Loan Default Risk Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Loan Analysis", tabName = "loan", icon = icon("money-bill-wave"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      #  OVERVIEW 
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBox(
            value = sum(loan$default == 1),
            subtitle = "Total Defaults",
            icon = icon("exclamation-triangle"),
            color = "red"
          ),
          
          valueBox(
            value = sum(loan$default == 0),
            subtitle = "Non-Defaults",
            icon = icon("check-circle"),
            color = "green"
          ),
          
          valueBox(
            value = paste0(round(mean(as.numeric(as.character(loan$default))) * 100, 2), "%"),
            subtitle = "Default Rate",
            icon = icon("percent"),
            color = "yellow"
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Default Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("defaultPlot")
          ),
          
          box(
            width = 6,
            title = "Default by Gender",
            status = "info",
            solidHeader = TRUE,
            plotOutput("genderPlot")
          )
        )
      ),
      
      #  DEMOGRAPHICS 
      tabItem(
        tabName = "demographics",
        
        fluidRow(
          box(
            width = 6,
            title = "Default by Age Group",
            status = "warning",
            solidHeader = TRUE,
            plotOutput("agePlot")
          ),
          
          box(
            width = 6,
            title = "Default by Education",
            status = "success",
            solidHeader = TRUE,
            plotOutput("educationPlot")
          )
        )
      ),
      
      #  LOAN ANALYSIS 
      tabItem(
        tabName = "loan",
        
        fluidRow(
          box(
            width = 6,
            title = "Default by Loan Size",
            status = "danger",
            solidHeader = TRUE,
            plotOutput("loanSizePlot")
          ),
          
          box(
            width = 6,
            title = "Past Due Days vs Default",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("pastDuePlot")
          )
        )
      )
    )
  )
)


# SERVER
server <- function(input, output) {
  
  output$defaultPlot <- renderPlot({
    ggplot(loan, aes(x = default, fill = default)) +
      geom_bar() +
      scale_x_discrete(labels = c("0" = "Non-Default", "1" = "Default")) +
      labs(x = "Loan Status", y = "Count") +
      theme_minimal()
  })
  
  output$genderPlot <- renderPlot({
    ggplot(loan, aes(x = gender, fill = default)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion") +
      theme_minimal()
  })
  
  output$agePlot <- renderPlot({
    ggplot(loan, aes(x = age_group, fill = default)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion") +
      theme_minimal()
  })
  
  output$educationPlot <- renderPlot({
    ggplot(loan, aes(x = education, fill = default)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion") +
      theme_minimal()
  })
  
  output$loanSizePlot <- renderPlot({
    ggplot(loan, aes(x = loan_size, fill = default)) +
      geom_bar(position = "fill") +
      labs(y = "Proportion") +
      theme_minimal()
  })
  
  output$pastDuePlot <- renderPlot({
    ggplot(loan, aes(x = past_due_days, y = as.numeric(as.character(default)))) +
      geom_jitter(height = 0.05, alpha = 0.4) +
      labs(y = "Default (1 = Yes)") +
      theme_minimal()
  })
}


shinyApp(ui, server)
