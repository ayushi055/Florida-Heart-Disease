library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(xgboost)


ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Statistics", tabName = "heart_disease", icon = icon("heartbeat")),
      menuItem("Risk Prediction", tabName = "risk", icon = icon("heart-circle-exclamation")),
      menuItem("Resources", tabName = "resources", icon = icon("heart-circle-check")),
      selectInput("year", "Select Year", choices = 2016:2022, selected = 2022),
      uiOutput("county_ui")
    )
  ),
  
  dashboardBody(
    # Heart Disease Stats Tab
    tabItems(
      tabItem(tabName = "heart_disease",
              fluidRow(
                column(width = 12, 
                       h3("Heart Disease Statistics"),
                       p("Select a year and county from the dropdown menus to 
                         view statistics on heart disease death rates. 
                         The map will display regional death rates, and the 
                         plot will show trends in death rates over the selected year."),
                       leafletOutput("heart_disease_map", height = 500))
              ),
              fluidRow(
                column(width = 12, 
                       plotOutput("death_rate_plot", height = 400)) 
              )
      ),
      
      # Risk Prediction Tab
      tabItem(tabName = "risk",
              fluidRow(
                column(width = 6,
                       h3("Heart Disease Risk Prediction"),
                       
                       # Age Input with description
                       numericInput("age_input", "Age", value = 50, min = 18, max = 100),
                       p("Age is a major risk factor for heart disease. The older you are, the higher your risk."),
                       
                       # Sex Input with description
                       selectInput("sex_input", "Sex", choices = c("Male", "Female"), selected = "Male"),
                       p("Sex can influence the likelihood of developing heart disease. Men tend to develop heart disease earlier than women."),
                       
                       # Cholesterol Level Input with description
                       numericInput("chol_input", "Total Cholesterol Level (mg/dL)", value = 200, min = 100, max = 400),
                       p("Cholesterol is a fatty substance that can build up in your blood vessels. High levels increase your risk of heart disease."),
                       
                       # Resting Blood Pressure Input with description
                       numericInput("trestbps_input", "Resting Blood Pressure (mmHg)", value = 120, min = 50, max = 200),
                       p("Blood pressure measures the force of blood against the walls of your arteries. High blood pressure can lead to heart disease."),
                       
                       # Exercise Induced Angina Input with description
                       selectInput("exang_input", "Exercise Induced Angina", choices = c("No", "Yes"), selected = "No"),
                       p("Angina is chest pain caused by reduced blood flow to the heart. Exercise-induced angina may be a sign of heart disease."),
                       
                       # Prediction Button
                       actionButton("predict_btn", "Predict Risk")
                ),
                
                column(width = 6,
                       h3("Prediction Result"),
                       
                       # Display Prediction Result Text
                       textOutput("prediction_result"),
                       
                       # Additional description below the result
                       br(),
                       p("Review the input values and click the 'Predict Risk' button to see your estimated risk of heart disease."),
                       p("If your risk is high, consider talking to a healthcare provider for further evaluation and preventive measures.")
                )
              )
      ),
      
      
      # Heart Disease Resources Tab
      tabItem(tabName = "resources",
              fluidRow(
                column(width = 12,
                       h3("Heart Disease Resources"),
                       p("Here you can find valuable resources to help you better 
                         understand heart disease, its prevention, treatment, and support options. 
                         These resources include trusted organizations that provide educational materials, guidelines, and community support."),
                       
                       # Description for the American Heart Association link
                       p(strong("American Heart Association (AHA):")),
                       p("The American Heart Association is one of the leading organizations dedicated to fighting heart disease and stroke. The AHA offers a wealth of resources, including heart health tips, patient guides, and ways to get involved in heart disease prevention."),
                       tags$ul(
                         tags$li(a("Visit the American Heart Association", href = "https://www.heart.org/", target = "_blank"))
                       ),
                       
                       # Description for the CDC link
                       p(strong("Centers for Disease Control and Prevention (CDC) - Heart Disease:")),
                       p("The CDC provides reliable, up-to-date information on the state of heart disease in the U.S., including statistical data, prevention tips, and health recommendations. Their resources focus on reducing the burden of cardiovascular disease in the population."),
                       tags$ul(
                         tags$li(a("Visit the CDC Heart Disease page", href = "https://www.cdc.gov/nchs/fastats/heart-disease.htm", target = "_blank"))
                       ),
                       
                       # Description for the National Heart, Lung, and Blood Institute link
                       p(strong("National Heart, Lung, and Blood Institute (NHLBI):")),
                       p("The NHLBI provides resources for both the general public and healthcare professionals. It includes heart disease prevention strategies, research initiatives, and comprehensive guides on diagnosis and treatment options."),
                       tags$ul(
                         tags$li(a("Visit the NHLBI Heart Disease page", href = "https://www.nhlbi.nih.gov/health-topics/heart-disease", target = "_blank"))
                       )
                )
              )
      )
    )
  )
)
