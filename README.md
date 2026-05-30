R Shiny App: Interactive Map of Heart Disease Death Rates In Florida and
Risk Prediction
================
Ayushi Jain
2024-12-05

App URL: <https://ayushij.shinyapps.io/florida/>

Note: Please allow a few seconds for the elements to load.

## Introduction

Heart disease remains one of the leading causes of death globally,
affecting millions of individuals each year \[1\]. While advancements in
medical science and treatment have improved outcomes for many, the
prevention and management of heart disease still largely depends on
early detection, awareness, and lifestyle modifications. Despite the
wealth of information available, many individuals struggle to understand
the personal risks they face, and the connection between local trends in
heart disease and their own health choices can be unclear. The Heart
Disease R Shiny app was created with the goal of informing individuals
in Florida of the prevalence and risk factors of heart disease. By
providing easy access to regional statistics and interactive tools for
heart disease risk prediction, this app seeks to raise awareness and
guide informed decision-making.

Key Motivations:

1)  Access to Localized Health Data: The app allows users to explore
    heart disease statistics specific to their county and year of choice
    via an interactive map and line graph, offering valuable insights
    into local trends and potential risk factors. This localized data
    can help individuals, communities, and healthcare professionals
    better understand regional variations in heart disease rates,
    fostering more targeted prevention and intervention strategies.

2)  Personalized Risk Prediction: In addition to viewing regional
    statistics, the app features a risk prediction tool that allows
    users to input personal health data, such as age, cholesterol
    levels, and blood pressure, to estimate their own risk of developing
    heart disease. This personalized feedback can help individuals
    recognize the importance of regular check-ups, early screenings, and
    adopting healthier lifestyles.

3)  Promoting Education and Awareness: The app also serves as an
    educational resource, providing users with trusted information from
    reputable organizations like the American Heart Association and the
    Centers for Disease Control and Prevention (CDC). By learning about
    the risk factors for heart disease and understanding how to manage
    them, users can make informed decisions about their health.

In summary, the Heart Disease app seeks to bridge the gap between raw
health data and individual awareness, offering a tool for both personal
health management and community-wide prevention efforts.

## Methods

#### Statistics Tab

The primary dataset used in the application, heartdisease.csv, contains
information on heart disease death rates across various counties in
Florida over several years from the Florida Department of Health \[2\].
County names were adjusted to match the formatting used in other data
sets. Key variables include:

- County: The name of the county in Florida.
- Year: The year the data corresponds to.
- Rate: The heart disease death rate per 100,000 people for the
  corresponding county and year.

A shapefile of Florida counties (cntbnd_sep15) obtained from the Florida
Geographic Data Library was used for spatial visualization \[3\]. This
shapefile contains geographic boundaries for each county, which is
merged with the heart disease data to allow for geographic mapping of
death rates. The shapefile was loaded and reprojected to the EPSG:4326
coordinate reference system for compatibility with web mapping tools.

``` r
server <- function(input, output, session) {
  
  fl_county_shapefile <- st_read("cntbnd_sep15")
  fl_county_shapefile <- st_transform(fl_county_shapefile, crs = 4326)
  fl_county_shapefile$County <- fl_county_shapefile$TIGERNAME
  
  flhealthcharts <- read.csv("heartdisease.csv")
  flhealthcharts[flhealthcharts == "Saint Johns"] <- "St. Johns"
  flhealthcharts[flhealthcharts == "Saint Lucie"] <- "St. Lucie"
  flhealthcharts[flhealthcharts == "Desoto"] <- "DeSoto"
```

The heart disease data was then filtered by year using a reactive
expression so the data could be visualized separately for each year and
this dataset was merged with the shapefile to plot the map. The map was
rendered using the ‘leaflet’ package.

``` r
# Reactive data filtered by year
  filtered_data <- reactive({
    flhealthcharts %>%
      filter(Year == input$year)
    })
  
  # Reactive for joining the shapefile with Heart Disease data
  merged_data <- reactive({
    # Merge the shapefile with the Heart Disease data by County
    data <- filtered_data()
    merged <- left_join(fl_county_shapefile, data, by = "County")
    return(merged)
  })
  
  # Render Leaflet map with Heart Disease rates for the selected year
  output$heart_disease_map <- renderLeaflet({
    data <- merged_data()
    
    # Define color palette for Heart Disease rates
    pal <- colorNumeric(palette = "YlOrRd", domain = data$Rate)
    
    # Create a leaflet map
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Rate),
        fillOpacity = 0.8,
        weight = 1,
        popup = ~paste(County, "<br>Death Rate: ", Rate)
      ) %>%
      addLegend(
        pal = pal, values = ~Rate, opacity = 0.8,
        title = "Death by Heart Disease Rate per 100,000", position = "bottomright"
      ) 

  })
```

A popup function was added to display the death rate for the selected
county.

``` r
# Render the county dropdown based on the filtered year
  output$county_ui <- renderUI({
    counties <- unique(filtered_data()$County)
    selectInput("county", "Select County", choices = counties)
  })
  
  # Observer to update the map with the popup for the selected county
  observeEvent(input$county, {
    # Get the selected county's geometry
    data <- merged_data()
    selected_data <- data %>% filter(County == input$county)
    
    if (nrow(selected_data) > 0) {
      selected_geom <- st_geometry(selected_data)[[1]]  # Get the first geometry
      
      # Get the centroid
        centroid <- st_centroid(selected_geom)
        coords <- st_coordinates(centroid)  # Get the coordinates of the centroid
        
        # Add a popup for the selected county
        leafletProxy("heart_disease_map") %>%
          clearPopups() %>%  # Clear any existing popups
          addPopups(
            lng = coords[1], 
            lat = coords[2], 
            popup = paste(input$county, "<br>Death Rate: ", selected_data$Rate)
          )
    }
  })
```

For the selected county, a line plot was generated showing the trend of
heart disease death rates over the years. This plot wass rendered using
‘ggplot2’, with data filtered by county. Points are plotted over the
line to highlight individual years, and the plot is dynamically updated
based on user input.

``` r
# Reactive expression to filter data by selected county for plotting
  filtered_county_data <- reactive({
    req(input$county)  # Ensure county is selected before proceeding
    flhealthcharts %>%
      filter(County == input$county)
  })
  
  # Render line plot for the selected county's death rates over the years
  output$death_rate_plot <- renderPlot({
    data <- filtered_county_data()
    
    # Check if data is available for plotting
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Year, y = Rate, group = 1)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        labs(title = paste("Death Rate for Heart Disease in", input$county),
             x = "Year", y = "Death Rate per 100,000") +
        theme_minimal()
    }
  })
```

#### Risk Predictions Tab

The app uses a pre-trained XGBoost model (model.rds) for heart disease
risk prediction. The model was trained using key health indicators (age,
sex, cholesterol level, blood pressure, and exercise-induced angina) to
predict the likelihood of developing heart disease. The data for
training the model was obtained from the UCI Machine Learning Repository
Heart Disease Dataset \[4\]. The model included only variables from the
dataset that were practical for users to input on their own and
variables that were determined to be significant for the prediction.

``` r
# Load the pre-trained XGBoost model 
  model <- readRDS(file = "model.rds")
  
  # Create the prediction when the button is clicked
  observeEvent(input$predict_btn, {
    
    # Prepare input data for prediction
    input_data <- data.frame(
      sex = as.numeric(ifelse(input$sex_input == "Male", 1, 0)), # Convert 'Male' to 1, 'Female' to 0
      age = as.numeric(input$age_input),
      trestbps = as.numeric(input$trestbps_input),
      chol = as.numeric(input$chol_input),
      exang = as.numeric(ifelse(input$exang_input == "Yes", 1, 0))  # Convert 'Yes' to 1, 'No' to 0
      
    )
  
    # Ensure the data has the same structure as the model input
    input_matrix <- model.matrix(~ sex + age + trestbps + chol + exang - 1, data = input_data)
    
    
    # Predict the risk using the XGBoost model
    prediction <- predict(model, newdata = input_matrix)
    risk_percentage <- round(prediction * 100, 2)  # Convert to percentage
    
    # Display the result
    output$prediction_result <- renderText({
      paste("Estimated Risk of Heart Disease: ", risk_percentage, "%")
    })
  })
}
```

#### User Interface

The Heart Disease Dashboard user interface (UI) was built with the
‘shinydashboard’ package, the dashboard features a three-part layout: a
header displaying the app’s title, a sidebar with navigation and input
controls, and a body that dynamically updates based on user
interactions. The sidebar includes a menu with three primary sections:
Statistics, Risk Prediction, and Resources. Each section was made to
provide users with distinct tools for exploring heart disease data and
risk factors.

``` r
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
```

``` r
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
```

``` r
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
```

``` r
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
```

## Results

The Statistics tab enables users to view heart disease death rates
across Florida counties through an interactive map, which visualizes
regional variations in death rates using color-coded polygons. A popup
appears when a user clicks on a county, providing additional information
about that area’s death rate. Below the map, a plot displays the trend
of heart disease death rates over time for the selected county, with the
option to adjust the year and county through dropdown menus.
Descriptions are provided to guide users in selecting the appropriate
year and county, and to explain how the map and plot relate to one
another.

In the Risk Prediction tab, users can input their personal health
information, such as age, sex, cholesterol levels, resting blood
pressure, and exercise-induced angina, to estimate their risk of heart
disease. Each input field is accompanied by a description that explains
the relevance of the variable to heart disease risk. After entering the
data, users can click a “Predict Risk” button, which triggers the app to
calculate and display the estimated risk as a percentage. A brief
explanation of the result is also provided, advising users on potential
next steps if their risk is high. As mentioned in the methods section,
the model used to generate the predictions was pre-trained and had a
prediction accuracy of 70% on a test dataset.

The Resources tab offers links to trusted organizations that provide
valuable information on heart disease prevention, treatment, and
support. These resources include the American Heart Association, the
Centers for Disease Control and Prevention (CDC), and the National
Heart, Lung, and Blood Institute (NHLBI). Each resource is accompanied
by a short description and a hyperlink to the respective website,
allowing users to explore additional materials on heart disease
management.

Example for using the app:

1)  In the Statistics tab, select year “2022” and county “Alachua” using
    the dropdowns. This should show the death rate from heart disease
    for Alachua county in 2022. Scroll down to see the plot of the trend
    in death rate over time for Alachua county.

2)  In the Risk Prediction tab, default values are already provided as
    example inputs. Scroll down and click the ‘Predict Risk’ button. The
    predicted risk percentage should display under ‘Prediction Result’.

3)  In the Resources tab, click the provided links to be redirected to
    helpful websites.

## Conclusion

The Heart Disease app provides descriptive statistics on heart disease
death rates in Florida by county, a heart disease risk calculator, and
resources to increase education on this topic. The app is meant to
provide a user-friendly resource for people to gain information on heart
disease and become informed about risk factors. It is also meant to
encourage them to do more research on preventative measures.

Further improvements can be made by increasing the accuracy of the
predictive model, which was only 70%, by using datasets including a
larger range of practical variables such as cigarettes smoked per day to
train the model. Additionally, adding a section describing specific
preventative measures would be highly informative.

## Sources

1.  American Heart Association. (2018). <https://www.heart.org>

2.  Florida Department of Health. (2022).
    <https://www.flhealthcharts.gov/Charts/>

3.  FLORIDA GEOGRAPHIC DATA LIBRARY DOCUMENTATION. (2015).
    <https://fgdl.org/zips/metadata/htm/cntbnd_sep15.htm>

4.  Janosi, A., Steinbrunn, W., Pfisterer, M., & Detrano, R. (1988, June
    30). UCI Machine Learning Repository. Archive.ics.uci.edu.
    <https://archive.ics.uci.edu/dataset/45/heart+disease>
