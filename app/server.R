

server <- function(input, output, session) {
  
  fl_county_shapefile <- st_read("cntbnd_sep15")
  fl_county_shapefile <- st_transform(fl_county_shapefile, crs = 4326)
  fl_county_shapefile$County <- fl_county_shapefile$TIGERNAME
  
  flhealthcharts <- read.csv("heartdisease.csv")
  flhealthcharts[flhealthcharts == "Saint Johns"] <- "St. Johns"
  flhealthcharts[flhealthcharts == "Saint Lucie"] <- "St. Lucie"
  flhealthcharts[flhealthcharts == "Desoto"] <- "DeSoto"
  
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
