library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(DT)
library(leaflet)
library(htmltools)
library(shinyjs)
library(plotly)
library(ggplot2)

load('dataOK.RData')

# ============================================================
# 预处理：获取各个 input 的 choices
# ============================================================
prepare_choices <- function(data) {
  list(
    year_range = c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE)),
    constructor_nat = c("ALL", data %>% count(constructor.nationality, sort = TRUE) %>% pull(constructor.nationality)),
    constructor_name = c("ALL", data %>% count(constructor.name, sort = TRUE) %>% pull(constructor.name)),
    driver_name = c("ALL", data %>% count(driver.name, sort = TRUE) %>% pull(driver.name)),
    driver_fatal = unique(data$driver.fatal)
  )
}

# Generate choices once at startup
choices <- prepare_choices(dataOK)

# ============================================================
# Helper function to create labeled inputs with tooltips on icons only
# ============================================================
create_labeled_input <- function(input_func, input_id, label_text, tooltip_text, ...) {
  tagList(
    tags$label(
      class = "control-label",
      label_text,
      tooltip(
        icon("circle-info", class = "text-muted ms-1"),
        tooltip_text,
        placement = "right"
      )
    ),
    input_func(input_id, label = NULL, ...)
  )
}

# ============================================================
# UI - Changed to page_navbar with a SHARED sidebar
# ============================================================
ui <- page_navbar(
  title = tags$div(
    tags$img(src = "https://www.formula1.com/etc/designs/fom-website/images/f1_logo.svg", height = "30px", style = "margin-right: 10px;"),
    "Formula 1 Data Explorer (1950-2024)",
    style = "display: flex; align-items: center;"
  ),
  
  # Create a SINGLE shared sidebar for all tabs
  sidebar = sidebar(
    width = 300,
    bg = "#f8f9fa",
    open ='always',
	
    # Year Range Slider with tooltip only on icon
    create_labeled_input(
      sliderInput,
      "year_range",
      "Year Range",
      "Filter F1 races by year. Drag the slider to select a range from 1950 to 2024.",
      min = choices$year_range[1],
      max = choices$year_range[2],
      value = c(choices$year_range[1], choices$year_range[2]),
      step = 1,
      sep = "",
      ticks = FALSE
    ),
    
    # Small divider
    tags$div(style = "height: 8px;"),
    
    # Constructor Nationality
    create_labeled_input(
      pickerInput,
      "constructor_nationality",
      "Constructor Nationality",
      "Select the nationality of constructor teams. Choose 'ALL' to include all.",
      choices = choices$constructor_nat,
      selected = "ALL",
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search nationality...",
        style = "btn-outline-secondary"
      )
    ),
    
    # Constructor Name
    create_labeled_input(
      pickerInput,
      "constructor_name",
      "Constructor Name",
      "Select the constructor/team name. Choose 'ALL' to include all.",
      choices = choices$constructor_name,
      selected = "ALL",
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search constructor...",
        style = "btn-outline-secondary"
      )
    ),
    
    # Driver Name
    create_labeled_input(
      pickerInput,
      "driver_name",
      "Driver Name",
      "Select a specific driver name. Choose 'ALL' to include all drivers.",
      choices = choices$driver_name,
      selected = "ALL",
      options = pickerOptions(
        liveSearch = TRUE,
        liveSearchPlaceholder = "Search driver...",
        style = "btn-outline-secondary"
      )
    ),
    
    # Driver Fatal Status
    create_labeled_input(
      checkboxGroupInput,
      "driver_fatal",
      "Driver Fatal Status",
      "'NULL' = No fatal accident; 'Death' = Driver had a fatal accident.",
      choices = choices$driver_fatal,
      selected = choices$driver_fatal,
      inline = TRUE
    ),
    
    # Small divider
    tags$div(style = "height: 8px;"),
    
    # Reset button
    tags$div(class = "d-grid gap-2 mt-2",
      actionButton(
        inputId = "reset_filters",
        label = "Reset All Filters",
        icon = icon("rotate"),
        class = "btn-outline-secondary"
      )
    ),
    
    # Data summary
    tags$div(
      class = "mt-3 pt-2 border-top",
      textOutput("data_summary"),
      style = "font-size: 0.9rem; color: #666;"
    )
  ),
  
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    primary = "#E10600",  # F1 red
    "navbar-bg" = "#15151E"  # F1 dark blue
  ),
  
  # Add shinyjs
  useShinyjs(),
  
  # Custom CSS for better styling
  tags$head(tags$style(HTML("
    .control-label { font-weight: 600; color: #333; }
    .card { box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
    /* Remove card margins */
    .tab-content .card { margin-bottom: 0px !important; }
    .action-button { margin-top: 10px; }
    .dataTables_wrapper { font-size: 14px; }
    .fatal-event { color: #E10600 !important; font-weight: bold; }
    .map-container { border-radius: 5px; overflow: hidden; }
    /* Added padding for the datatable */
    .datatable-container { 
      height: calc(100vh - 200px) !important; 
      min-height: 700px;
      padding: 15px !important; 
    }
    /* More compact sidebar styles */
    .sidebar .form-group { margin-bottom: 10px; }
    .sidebar .control-label { font-size: 0.9rem; }
    .sidebar .shiny-input-container { margin-bottom: 5px !important; }
    /* Add spacing between tab titles and content */
    .tab-content { padding-top: 15px; }
    .nav-underline { margin-bottom: 10px; }
    /* Chart container styling */
    .chart-container { height: 400px; width: 100%; }
    .plotly-graph { height: 100%; width: 100%; }
    /* Card headers */
    .card-header { background-color: #f8f9fa; }
    /* No side padding for the main container */
    .tab-content .container-fluid { padding-left: 0 !important; padding-right: 0 !important; }
    /* Fix for sidebar labels */
    .sidebar .control-label { display: flex; align-items: center; }
    /* Bottom margin for layout columns */
    .layout-columns { margin-bottom: 0 !important; }
  "))),
  
  # ----------------------------------------------------------
  # Primary tab: Circuit Map
  # ----------------------------------------------------------
  nav_panel(
    title = "Circuit Map",
    icon = icon("map-location-dot"),
    
    # Main content - no need for layout_sidebar since we have a shared sidebar
    tags$div(style = "height: 15px;"),
      
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("F1 Circuit Locations", class = "h5 m-0"),
        tags$div(
          class = "btn-group",
          actionButton(
            inputId = "zoom_all",
            label = "View All",
            icon = icon("globe"),
            class = "btn-sm btn-outline-primary"
          ),
          actionButton(
            inputId = "zoom_europe",
            label = "Europe",
            icon = icon("location-dot"),
            class = "btn-sm btn-outline-primary"
          )
        )
      ),
      card_body(
        class = "p-0 map-container",
        leafletOutput("map", height = "700px")
      )
    )
  ),
  
  # ----------------------------------------------------------
  # Secondary tab: Analytics Dashboard
  # ----------------------------------------------------------
  nav_panel(
    title = "Analytics Dashboard",
    icon = icon("chart-line"),
    
    # Main content - no need for layout_sidebar
    tags$div(style = "height: 15px;"),
      
    # First row: Annual Races and Fatal Event Occurrence
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Annual Races and Fatal Event Occurrence", class = "h5 m-0")
      ),
      card_body(
        class = "chart-container",
        plotlyOutput("annual_race_plot", height = "100%")
      )
    ),
      
    # Second row: Two visualizations side by side
    layout_columns(
      col_widths = c(6, 6),
        
      # Left column: Distribution of Fatal Events by Constructor
      card(
        height = "100%",
        card_header(
          "Distribution of Fatal Events by Constructor"
        ),
        card_body(
          class = "chart-container",
          plotlyOutput("constructor_pie_chart", height = "100%")
        )
      ),
        
      # Right column: Driver Nationalities and Fatality Rates
      card(
        height = "100%",
        card_header(
          "Driver Nationalities and Fatality Rates"
        ),
        card_body(
          class = "chart-container",
          plotlyOutput("nationality_sunburst", height = "100%")
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # Third tab: Data Table
  # ----------------------------------------------------------
  nav_panel(
    title = "Data Table",
    icon = icon("table"),
    
    # Main content - no need for layout_sidebar
    tags$div(style = "height: 15px;"),
      
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        tags$span("Race Results", class = "h5 m-0"),
        downloadButton("download_data", "Download", class = "btn-sm btn-outline-primary")
      ),
      card_body(
        class = "datatable-container", # Added padding in CSS
        DTOutput("datatable", height = "100%")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Reset Filters Button
  # ----------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "year_range", value = c(choices$year_range[1], choices$year_range[2]))
    updatePickerInput(session, "constructor_nationality", selected = "ALL")
    updatePickerInput(session, "constructor_name", selected = "ALL")
    updatePickerInput(session, "driver_name", selected = "ALL")
    updateCheckboxGroupInput(session, "driver_fatal", selected = choices$driver_fatal)
  })
  
  # ----------------------------------------------------------
  # Reactive: Filtered Data
  # ----------------------------------------------------------
  filtered_data <- reactive({
    # Apply filters
    data <- dataOK %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    # Filter by constructor nationality
    if (!is.null(input$constructor_nationality) && 
        input$constructor_nationality != "ALL") {
      data <- data %>%
        filter(constructor.nationality == input$constructor_nationality)
    }
    
    # Filter by constructor name
    if (!is.null(input$constructor_name) && 
        input$constructor_name != "ALL") {
      data <- data %>%
        filter(constructor.name == input$constructor_name)
    }
    
    # Filter by driver name
    if (!is.null(input$driver_name) && 
        input$driver_name != "ALL") {
      data <- data %>%
        filter(driver.name == input$driver_name)
    }
    
    # Filter by driver fatal status
    if (!is.null(input$driver_fatal) && length(input$driver_fatal) > 0) {
      data <- data %>%
        filter(driver.fatal %in% input$driver_fatal)
    }
    
    data
  })
  
  # ----------------------------------------------------------
  # Output: Data Summary
  # ----------------------------------------------------------
  output$data_summary <- renderText({
    data <- filtered_data()
    races <- length(unique(data$Race))
    drivers <- length(unique(data$driver.name))
    fatalities <- sum(data$driver.fatal == "Death", na.rm = TRUE)
    
    paste0(
      "Showing ", nrow(data), " entries\n",
      "Races: ", races, " | ",
      "Drivers: ", drivers, " | ",
      "Fatal events: ", fatalities
    )
  })
  
  # ----------------------------------------------------------
  # Reactive: Map Data
  # ----------------------------------------------------------
  map_data <- reactive({
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No data available for the selected filters")
    )
    
    # Optimize by pre-filtering only necessary columns
    data %>%
      select(lng, lat, circuit.name, Race, driver.name, driver.fatal) %>%
      # Group by circuit
      group_by(lng, lat, circuit.name) %>%
      summarise(
        races = paste(unique(Race), collapse = "<br>"),
        race_count = n_distinct(Race),
        has_fatal = any(driver.fatal == "Death", na.rm = TRUE),
        fatal_drivers = ifelse(
          any(driver.fatal == "Death", na.rm = TRUE),
          paste(unique(driver.name[driver.fatal == "Death"]), collapse = ", "),
          ""
        ),
        .groups = "drop"
      )
  })
  
  # ----------------------------------------------------------
  # Output: Data Table - prioritizing "Death" rows
  # ----------------------------------------------------------
  output$datatable <- renderDT({
    # Select and rename needed columns with priority for Death
    table_data <- filtered_data() %>%
      select(
        Race,
        driver.name,
        constructor.name,
        date,
        driver.points,
        driver.fatal
      ) %>%
      rename(
        "Grand Prix" = Race,
        "Driver" = driver.name,
        "Constructor" = constructor.name,
        "Date" = date,
        "Points" = driver.points,
        "Fatal Status" = driver.fatal
      ) %>%
      # Sort with Death first, then by date
      arrange(desc(`Fatal Status` == "Death"), desc(Date))
    
    # Check if we have data
    validate(
      need(nrow(table_data) > 0, "No data available for the selected filters")
    )
    
    datatable(
      table_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "650px",
        deferRender = TRUE,
        dom = '<"top"lf>rt<"bottom"ip>',
        order = list(list(5, 'desc'), list(3, 'desc')),
        columnDefs = list(
          list(width = "200px", targets = 0),
          list(width = "180px", targets = 1),
          list(width = "180px", targets = 2),
          list(width = "100px", targets = 3),
          list(width = "80px", targets = 4),
          list(width = "100px", targets = 5)
        )
      ),
      filter = "top",
      rownames = FALSE,
      class = "display compact stripe hover",
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: left; font-size: 14px; margin-bottom: 10px;",
        paste0("Showing ", nrow(table_data), " of ", nrow(dataOK), " records")
      )
    ) %>%
      # Highlight fatal events
      formatStyle(
        columns = "Fatal Status",
        target = "row",
        backgroundColor = styleEqual(
          levels = "Death",
          values = "#ffecec"
        )
      ) %>%
      formatStyle(
        columns = "Fatal Status",
        color = styleEqual(
          levels = "Death",
          values = "#E10600"
        ),
        fontWeight = styleEqual(
          levels = "Death",
          values = "bold"
        )
      )
  })
  
  # ----------------------------------------------------------
  # Download handler for the data table
  # ----------------------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste("f1_data_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv", sep = "")
    },
    content = function(file) {
      data <- filtered_data() %>%
        select(
          Race, driver.name, constructor.name, 
          date, driver.points, driver.fatal
        )
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ----------------------------------------------------------
  # Output: Leaflet Map
  # ----------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 2, maxZoom = 18)
      ) %>%
      setView(lng = 10, lat = 40, zoom = 3) %>%
      # Add a legend
      addLegend(
        position = "bottomright",
        colors = c("#3498DB", "#E10600"),
        labels = c("Regular Circuit", "Circuit with Fatal Event"),
        opacity = 0.8
      )
  })
  
  # ----------------------------------------------------------
  # Observer: Map zoom controls
  # ----------------------------------------------------------
  observeEvent(input$zoom_all, {
    leafletProxy("map") %>%
      setView(lng = 10, lat = 30, zoom = 2)
  })
  
  observeEvent(input$zoom_europe, {
    leafletProxy("map") %>%
      setView(lng = 10, lat = 50, zoom = 4)
  })
  
  # ----------------------------------------------------------
  # Observer: Update map markers
  # ----------------------------------------------------------
  observe({
    data <- map_data()
    
    # Skip if no data
    if (is.null(data) || nrow(data) == 0) {
      return()
    }
    
    # Clear previous markers
    proxy <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    
    # Create HTML labels with styling
    labels <- lapply(1:nrow(data), function(i) {
      circuit <- data$circuit.name[i]
      races <- data$races[i]
      has_fatal <- data$has_fatal[i]
      fatal_drivers <- data$fatal_drivers[i]
      race_count <- data$race_count[i]
      
      HTML(paste0(
        "<div style='font-family: Arial, sans-serif; min-width: 220px;'>",
        "<div style='background-color: ", ifelse(has_fatal, "#ffecec", "#f0f8ff"), "; ",
        "padding: 10px; border-radius: 5px 5px 0 0; border-bottom: 2px solid ", 
        ifelse(has_fatal, "#E10600", "#3498DB"), ";'>",
        "<strong style='font-size: 16px;'>", circuit, "</strong>",
        "<div style='margin-top: 4px; font-size: 12px;'>", 
        race_count, " race", ifelse(race_count > 1, "s", ""), " held",
        "</div>",
        "</div>",
        "<div style='padding: 10px; max-height: 200px; overflow-y: auto; font-size: 13px;'>",
        if (has_fatal && nchar(fatal_drivers) > 0) {
          paste0(
            "<div style='margin-bottom: 10px; padding: 8px; background-color: #ffecec; ",
            "border-left: 3px solid #E10600; border-radius: 3px;'>",
            "<strong style='color: #E10600;'>Fatal Incident", 
            ifelse(grepl(",", fatal_drivers), "s", ""), ":</strong><br>",
            fatal_drivers,
            "</div>"
          )
        } else { "" },
        "<div style='font-size: 12px; opacity: 0.9;'>", races, "</div>",
        "</div>",
        "</div>"
      ))
    })
    
    # Add markers
    proxy %>%
      addCircleMarkers(
        data = data,
        lng = ~lng,
        lat = ~lat,
        radius = ~ifelse(has_fatal, 8, 6) + sqrt(race_count),
        color = ~ifelse(has_fatal, "#E10600", "#3498DB"),
        fillColor = ~ifelse(has_fatal, "#E10600", "#3498DB"),
        fillOpacity = ~ifelse(has_fatal, 0.8, 0.6),
        stroke = TRUE,
        weight = 2,
        label = lapply(labels, HTML),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Arial, sans-serif",
            "font-size" = "12px",
            "padding" = "5px",
            "border-radius" = "3px"
          ),
          textsize = "13px",
          direction = "auto",
          offset = c(0, -10)
        ),
        popup = labels,
        popupOptions = popupOptions(
          closeButton = TRUE,
          closeOnClick = TRUE,
          minWidth = 250
        )
      )
  })
  
  # ----------------------------------------------------------
  # VISUALIZATION 1: Annual Races and Fatal Event Occurrence
  # ----------------------------------------------------------
  # Reactive data for annual race plot
  annual_race_data <- reactive({
    # Use filtered data
    data <- filtered_data()
    
    # Group by year and calculate metrics
    data %>%
      group_by(year) %>%
      summarize(
        NumberRaces = n_distinct(Race),
        FatalEventOccur = any(driver.fatal == 'Death', na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  # Annual races plotly output - FIXED implementation with non-overlapping legend
  output$annual_race_plot <- renderPlotly({
    data <- annual_race_data()
    
    validate(
      need(nrow(data) > 0, "No data available for the selected filters")
    )
    
    # Create color mapping for points
    data$point_color <- ifelse(data$FatalEventOccur, "#E10600", "#3498DB")
    
    # Create the plot with a simpler approach
    plot_ly() %>%
      # Add line for races
      add_trace(
        data = data,
        x = ~year,
        y = ~NumberRaces,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = '#666666', width = 2),
        marker = list(
          size = 10,
          color = ~point_color,
          line = list(color = '#FFFFFF', width = 1)
        ),
        name = 'Races',
        text = ~paste("Year:", year, "<br>Races:", NumberRaces, 
                     "<br>Fatal Event:", ifelse(FatalEventOccur, "Yes", "No")),
        hoverinfo = 'text'
      ) %>%
      # Layout
      layout(
        title = list(
          text = "Annual Number of Races and Fatal Event Occurrence",
          font = list(size = 18)
        ),
        xaxis = list(
          title = "Year",
          type = "category"
        ),
        yaxis = list(
          title = "Number of Races",
          rangemode = "tozero"
        ),
        showlegend = FALSE,
        # Add custom legend items with fixed spacing
        annotations = list(
          list(
            x = 0.02,
            y = 0.98,
            xref = "paper",
            yref = "paper",
            text = "Race with Fatal Event",
            showarrow = FALSE,
            bgcolor = "#E10600",
            bordercolor = "#FFFFFF",
            borderwidth = 1,
            font = list(color = "#FFFFFF"),
            align = "left",
            xanchor = "left",
            yanchor = "top",
            width = 150,
            height = 20
          ),
          list(
            x = 0.02,
            y = 0.81, # Increased space between annotations
            xref = "paper",
            yref = "paper",
            text = "Race with No Fatal Event",
            showarrow = FALSE,
            bgcolor = "#3498DB",
            bordercolor = "#FFFFFF",
            borderwidth = 1,
            font = list(color = "#FFFFFF"),
            align = "left",
            xanchor = "left",
            yanchor = "top",
            width = 170,
            height = 20
          )
        )
      )
  })
  
  # ----------------------------------------------------------
  # VISUALIZATION 2: Distribution of Fatal Events by Constructor
  # ----------------------------------------------------------
  # Reactive data for constructor pie chart
  constructor_fatal_data <- reactive({
    # Use filtered data
    data <- filtered_data()
    
    # Filter for fatal events and count by constructor
    data %>%
      filter(driver.fatal == 'Death') %>%
      group_by(constructor.name) %>%
      summarize(NumberFatalDrivers = n(), .groups = "drop") %>%
      arrange(desc(NumberFatalDrivers))
  })
  
  # Constructor pie chart plotly output
  output$constructor_pie_chart <- renderPlotly({
    data <- constructor_fatal_data()
    
    validate(
      need(nrow(data) > 0, "No fatal events in the selected data")
    )
    
    # Create the pie chart
    plot_ly(
      data = data, 
      labels = ~constructor.name, 
      values = ~NumberFatalDrivers, 
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~paste(constructor.name, ": ", NumberFatalDrivers, " fatal incidents"),
      marker = list(
        colors = colorRampPalette(c("#3a86ff", "#e63946"))(nrow(data)),
        line = list(color = '#FFFFFF', width = 1)
      )
    ) %>%
      layout(
        title = list(
          text = "Distribution of Fatal Events by Constructor",
          font = list(size = 16)
        ),
        margin = list(l = 20, r = 20, t = 50, b = 20),
        showlegend = FALSE
      )
  })
  
  # ----------------------------------------------------------
  # VISUALIZATION 3: Driver Nationalities and Fatality Rates
  # ----------------------------------------------------------
  # Reactive data for nationality sunburst chart
  nationality_data <- reactive({
    # Use filtered data
    data <- filtered_data()
    
    # Group by nationality and calculate metrics
    data %>%
      group_by(driver.nationality) %>%
      summarize(
        NumberDrivers = n_distinct(driver.name),
        NumberFatalEvent = sum(driver.fatal == 'Death', na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Add continent grouping and calculate rates
      mutate(
        continent = case_when(
          driver.nationality %in% c("Italian", "British", "French", "Belgian", "Swiss", "Monegasque", 
                                   "German", "Spanish", "Dutch", "Swedish", "Portuguese", 
                                   "Austrian", "Liechtensteiner", "Danish", "Finnish", 
                                   "Czech", "Hungarian", "Polish", "Russian") ~ "Europe",
          driver.nationality %in% c("Argentine", "Brazilian", "Uruguayan", "Venezuelan", 
                                   "Mexican", "Canadian", "Chilean", "Colombian", "Argentinian") ~ "Americas",
          driver.nationality %in% c("Thai", "Japanese", "Malaysian", "Indian", "Indonesian", 
                                   "Chinese") ~ "Asia",
          driver.nationality %in% c("South African", "Rhodesian") ~ "Africa",
          driver.nationality %in% c("Australian", "New Zealander") ~ "Oceania",
          driver.nationality == "American" ~ "North America",
          driver.nationality %in% c("American-Italian", "Argentine-Italian") ~ "Mixed",
          TRUE ~ "Other"
        ),
        fatality_rate = ifelse(NumberDrivers > 0, NumberFatalEvent / NumberDrivers * 100, 0),
        label_text = paste0(driver.nationality, 
                           "<br>Drivers: ", NumberDrivers, 
                           "<br>Fatal events: ", NumberFatalEvent,
                           "<br>Fatality rate: ", round(fatality_rate, 1), "%")
      )
  })
  
  # Create continent summaries for the sunburst inner ring
  continent_summary <- reactive({
    data <- nationality_data()
    
    # If no data, return empty data frame
    if (nrow(data) == 0) {
      return(data.frame())
    }
    
    data %>%
      group_by(continent) %>%
      summarize(
        NumberDrivers = sum(NumberDrivers),
        NumberFatalEvent = sum(NumberFatalEvent),
        fatality_rate = sum(NumberFatalEvent) / sum(NumberDrivers) * 100,
        .groups = "drop"
      ) %>%
      mutate(
        label_text = paste0(continent, 
                           "<br>Total drivers: ", NumberDrivers, 
                           "<br>Total fatal events: ", NumberFatalEvent,
                           "<br>Fatality rate: ", round(fatality_rate, 1), "%")
      )
  })
  
  # Nationality sunburst chart plotly output
  output$nationality_sunburst <- renderPlotly({
    nat_data <- nationality_data()
    cont_data <- continent_summary()
    
    validate(
      need(nrow(nat_data) > 0, "No data available for the selected filters")
    )
    
    # Prepare data for the sunburst chart
    labels <- c(cont_data$continent, nat_data$driver.nationality)
    parents <- c(rep("", nrow(cont_data)), nat_data$continent)
    values <- c(cont_data$NumberDrivers, nat_data$NumberDrivers)
    hover_text <- c(cont_data$label_text, nat_data$label_text)
    
    # Create color scale based on fatality rate
    fatality_rates <- c(cont_data$fatality_rate, nat_data$fatality_rate)
    colors <- sapply(fatality_rates, function(rate) {
      if (rate == 0) return("#3a86ff")          # Blue for zero fatality
      else if (rate <= 3) return("#8ecae6")      # Light blue for low fatality
      else if (rate <= 6) return("#ffb703")      # Yellow for medium fatality
      else return("#e63946")                     # Red for high fatality
    })
    
    # Create the sunburst chart
    plot_ly(
      labels = labels,
      parents = parents,
      values = values,
      type = "sunburst",
      branchvalues = "total",
      hovertext = hover_text,
      hoverinfo = "text",
      marker = list(
        colors = colors,
        line = list(color = "#ffffff", width = 1)
      ),
      textinfo = "label",
      insidetextorientation = "radial"
    ) %>% layout(
      title = list(
        text = "F1 Drivers by Nationality and Fatality Rate",
        font = list(size = 16)
      ),
      margin = list(l = 0, r = 0, t = 50, b = 0)
    )
  })
}

# ============================================================
# Run App
# ============================================================
shinyApp(ui, server)