library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(janitor)
library(DT)
library(plotly)
library(htmlwidgets)
library(ggplot2)
library(shinyWidgets)

state_lookup <- tibble(
  state_ut = c(
    "ANDHRA PRADESH", "ARUNACHAL PRADESH", "ASSAM", "BIHAR",
    "CHANDIGARH", "CHHATTISGARH", "GOA", "GUJARAT", "HARYANA",
    "HIMACHAL PRADESH", "JAMMU & KASHMIR", "JHARKHAND", "KARNATAKA",
    "KERALA", "MADHYA PRADESH", "MAHARASHTRA", "MANIPUR", "MEGHALAYA",
    "MIZORAM", "NAGALAND", "ORISSA", "PUNJAB", "RAJASTHAN", "SIKKIM",
    "TAMIL NADU", "TRIPURA", "UTTAR PRADESH", "UTTARANCHAL", "WEST BENGAL",
    "A & N ISLANDS", "D & N HAVELI", "DAMAN & DIU", "DELHI UT",
    "LAKSHADWEEP", "PUDUCHERRY"
  ),
  STATE = c(
    "ANDHRA PRADESH", "ARUNACHAL PRADESH", "ASSAM", "BIHAR",
    "CHANDIGARH", "CHHATTISGARH", "GOA", "GUJARAT", "HARYANA",
    "HIMACHAL PRADESH", "JAMMU AND KASHMIR", "JHARKHAND", "KARNATAKA",
    "KERALA", "MADHYA PRADESH", "MAHARASHTRA", "MANIPUR", "MEGHALAYA",
    "MIZORAM", "NAGALAND", "ODISHA", "PUNJAB", "RAJASTHAN", "SIKKIM",
    "TAMIL NADU", "TRIPURA", "UTTAR PRADESH", "UTTARAKHAND", "WEST BENGAL",
    "ANDAMAN & NICOBAR", "DADRA & NAGAR HAVELI", "DAMAN & DIU", "DELHI",
    "LAKSHADWEEP", "PUDUCHERRY"
  )
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap"),
    tags$style(HTML("
      body {
        font-family: 'Inter', sans-serif;
        background: #ecf0f1;
      }
      .sidebar-panel {
        background: white;
        border-radius: 16px;
        box-shadow: 0 2px 12px rgba(44, 62, 80, 0.2);
        padding: 25px;
        margin-top: 20px;
        border-left: 5px solid #F39C12;
      }
      .sidebar-panel h3 {
        color: #2C3E50;
        font-weight: 700;
        font-size: 24px;
      }
      .main-card {
        background: white;
        border-radius: 16px;
        box-shadow: 0 2px 12px rgba(44, 62, 80, 0.15);
        padding: 20px;
        margin-top: 20px;
      }
      .btn-custom {
        background: linear-gradient(90deg, #F39C12, #f1c40f);
        border: none;
        color: white;
        font-weight: 600;
        width: 100%;
        border-radius: 10px;
      }
      .btn-custom:hover {
        background: linear-gradient(90deg, #f1c40f, #F39C12);
        color: #2C3E50;
      }
      .selectpicker {
        width: 100% !important;
      }
      .leaflet-tooltip {
        font-weight: bold;
        background: #2C3E50;
        color: white;
        border-radius: 5px;
        padding: 6px;
      }
    "))
  ),
  
  fluidRow(
    column(
      width = 3,
      div(
        class = "sidebar-panel",
        h3("Crime Explorer"),
        pickerInput("year", "Selecting Year", choices = NULL,
                    options = list(style = "btn-warning", `live-search` = TRUE)),
        pickerInput("crime_type", " Select Crime Type", choices = NULL,
                    options = list(style = "btn-warning", `live-search` = TRUE)),
        br(),
        downloadButton("download_map", " Export Map", class = "btn-custom")
      )
    ),
    
    column(
      width = 9,
      div(class = "main-card", leafletOutput("crime_map", height = 520)),
      tabsetPanel(
        tabPanel("Plots",
                 div(class = "main-card", plotlyOutput("bar_plot", height = 300)),
                 div(class = "main-card", plotlyOutput("line_plot", height = 300)),
                 div(class = "main-card", plotlyOutput("pie_plot", height = 300))
        ),
        tabPanel("Data Table",
                 div(class = "main-card", DTOutput("crime_table")))
      )
    )
  )
)

server <- function(input, output, session) {
  india_sf <- st_read("india_st.shp", quiet = TRUE)
  
  crime_data_wide <- read_csv("17_Crime_by_place_of_occurrence_2001_2012.csv", show_col_types = FALSE) %>%
    clean_names()
  
  crime_data_long <- crime_data_wide %>%
    pivot_longer(
      cols = -c(state_ut, year),
      names_to = "crime_type",
      values_to = "number_of_cases"
    ) %>%
    mutate(crime_type = str_replace_all(crime_type, "_", " ") %>% toupper())
  
  observe({
    updatePickerInput(session, "year",
                      choices = sort(unique(crime_data_long$year)),
                      selected = max(crime_data_long$year, na.rm = TRUE))
    
    updatePickerInput(session, "crime_type",
                      choices = sort(unique(crime_data_long$crime_type)),
                      selected = unique(crime_data_long$crime_type)[1])
  })
  
  crime_data_mapped <- reactive({
    crime_data_long %>%
      left_join(state_lookup, by = "state_ut") %>%
      filter(!is.na(STATE))
  })
  
  filtered_crime <- reactive({
    req(input$year, input$crime_type)
    crime_data_mapped() %>%
      filter(year == input$year,
             crime_type == input$crime_type) %>%
      group_by(STATE) %>%
      summarise(total_crimes = sum(number_of_cases, na.rm = TRUE))
  })
  
  joined_data <- reactive({
    india_sf %>%
      left_join(filtered_crime(), by = "STATE") %>%
      mutate(total_crimes = replace_na(total_crimes, 0))
  })
  
  map_widget <- reactive({
    data <- joined_data()
    pal <- colorNumeric(palette = "YlOrRd", domain = data$total_crimes)
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(total_crimes),
        fillOpacity = 0.75,
        color = "#2C3E50",
        weight = 1.2,
        label = ~paste0("<b>", STATE, "</b><br>", total_crimes, " cases"),
        labelOptions = labelOptions(
          direction = "auto",
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          opacity = 0.9
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#F39C12",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = data$total_crimes,
        title = htmltools::HTML("<b>Number of Crimes</b>"),
        position = "bottomright",
        labFormat = labelFormat(suffix = " cases")
      )
  })
  
  output$crime_map <- renderLeaflet({ map_widget() })
  
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("crime_map_", Sys.Date(), ".png")
    },
    content = function(file) {
      saveWidget(map_widget(), "temp_map.html", selfcontained = TRUE)
      webshot("temp_map.html", file = file, cliprect = "viewport")
      unlink("temp_map.html")
    }
  )
  
  output$bar_plot <- renderPlotly({
    plot_data <- filtered_crime() %>%
      arrange(desc(total_crimes)) %>%
      slice_head(n = 10)
    p <- ggplot(plot_data, aes(x = reorder(STATE, total_crimes), y = total_crimes)) +
      geom_bar(stat = "identity", fill = "#F39C12") +
      coord_flip() +
      labs(title = "Top 10 States by Crime Count", x = "State", y = "Crimes")
    ggplotly(p)
  })
  
  output$line_plot <- renderPlotly({
    line_data <- crime_data_mapped() %>%
      filter(crime_type == input$crime_type) %>%
      group_by(year) %>%
      summarise(total_crimes = sum(number_of_cases, na.rm = TRUE))
    p <- ggplot(line_data, aes(x = year, y = total_crimes)) +
      geom_line(color = "#2C3E50", size = 1.5) +
      geom_point(color = "#F39C12", size = 3) +
      labs(title = paste("Trend of", input$crime_type), x = "Year", y = "Crimes")
    ggplotly(p)
  })
  
  output$pie_plot <- renderPlotly({
    lollipop_data <- filtered_crime() %>%
      arrange(desc(total_crimes)) %>%
      slice_head(n = 5)
    
    p <- ggplot(lollipop_data, aes(x = reorder(STATE, total_crimes), y = total_crimes)) +
      geom_segment(aes(xend = STATE, yend = 0), color = "#F39C12", size = 1.5) +
      geom_point(color = "#2C3E50", size = 6) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 5 States by Crime", x = "State", y = "Crime Count")
    
    ggplotly(p)
  })
  
  output$crime_table <- renderDT({
    datatable(filtered_crime(), options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
