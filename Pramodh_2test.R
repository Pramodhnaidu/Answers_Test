
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

ui <- fluidPage(
  titlePanel(" Analysis on Superstore Customer data"),
  
  navbarPage("Analysis Sections",
             tabPanel("Customer Enrollment & Spending Trends",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Analysis Options"),
                          p("This section displays key metrics related to customer enrollment and spending trends over time and by marital status."),
                          hr()
                        ),
                        mainPanel(
                          tabsetPanel(
                            id = "enrollmentSpendingTabs",
                            tabPanel("Customer Enrollment",
                                     h4("Year-wise Customer Enrollment"),
                                     plotOutput("enrollmentPlot")
                            ),
                            tabPanel("Total Spending (Year-wise)",
                                     h4("Year-wise Total Amount Spent"),
                                     plotOutput("spendingYearPlot")
                            ),
                            tabPanel("Spending by Marital Status",
                                     h4("Total Spending by Marital Status"),
                                     plotOutput("spendingMaritalPlot")
                            )
                          )
                        )
                      )
             ),
             
             tabPanel("Age-based Purchase Behavior",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Filter by Age"),
                          sliderInput("age_range",
                                      "Select Age Range:",
                                      min = 18,
                                      max = 95,
                                      value = c(25, 65)),
                          
                          hr(),
                          h4("How Age is Calculated:"),
                          p("Age is calculated by subtracting the 'Year_Birth' from the current year (2025)."),
                          h4("What is a 'Purchase'?"),
                          p("Total Purchases represent the sum of purchases made through Deals, Web, Catalog, and Store channels for each customer.")
                        ),
                        mainPanel(
                          h3("Total Purchases by Age Group"),
                          plotOutput("purchase_plot"),
                          br(), # Line break for spacing
                          h3("Spending Insights"),
                          textOutput("spending_insights")
                        )
                      )
             ),
             
             tabPanel("Income vs. Spending Correlation",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(
                            inputId = "educationFilter",
                            label = "Filter by Education Level:",
                            choices = NULL, # Choices will be updated by server
                            selected = NULL
                          ),
                          
                          checkboxGroupInput(
                            inputId = "maritalStatusFilter",
                            label = "Filter by Marital Status:",
                            choices = NULL, # Choices will be updated by server
                            selected = NULL
                          )
                        ),
                        mainPanel(
                          plotOutput("correlationPlot")
                        )
                      )
             ),
             
             tabPanel("Best-selling Product Category",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("filter_year", "Select Year:",
                                      choices = c("All"), # Populated by server
                                      selected = "All"),
                          selectInput("filter_marital_best_selling", "Select Marital Status:", # Renamed to avoid conflict
                                      choices = c("All"), # Populated by server
                                      selected = "All")
                        ),
                        mainPanel(
                          plotOutput("categoryPlot")
                        )
                      )
             ),
             
             tabPanel("In-store vs. Online Purchases",
                      fluidPage(
                        titlePanel("In-store vs. Online Purchases"),
                        plotOutput("purchasePlotChannel"), # Renamed to avoid conflict
                        downloadButton("downloadPlotChannel", "Download Plot") # Renamed to avoid conflict
                      )
             )
  )
)

server <- function(input, output, session) {
  
  # Consolidated Data Loading and Preprocessing
  data <- reactive({
    df <- read.csv("superstore_data.csv", header = TRUE, stringsAsFactors = FALSE)
    
    # Preprocessing from Test 1.R
    df$Dt_Customer <- mdy(df$Dt_Customer)
    df$Enrollment_Year <- year(df$Dt_Customer)
    df <- df %>%
      mutate(Total_Spent = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds)
    df <- df %>% filter(!is.na(Income))
    
    # Preprocessing from Test 2.R
    df <- df %>%
      mutate(
        Age = 2025 - Year_Birth,
        TotalPurchases = rowSums(select(., starts_with("Num")), na.rm = TRUE)
      ) %>%
      filter(Age >= 18 & Age <= 95)
    
    # Preprocessing from Test 3.R
    df$Amount_Spent <- df$MntWines + df$MntFruits + df$MntMeatProducts +
      df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds
    
    # Preprocessing from Question 5.R
    df$TotalSpent <- df$MntWines + df$MntFruits + df$MntMeatProducts +
      df$MntFishProducts + df$MntSweetProducts + df$MntGoldProds
    
   
    df$Marital <- as.factor(df$Marital_Status) 
    
    df
  })
  
  enrollment_data <- reactive({
    data() %>%
      group_by(Enrollment_Year) %>%
      summarise(Customers = n_distinct(Id)) %>%
      arrange(Enrollment_Year)
  })
  
  spending_year_data <- reactive({
    data() %>%
      group_by(Enrollment_Year) %>%
      summarise(Total_Amount_Spent = sum(Total_Spent, na.rm = TRUE)) %>%
      arrange(Enrollment_Year)
  })
  
  spending_marital_data <- reactive({
    data() %>%
      group_by(Marital_Status) %>%
      summarise(Total_Amount_Spent = sum(Total_Spent, na.rm = TRUE)) %>%
      arrange(Total_Amount_Spent)
  })
  
  output$enrollmentPlot <- renderPlot({
    ggplot(enrollment_data(), aes(x = factor(Enrollment_Year), y = Customers, fill = factor(Enrollment_Year))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Customers), vjust = -0.5, size = 4) +
      labs(title = "Customer Enrollment Over Years",
           x = "Enrollment Year",
           y = "Number of Customers") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$spendingYearPlot <- renderPlot({
    ggplot(spending_year_data(), aes(x = Enrollment_Year, y = Total_Amount_Spent)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(Total_Amount_Spent, 0)), vjust = -1, size = 4) +
      labs(title = "Total Spending Across Categories Over Years",
           x = "Enrollment Year",
           y = "Total Amount Spent") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = unique(spending_year_data()$Enrollment_Year))
  })
  
  output$spendingMaritalPlot <- renderPlot({
    ggplot(spending_marital_data(), aes(x = reorder(Marital_Status, -Total_Amount_Spent), y = Total_Amount_Spent, fill = Marital_Status)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Total_Amount_Spent, 0)), vjust = -0.5, size = 4) +
      labs(title = "Total Spending by Marital Status",
           x = "Marital Status",
           y = "Total Amount Spent") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  observe({
    req(data()) # Ensure data is loaded
    min_age <- min(data()$Age, na.rm = TRUE)
    max_age <- max(data()$Age, na.rm = TRUE)
    updateSliderInput(session, "age_range",
                      min = min_age,
                      max = max_age,
                      value = c(min_age, max_age))
  })
  
  filtered_data_age <- reactive({ # Renamed to avoid conflict
    data() %>%
      filter(Age >= input$age_range[1] & Age <= input$age_range[2])
  })
  
  output$purchase_plot <- renderPlot({
    req(filtered_data_age())
    
    plot_data <- filtered_data_age() %>%
      group_by(Age) %>%
      summarise(TotalPurchases = sum(TotalPurchases, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(plot_data, aes(x = Age, y = TotalPurchases)) +
      geom_bar(stat = "identity", fill = "#428", color = "white", width = 0.8) +
      labs(title = "Total Purchases by Customer Age",
           x = "Customer Age",
           y = "Total Number of Purchases") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")
      )
  })
  
  output$spending_insights <- renderText({
    req(filtered_data_age())
    
    current_data <- filtered_data_age()
    
    if (nrow(current_data) == 0) {
      return("No data available for the selected age range. Please adjust the slider.")
    }
    
    total_purchases_filtered <- sum(current_data$TotalPurchases, na.rm = TRUE)
    avg_purchases_per_customer <- mean(current_data$TotalPurchases, na.rm = TRUE)
    
    age_group_summary <- current_data %>%
      group_by(Age) %>%
      summarise(AvgPurchases = mean(TotalPurchases, na.rm = TRUE)) %>%
      arrange(desc(AvgPurchases)) %>%
      ungroup()
    
    top_age_group <- age_group_summary$Age[1]
    top_avg_purchases <- age_group_summary$AvgPurchases[1]
    
    total_purchases_by_age <- current_data %>%
      group_by(Age) %>%
      summarise(TotalPurchases = sum(TotalPurchases, na.rm = TRUE)) %>%
      arrange(desc(TotalPurchases)) %>%
      ungroup()
    
    max_total_age <- total_purchases_by_age$Age[1]
    max_total_purchases <- total_purchases_by_age$TotalPurchases[1]
    
    young_adults <- current_data %>% filter(Age >= 18 & Age <= 35)
    middle_aged <- current_data %>% filter(Age > 35 & Age <= 60)
    seniors <- current_data %>% filter(Age > 60)
    
    insight_text <- paste0(
      "Within the selected age range (", input$age_range[1], " to ", input$age_range[2], " years old):",
      "\n\n- Total purchases made: ", format(total_purchases_filtered, big.mark = ","),
      "\n- Average purchases per customer: ", round(avg_purchases_per_customer, 2), " units.",
      "\n\n- The age of ", top_age_group, " years old shows the highest average purchases per customer (approx. ", round(top_avg_purchases, 2), " units).",
      "\n- The age of ", max_total_age, " years old accounts for the highest total number of purchases (", format(max_total_purchases, big.mark = ","), " units)."
    )
    
    if (nrow(young_adults) > 0) {
      avg_young <- mean(young_adults$TotalPurchases, na.rm = TRUE)
      insight_text <- paste0(insight_text, "\n\n- Young adults (18-35 years) average around ", round(avg_young, 2), " purchases.")
    }
    if (nrow(middle_aged) > 0) {
      avg_middle <- mean(middle_aged$TotalPurchases, na.rm = TRUE)
      insight_text <- paste0(insight_text, "\n- Middle-aged customers (36-60 years) average around ", round(avg_middle, 2), " purchases.")
    }
    if (nrow(seniors) > 0) {
      avg_senior <- mean(seniors$TotalPurchases, na.rm = TRUE)
      insight_text <- paste0(insight_text, "\n- Senior customers (61+ years) average around ", round(avg_senior, 2), " purchases.")
    }
    
    insight_text
  })
  
  # --- Reactive data and outputs for "Income vs. Spending Correlation" tab (from Test 3.R) ---
  observe({
    req(data())
    updateCheckboxGroupInput(session, "educationFilter",
                             choices = unique(data()$Education),
                             selected = unique(data()$Education))
    updateCheckboxGroupInput(session, "maritalStatusFilter",
                             choices = unique(data()$Marital_Status),
                             selected = unique(data()$Marital_Status))
  })
  
  filtered_data_correlation <- reactive({ # Renamed to avoid conflict
    req(input$educationFilter)
    req(input$maritalStatusFilter)
    
    data() %>%
      filter(Education %in% input$educationFilter) %>%
      filter(Marital_Status %in% input$maritalStatusFilter) %>%
      filter(!is.na(Income) & !is.na(Amount_Spent) & Income > 0 & Amount_Spent > 0)
  })
  
  output$correlationPlot <- renderPlot({
    ggplot(filtered_data_correlation(), aes(x = Income, y = Amount_Spent)) +
      geom_point(alpha = 0.6, color = "darkblue") +
      geom_smooth(method = "lm", col = "darkred", se = FALSE) +
      labs(
        title = "Correlation between Income and Amount Spent",
        x = "Income",
        y = "Amount Spent"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  

  observe({
    req(data())
    updateSelectInput(session, "filter_year",
                      choices = c("All", sort(unique(data()$Enrollment_Year)))) # Using Enrollment_Year as "Year"
    updateSelectInput(session, "filter_marital_best_selling",
                      choices = c("All", unique(data()$Marital_Status))) # Using Marital_Status
  })
  
  filtered_data_category <- reactive({ # Renamed to avoid conflict
    df_filtered <- data()
    
    if (input$filter_year != "All") {
      df_filtered <- df_filtered %>% filter(Enrollment_Year == as.numeric(input$filter_year)) # Changed to Enrollment_Year
    }
    if (input$filter_marital_best_selling != "All") { # Updated input ID
      df_filtered <- df_filtered %>% filter(Marital_Status == input$filter_marital_best_selling) # Changed to Marital_Status
    }
    df_filtered
  })
  
  output$categoryPlot <- renderPlot({
    category_data <- filtered_data_category() %>%
      summarise(
        Wine = sum(MntWines, na.rm = TRUE),
        Meat = sum(MntMeatProducts, na.rm = TRUE),
        Fish = sum(MntFishProducts, na.rm = TRUE),
        Fruits = sum(MntFruits, na.rm = TRUE),
        Sweets = sum(MntSweetProducts, na.rm = TRUE),
        Gold = sum(MntGoldProds, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Sales")
    
    ggplot(category_data, aes(x = Category, y = Sales, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Best-selling Product Categories", x = "Category", y = "Total Sales")
  })
  

  summary_data_channels <- reactive({ # Renamed to avoid conflict
    data.frame(
      Channel = c("In-store", "Online"),
      NumPurchases = c(sum(data()$NumStorePurchases), sum(data()$NumWebPurchases)),
      TotalSpent = c(
        sum(data()$TotalSpent * data()$NumStorePurchases /
              (data()$NumStorePurchases + data()$NumWebPurchases + 1e-5), na.rm = TRUE),
        sum(data()$TotalSpent * data()$NumWebPurchases /
              (data()$NumStorePurchases + data()$NumWebPurchases + 1e-5), na.rm = TRUE)
      )
    )
  })
  
  output$purchasePlotChannel <- renderPlot({ # Renamed to avoid conflict
    df <- summary_data_channels()
    ggplot(df, aes(x = Channel)) +
      geom_bar(aes(y = NumPurchases), stat = "identity", fill = "blue") +
      geom_bar(aes(y = TotalSpent/10), stat = "identity", fill = "pink", alpha = 0.5) +
      labs(y = "Purchases / (Total Spent ÷ 10)", title = "Comparison of In-store vs. Online") +
      theme_minimal()
  })
  
  output$downloadPlotChannel <- downloadHandler(
    filename = function() {
      paste("purchase_plot_channels", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 7, height = 5)
    }
  )
}

shinyApp(ui = ui, server = server)