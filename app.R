# Load necessary libraries
library(tidyverse)
library(vroom)
library(fs)
library(readr)
library(dplyr)
library(plotly)
library(shiny)
library(bslib)
library(bsicons)

# Load the cleaned data
paygap <- read_csv('data/cleaned/paygaps_2018_2022.csv')
companies <- read_csv('data/cleaned/companies.csv')

# Extract the Year from DueDate
paygap <- paygap %>%
  mutate(Year = year(ymd_hms(DueDate)))

# Function to filter sector by keyword
filter_by_keyword <- function(df, keywords) {
  pattern <- paste(keywords, collapse = "|")
  filtered_df <- df[grep(pattern, df$CurrentName, ignore.case = TRUE),]
  return(filtered_df)
}

# List of keywords for different sectors
sectors_keywords <- list(
  Tech = c("Tech", "IT"),
  Finance = c("Financial", "Finance", "Insurance"),
  Education = c("Education", "School"),
  Healthcare = c("Medical", "Pharmaceutical", "Health"),
  Retail = c("Retail"),
  Entertainment = c("Entertainment", "Art", "Recreation"),
  Travel = c("Travel"),
  Consulting = c("Consulting"),
  Banking = c("Bank"),
  Marketing = c("Marketing")
)

# Dictionary to hold data frames for each sector
sector_data_frames <- map(sectors_keywords, filter_by_keyword, df = paygap)

# Task 1: Plot Diverging Bar Chart
plot_diverging_bar_chart <- function(data, year, sector_name) {
  data <- data %>%
    mutate(DiffMeanHourlyPercent = as.numeric(DiffMeanHourlyPercent),
           Year = year(DueDate)) %>%
    filter(Year == year)
  
  max_val <- ceiling(max(abs(data$DiffMeanHourlyPercent), na.rm = TRUE))
  
  p <- ggplot(data, aes(x = reorder(CurrentName, DiffMeanHourlyPercent), y = DiffMeanHourlyPercent)) +
    geom_bar(data = subset(data, DiffMeanHourlyPercent >= 0), stat = "identity", aes(y = DiffMeanHourlyPercent), fill = "orange") +
    geom_bar(data = subset(data, DiffMeanHourlyPercent < 0), stat = "identity", fill = "lightblue") +
    coord_flip() +
    scale_y_continuous(labels = function(x) abs(x)) +
    labs(title = paste("Diverging Bar Chart of", sector_name, "Sector Pay Gaps in", year),
         x = "Company Name",
         y = "Mean Hourly Pay Difference (%)") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 4),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", size = 0.5))
  
  return(p)
}

# Task 3: Plot Gender Distribution
plot_gender_distribution <- function(df) {
  df %>%
    gather(key = "Quartile", value = "Percentage", MaleLowerQuartile:FemaleTopQuartile) %>%
    separate(Quartile, into = c("Gender", "Quartile"), sep = "(?<=Male|Female)") %>%
    mutate(Quartile = factor(Quartile, levels = c("LowerQuartile", "LowerMiddleQuartile", "UpperMiddleQuartile", "TopQuartile"))) %>%
    ggplot(aes(x = Quartile, y = Percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Gender Pay Gap Distribution",
         x = "Income Quartile",
         y = "Percentage of Population",
         fill = "Gender") +
    theme_minimal()
}

# Define UI
ui <- fluidPage(
  titlePanel("Gender Pay Gap Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sector", "Choose a Sector:", choices = names(sectors_keywords), multiple = FALSE),
      selectInput("year", "Select Year:", choices = unique(paygap$Year)),
      actionButton("go", "Generate Graph")
    ),
    mainPanel(
      plotOutput("incomeDistPlot"), 
      plotOutput("divergingBarPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  data_filtered <- reactive({
    req(input$go)
    selected_sectors <- input$sector
    year_selected <- as.numeric(input$year)
    
    combined_data <- do.call(rbind, lapply(selected_sectors, function(sector) {
      sector_data_frames[[sector]] %>% filter(Year == year_selected)
    }))
    
    return(combined_data)
  })
  
  output$incomeDistPlot <- renderPlot({
    req(data_filtered())
    plot_gender_distribution(data_filtered())
  })
  
  output$divergingBarPlot <- renderPlot({
    req(data_filtered())
    plot_diverging_bar_chart(data_filtered(), input$year, input$sector)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
