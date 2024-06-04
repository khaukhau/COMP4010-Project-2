

## Preparing task

library(tidyverse)
#install.packages("vroom")
library(vroom)
#install.packages("fs")
library(fs)
library(readr)
library(dplyr)
library(plotly)
#install.packages("plotly")
library(shiny)
library(bslib)
library(bsicons)
library(showtext)


#After processing, cleaned data would be used

paygap <- read_csv('data/cleaned/paygaps_2018_2022.csv')
companies <- read_csv('data/cleaned/companies.csv')

#Extract the Year from DueDate

paygap <- paygap %>%
  mutate(Year = year(ymd_hms(DueDate)))  


#Filtered by sector

# Function to filter sector by keyword
filter_by_keyword <- function(df, keywords) {
  pattern <- paste(keywords, collapse = "|")
  filtered_df <- df[grep(pattern, df$CurrentName, ignore.case = TRUE ),]
  return(filtered_df)
}

# List of keywords for different sectors
sectors_keywords <- list(
  Tech = c("Tech"),
  Finance = c("Financial", "Finance", "Insurance"),
  Education = c("Education"),
  Healthcare = c("Health"),
  Retail = c("Retail"),
  Entertainment = c("Entertainment", "Art"),
  Travel = c("Travel"),
  Consulting = c("Consulting"),
  Banking = c("Bank"),
  Marketing = c("Marketing")
)

# Dictionary to hold data frames for each sector
sector_data_frames <- map(sectors_keywords, filter_by_keyword, df = paygap)



#Create a theme

font_add_google("DM Sans", "DM Sans")
showtext_auto()
main_font = "DM Sans"

theme_general <- function() {
  theme_minimal(
    base_family = main_font,
    base_size = 11,
    base_line_size = 11/22,
    base_rect_size = 11/22
  ) + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.major.y = element_line(color = "grey80"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey80"),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(size = 12, hjust = 0.5, face = "bold"),
      axis.title.y = element_text(size = 12, hjust = 0.5, face = "bold"),
      legend.title = element_text(face = "bold")
    )
}


#Color pallete

pal <- c('#00202e', '#003f5c', '#2c4875', '#8a508f', '#bc5090', '#ff6361', '#ff8531', '#ffa600', '#ffd380','#35A29F', '#6ca0dc', '#ffc2d1')


## Task 1 (Kiet)

plot_diverging_bar_chart <- function(data, year, sector_name) {
  # Filter by year
  data <- data %>%
    mutate(DiffMeanHourlyPercent = as.numeric(DiffMeanHourlyPercent),
           Year = year(DueDate)) %>%
    filter(Year == year)
  
  max_val <- ceiling(max(abs(data$DiffMeanHourlyPercent), na.rm = TRUE))
  
  # Create the plot
  p <- ggplot(data, aes(x = reorder(CurrentName, DiffMeanHourlyPercent), y = DiffMeanHourlyPercent,
                        text = paste(CurrentName, 
                                     "<br>", sprintf("%.2f%%", DiffMeanHourlyPercent)))) +
    geom_bar(data = subset(data, DiffMeanHourlyPercent >= 0), stat = "identity", aes(y = DiffMeanHourlyPercent), fill = pal[11]) +
    geom_bar(data = subset(data, DiffMeanHourlyPercent < 0), stat = "identity", fill = pal[12]) +
    coord_flip() +
    scale_y_continuous(labels = function(x) abs(x)) +
    labs(title = paste("Diverging Bar Chart of", sector_name, "Sector Pay Gaps in", year),
         x = "Company Name",
         y = "Mean Hourly Pay Difference (%)") +
    theme_minimal() +
    theme_general() +
    theme(axis.text.y = element_text(size = 4),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", size = 0.5)) 
  
  ggplotly(p, tooltip = "text")
  #return(p)
}


## Task 2 (Nguyen)

plot_dumbbell_chart <- function(data) {
  p <- data %>%
    gather(key = "Gender_Quartile", value = "Percentage", 
           MaleLowerQuartile:FemaleTopQuartile) %>%
    separate(Gender_Quartile, into = c("Gender", "Quartile"), sep = "(?<=Male|Female)") %>%
    mutate(Quartile = factor(Quartile, levels = c("LowerQuartile", "LowerMiddleQuartile", "TopQuartile", "UpperMiddleQuartile"))) %>%
    group_by(Year, Gender, Quartile) %>%
    summarize(Percentage = mean(Percentage, na.rm = TRUE), .groups = 'drop') %>%
    ggplot(aes(x = Year, y = Percentage, group = interaction(Quartile, Year))) +
    geom_line() +
    geom_point(aes(color = Gender, text = paste(sprintf("%.2f%%", Percentage))), size = 3) +
    facet_wrap(~Quartile, scales = "free_y") +
    scale_color_manual(values = c("Male" = pal[11], "Female" = pal[12])) +
    labs(title = "Yearly Changes in Quartile Percentages",
         x = "Year",
         y = "Average Percentage",
         color = "Gender") +
    theme(legend.position = "bottom") +
    theme_general() +
    coord_flip()
  
  ggplotly(p, tooltip = "text")
}

#plot <- plot_dumbbell_chart(paygap)
#print(plot)


## Task 3 (Quan)

plot_gender_distribution <- function(df) {
    # Define the specific columns and their pairings
    selected_columns <- c("MaleLowerQuartile", "FemaleLowerQuartile", 
                          "MaleLowerMiddleQuartile", "FemaleLowerMiddleQuartile", 
                          "MaleUpperMiddleQuartile", "FemaleUpperMiddleQuartile", 
                          "MaleTopQuartile", "FemaleTopQuartile")
    
    # Create a factor with levels for ordering in the plot
    pairs <- factor(c("LowerQuartile", "LowerQuartile", 
                      "LowerMiddleQuartile", "LowerMiddleQuartile", 
                      "UpperMiddleQuartile", "UpperMiddleQuartile", 
                      "TopQuartile", "TopQuartile"),
                    levels = c("LowerQuartile", "LowerMiddleQuartile", "UpperMiddleQuartile", "TopQuartile"))
    
    # Subset the dataframe to include only selected columns
    df_subset <- select(df, all_of(selected_columns))
    
    # Calculate averages of each selected column
    averages <- colMeans(df_subset, na.rm = TRUE)  # Ensuring NA values are ignored
    
    # Convert averages to a dataframe and create a simpler gender identifier
    average_df <- data.frame(
      Column = names(averages),
      Average = as.vector(averages),
      Quartile = pairs,
      Gender = ifelse(grepl("Male", names(averages)), "Male", "Female")
    )
    
    # Define colors for each gender
    colors <- c("Male" = pal[11], "Female" = pal[12])
    
    # Plotting using ggplot2
    p <- ggplot(average_df, aes(x = Quartile, y = Average, fill = Gender,
                                text = paste(
                                  sprintf("%.2f%%", Average)))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_fill_manual(values = colors, name = "Gender",
                        labels = c("Female", "Male")) +
      labs(title = "Average Percentage by Gender Across Quartiles",
           x = "Quartile",
           y = "Average Percentage") +
      theme_general()  # Assuming theme_general is a custom theme; replace if necessary
    
    ggplotly(p, tooltip = "text")
}



## Buid Shiny App

ui <- fluidPage(
  # Header with Logo and Title
  div(
    style = "margin-bottom: 20px;",
    img(src = "uni_logo.png", height = "50px", width = "50px", style = "float: left; margin-right: 10px;"),
    h1("Gender Pay Gap Visualization", style = "position: relative; top: 10px;")
  ),
  
  # Main tabset panel for navigation
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Introduction", 
             fluidRow(
               column(12, 
                      h1("Welcome to the Gender Pay Gap Visualization"),
                      h2("Our Goal"),
                      p("Despite advancements in workplace equality, wage disparities between genders remain a significant challenge across various sectors.
We want to explore the gender pay gaps in the UK using the UK Pay Gap data to develop an interactive dashboard that illustrates wage disparities across industries and over time, enhancing understanding of pay inequality causes.
"),
                      p(HTML("<b>Objective:</b> Build an <b>interactive dashboard</b> visualizing the gender pay gap in various sectors like technology, finance, education, and healthcare, showing both current disparities and historical trends.")),
                      p(HTML("<b>Approach:</b> Utilize the <b>Shiny framework</b> to make complex data accessible and engaging, with interactive charts and graphs for sector comparisons and temporal analysis.
")),
                      p(HTML("<b>Impact:</b> Increase awareness and understanding of sector-specific pay gaps in the UK economy.
")),
                      p(HTML("<b>Innovation:</b> Use interactive elements to deepen user engagement and comprehension of how pay disparities vary across industries.
")),
                      actionButton("continue", "Continue")
               )
             )
    ),
    tabPanel("Overall Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sector", "Choose a Sector:", choices = names(sectors_keywords), multiple = FALSE)
               ),
               mainPanel(
                 plotlyOutput("dumbbellChart"),
                 p(HTML("The <b>dumbbell</b> chart represents the yearly changes by gender across <b>different income quartiles</b>. 
                 Each chart represents the gaps in pay for each gender from 2018 to 2022 for a specific income quartile. The <b>blue dots</b> represent the average percentage for <b>male employees</b>, 
                 while the <b>pink dots</b> represent the average percentage for <b>female employees</b>. This visualization helps to illustrate how the income gap would change across years for each income quartiles."))
               )
             )
    ),
    tabPanel("Sector Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sector", "Choose a Sector:", choices = names(sectors_keywords), multiple = FALSE),
                 selectInput("year", "Select Year:", choices = unique(paygap$Year))
               ),
               mainPanel(
                 plotlyOutput("incomeDistPlot"),
                 p(HTML("The <b>bar chart</b> represents the average percentage by gender across different income quartiles. Each pair of bars represents a specific quartile, categorized from <b>LowerQuartile</b> to <b>TopQuartile</b> from left to right. 
                 The <b>blue bars</b> represent the average percentage for <b>male employees</b>, while the <b>pink bars</b> represent the average percentage for <b>female employees</b> in each quartile. The height of each bar indicates the average percentage of employees 
                 in that particular gender and quartile category. This visualization helps to compare the gender distribution within different earning brackets across the sampled data.")),
                 plotlyOutput("divergingBarPlot"),
                 p(HTML("The <b>diverging bar chart</b> represents the difference in pay gap with respect to gender for <b>various companies</b>. The <b>blue bars</b> 
                 extending to the right indicate companies where <b>male employees</b> have higher mean hourly pay, 
                 while the <b>pink bars</b> extending to the left indicate companies where <b>female employees</b> earn more. 
                  The length of each bar represents the magnitude of the pay difference."))
               )
             )
    ),
    tabPanel("Team",
             div(
               h3("Meet Our Team"),
               fluidRow(
                 column(4,
                        img(src = "member1.jpg", height = "100px", style = "border-radius: 50%;"),
                        h4("Khau Lien Kiet"),
                        p("Email: 20kiet.kl@vinuni.edu.vn"),
                        p("Favorite Food: Jajangmyeon")
                 ),
                 column(4,
                        img(src = "member2.jpg", height = "100px", style = "border-radius: 50%;"),
                        h4("Hoang Khoi Nguyen"),
                        p("Email: 20nguyen.hk@vinuni.edu.vn"),
                        p("Favorite Food: Chicken fried rice")
                 ),
                 column(4,
                        img(src = "member3.jpg", height = "100px", style = "border-radius: 50%;"),
                        h4("Nguyen Duy Anh Quan"),
                        p("Email: 20quan.nda@vinuni.edu.vn"),
                        p("Favorite Food: Ramen")
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$continue, {
    updateTabsetPanel(session, "main_tabs", selected = "Overall Analysis")
  })
  
  data_filtered <- reactive({
    sector_data <- sector_data_frames[[input$sector]] %>% 
      filter(Year == as.numeric(input$year))
    return(sector_data)
  })
  
  output$dumbbellChart <- renderPlotly({
    req(sector_data_frames[[input$sector]])
    plot_dumbbell_chart(sector_data_frames[[input$sector]])
    #req(data_filtered())
    #plot_dumbbell_chart(data_filtered())
  })
  
  output$incomeDistPlot <- renderPlotly({
    req(data_filtered())
    plot_gender_distribution(data_filtered())
  })
  
  output$divergingBarPlot <- renderPlotly({
    req(data_filtered())
    plot_diverging_bar_chart(data_filtered(), input$year, input$sector)
  })
}




shinyApp(ui = ui, server = server)







