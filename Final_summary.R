library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(plotly)
library(DT)

#Loading data
strategy <- read.csv("C:/Users/imogen.hamer/OneDrive - Ocean Infinity Group Limited/Documents/ShinyApps/Final_summary/Strategy_Data_Year.csv")
colnames(strategy)[1] <- "Goal"
strategy$Goal <- as.factor(strategy$Goal)
strategy$Objective <- as.factor(strategy$Objective)
strategy$Methodology <- as.factor(strategy$Methodology)

total <- strategy %>%
  group_by(Goal, Objective, Methodology) %>%
  summarise(Total_EUR = sum(Total_EUR))
Metric = rep("Total", times = 27)
Metric <- as.data.frame("Metric")
total <- cbind(Metric, total)

average <-  strategy %>%
  group_by(Goal, Objective, Methodology) %>%
  summarise(Total_EUR = mean(Total_EUR))
Metric <- rep("Average", times = 27)
Metric <- as.data.frame("Metric")
average <- cbind(Metric, average)

all_metric <- rbind(total, average)

long_strat <- strategy %>%
  pivot_longer(
    cols = starts_with("EUR_2"), 
    names_to = "Year", 
    names_prefix = "EUR_",
    values_to = "Spend"
  ) %>%
  drop_na("Spend")

long_strat$Project_total <- long_strat$Total_EUR


total2 <- long_strat %>%
  group_by(Year, Goal, Objective, Methodology, Project, Organisation, Project_total) %>%
  summarise(Total_EUR = sum(Spend)) 

Metric2 = rep("Total", times = 84)
Metric2 <- as.data.frame(Metric2)
total2 <- cbind(Metric2, total2)

average2 <-  long_strat %>%
  group_by(Year, Goal, Objective, Methodology, Project, Organisation, Project_total) %>%
  summarise(Total_EUR = mean(Spend))

Metric2 <- rep("Average", times = 84)
Metric2 <- as.data.frame(Metric2)
average2 <- cbind(Metric2, average2)

all_metric2 <- rbind(total2, average2)

#ui.R
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      body {
        background-color: #d2e1ee;
        color: #030160;
        margin: 0; 
      }
      #plot {
        border: 3px solid #aec9e1; 
        border-radius: 9px; 
        overflow: hidden; 
      }
      .navbar {
        background-color: #d2e1ee;
      }
      #plot-container {
        background-color: #d2e1ee;
        padding-top: 60px; 
      }
      .nav-sidebar {
        color: #030160;
      }
      .form-group label {
        color: #030160;
      }
      .form-group .slider-container .irs .irs-line {
        background: #030160;
      }
      .form-group .slider-container .irs .irs-bar {
        background: #030160;
      }
      .navbar-brand img {
        max-height: 40px;
        margin: 10px; 
      }
    .title-panel {
        margin-top: 30px; 
      }
      .sidebar-layout {
        margin-top: 30px; 
      }
     table {
        border-collapse: separate;
        border-spacing: 0;
        width: 100%;
        border: 2px solid #aec9e1; 
        border-radius: 5px; 
        overflow: hidden; 
      }
      th, td {
        border: 1px solid #aec9e1; 
        padding: 8px;
        text-align: left;
      }
    ")
    ),
    tags$img(src = "https://github.com/imbmgh20/Flotilla_dashboard/blob/main/Flotilla_Logo.png?raw=true",
             alt = "Logo", style = "max-height: 60px; position: fixed; top: 10px; right: 10px;")
  ),
  titlePanel(
    h1("Strategy spending breakdown", style = "color: #030160; font-size: 40px; font-weight: 600;padding: 10px")
  ),
  sidebarLayout(
    sidebarPanel(width = 3,
                 style = "background-color: #d2e1ee; border: 3px solid #aec9e1; border-radius: 5px; padding: 10px;",
                 selectInput(inputId = "Goal",
                             label = "1. Select Goal",
                             choices = c("Pollution" = "Pollution",
                                         "Climate Resilience" = "Climate resilience"),
                             selected = "Pollution"),      
                 selectInput(inputId = "Metric",
                             label = "2. Select Metric",
                             choices = c("Total spend" = "Total",
                                         "Average spend" = "Average"),
                             selected = "Total spend"),
                 
                 sliderInput(inputId = "Year",
                             label = "3. Time period",
                             min = 2021,
                             max = 2026, 
                             value = c(2021, 2023), 
                             sep = "")),
    mainPanel(
      plotlyOutput("plot", width = "100%"),
      br(),
      DTOutput("interactive_table")  
    )
  )
)

#server.R
server <- function(input, output){
  # Render the plot using renderPlotly
  output$plot <- renderPlotly({
    dfr <- all_metric2 %>%
      filter(Year >= input$Year[1], Year <= input$Year[2])
    
    p <- ggplot(dfr, aes(x = str_wrap(Objective, width = 12),
                         y = Total_EUR, fill = Methodology)) +
      geom_bar(stat = "identity",
               data = dfr[dfr$Metric == input$Metric &
                            dfr$Goal == input$Goal,]) +
      theme_classic() +
      theme(axis.text.x = element_text(size = 10, color = "#030160"),
            axis.text.y = element_text(size = 10, color = "#030160"),
            axis.title = element_text(size = 13, face = "bold", color = "#030160"),
            legend.title = element_text(size = 12, face = "bold", color = "#030160"),
            legend.text = element_text(size = 11, color = "#030160"), 
            plot.background = element_rect(fill = "#d2e1ee"),
            panel.background = element_rect(fill = "#d2e1ee"), 
            legend.background = element_rect(fill = "#d2e1ee")) +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Spend (EUR)", x = "Objective")
    
    ggplotly(p, tooltip = c("Date", "Methodology", "Total_EUR"))
  })
  
  output$interactive_table <- renderDT({
    long_strat_filtered <- all_metric2 %>%
      filter(Year >= input$Year[1], Year <= input$Year[2],
             Goal == input$Goal,
             Metric2 == input$Metric)  
    
    long_strat_filtered %>%
      select(Objective, Methodology, Organisation,Project, Year,
             "Yearly Spend (EUR)" = Total_EUR, "Project Spend (EUR)" = Project_total) %>%
      datatable(
        options = list(
          searching = TRUE,
          paging = TRUE, 
          pageLength = 5,  
          lengthMenu = c(5, 10, 20, 50, 100)
        )
      )
  })
}

#Run app
shinyApp(ui=ui, server=server) 

