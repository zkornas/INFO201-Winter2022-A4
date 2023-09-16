#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("plotly")
library(dplyr)
library(tidyverse)
library(shiny)
library(plotly)

raw_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

continent_data <- raw_data %>%
  filter(country == "North America" |
           country == "South America" |
           country == "Europe" |
           country == "Africa" |
           country == "Asia" |
           country == "Australia")

country_data <- raw_data %>%
  filter(iso_code != "")
  
continent_highest_co2_per_capita <- continent_data %>%
  filter(year == max(year)) %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE))

continent_highest_co2_per_capita_name <- continent_highest_co2_per_capita %>%
  pull(country)

continent_highest_co2_per_capita_amount <- continent_highest_co2_per_capita %>%
  pull(co2_per_capita)

continent_lowest_co2_per_capita <- continent_data %>%
  filter(year == max(year)) %>%
  filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE))

continent_lowest_co2_per_capita_name <- continent_lowest_co2_per_capita %>%
  pull(country)

continent_lowest_co2_per_capita_amount <- continent_lowest_co2_per_capita %>%
  pull(co2_per_capita)

country_highest_co2_per_capita <- country_data %>%
  filter(year == max(year)) %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE))

country_highest_co2_per_capita_name <- country_highest_co2_per_capita %>%
  pull(country)

country_highest_co2_per_capita_amount <- country_highest_co2_per_capita %>%
  pull(co2_per_capita)

country_lowest_co2_per_capita <- country_data %>%
  filter(year == max(year)) %>%
  filter(co2_per_capita == min(co2_per_capita, na.rm = TRUE))

country_lowest_co2_per_capita_name <- country_lowest_co2_per_capita %>%
  pull(country)

country_lowest_co2_per_capita_amount <- country_lowest_co2_per_capita %>%
  pull(co2_per_capita)

global_co2_recent_year <- continent_data %>%
  filter(year == max(year))

global_co2_recent_year_amount <- global_co2_recent_year %>%
  pull(co2)

global_co2 <- sum(global_co2_recent_year$co2)





ui <- navbarPage(
  "CO2 Emissions and Climate Change Analysis",
  tabPanel(
    "Summary",
    mainPanel(
      p(strong("Zach Kornas - Assignment 4: Climate Data")),
      p(strong("Topic Introduction")),
      br("Climate change is one of the most serious issues we face in our today. The lives
         of future generations are at stake due to the unsustainable overconsumption in
         our modern world. While climate change has become increasingly politicized and debated,
         there are ways to objectively measure it's impact. One of the numeric measures of
         climate change is C02 emissions. We are able to measure total C02 produced by region
         as well as C02 produced per capita by region, which gives us a way to compare which regions
         are contributing the most to climate change."),
      p(strong("Values")),
      br("1. Which continent produced the most CO2 per capita in the most recent year?: ", continent_highest_co2_per_capita_name, "- ", continent_highest_co2_per_capita_amount, " (tonnes)"),
      br("2. Which continent produced the least CO2 per capita in the most recent year?: ", continent_lowest_co2_per_capita_name, "- ", continent_lowest_co2_per_capita_amount, " (tonnes)"),
      br("3. Which country produced the most CO2 per capita in the most recent year?: ", country_highest_co2_per_capita_name, "- ", country_highest_co2_per_capita_amount, " (tonnes)"),
      br("4. Which country produced the least CO2 per capita in the most recent year?: ", country_lowest_co2_per_capita_name, "- ", country_lowest_co2_per_capita_amount, " (tonnes)"),
      br("5. How much CO2 in total was released in the most recent year?: ", global_co2, "- (million tonnes)")
    )
  ),
  
  tabPanel(
    "CO2 emissions graph",
    sidebarLayout(
      sidebarPanel(
        selectInput("co2_type", 
                    label = h3("Select CO2 type"),
                    choices = c("Total CO2", "CO2 Per Capita"),
                    selected = "Total CO2"),
        
        radioButtons("select_continent", label = h3("Select A Continent"),
                     choices = c("North America", "South America", "Europe",
                                 "Africa", "Asia", "Australia"),
                     selected = "North America")
      ),
      
      mainPanel(
        plotlyOutput("continent_year"),
        p(strong("Description")),
        br("I decided it was best to include this chart, as it gives a greate visualization of how 
           CO2 emissions are changing by region over time. We are able to see that around the 1800's, 
           all continents were producing around the same level of CO2, nearly zero tonnes. As we enter 
           the industrial revolution in the 1900's, we see that regions in the Global North are producing 
           far more CO2 than the Global South. This could possibly be attributed to the wealth divide 
           between the Global North and South, with the Global North having the capital to produce, consume, 
           and dispose of material goods at a much more rapid rate than the Global South.")
      )
    )
  )
    
)

server <- function(input, output) {

  output$continent_year <- renderPlotly({
    if(input$select_continent == "North America") {
      selected.data <- continent_data %>% filter(country == "North America") %>%
        select(country, year, co2, co2_per_capita)
    } else if (input$select_continent == "South America") {
      selected.data <- continent_data %>% filter(country == "South America") %>%
        select(country, year, co2, co2_per_capita)
    }  else if (input$select_continent == "Europe") {
      selected.data <- continent_data %>% filter(country == "Europe") %>%
        select(country, year, co2, co2_per_capita)
    }  else if (input$select_continent == "Africa") {
      selected.data <- continent_data %>% filter(country == "Africa") %>%
        select(country, year, co2, co2_per_capita)
    }  else if (input$select_continent == "Asia") {
      selected.data <- continent_data %>% filter(country == "Asia") %>%
        select(country, year, co2, co2_per_capita)
    }  else if (input$select_continent == "Australia") {
      selected.data <- continent_data %>% filter(country == "Australia") %>%
        select(country, year, co2, co2_per_capita)
    }  else {
      selected.data <- continent_data %>% select(country, year, co2, co2_per_capita)
    }
    
    if(input$co2_type == "Total CO2") {
      selected.data <- selected.data %>%
        select(country, year, co2)
      y_title <- "Total CO2 (million tonnes)"
    } else {
      selected.data <- selected.data %>%
        select(country, year, co2_per_capita)
      y_title <- "CO2 Per Capita (tonnes)"
    }
    
    graph <- ggplot(data = selected.data, aes(x=year, y=unlist(selected.data[,3]))) +
      geom_line(stat = "identity") +
      theme(legend.position="none") +
      ylab(y_title) +
      xlab("Years") +
      ggtitle(paste0(input$co2_type, " from ", input$select_continent,
                     " over the years")) +
      scale_y_continuous(labels = scales::comma)
    
    g <- ggplotly(graph)
    return(g)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
