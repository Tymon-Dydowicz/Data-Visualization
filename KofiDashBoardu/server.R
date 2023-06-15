#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(ggthemes)
library(sp)

# Define server logic required to draw a histogram
function(input, output, session) {
  set.seed(4444)
  
  #data and variable definition
  ##Data
  dailyPrices = read.csv("DataFalsification/False_Daily_Prices.csv")
  blends <- read.csv("DataFalsification/Aromas.csv")
  coffeeDrinkTypes <- read.csv("DataFalsification/False_Coffee_Types.csv")
  coffeeContents <- read.csv("DataFalsification/Contents.csv")
  ##Variables
  coffeeDrinkTypesNames <- coffeeDrinkTypes$coffee_type %>% unique()
  blendsNames <- blends %>% select(Blend) %>% as_tibble()
  blend <- reactiveVal(NULL)
  selected_row <- reactiveVal(NULL)
  selected_rows <- reactiveVal(NULL)
  selected_rows2 <- reactiveVal(NULL)
  selectedCoffeeDrinkType <- reactiveVal(NULL)
  coffeeType <- reactiveVal(NULL)
  empty_df <- data.frame(
    AvgPrice = numeric(),
    Overall.Rating = numeric(),
    Coffee.Beans = character(),
    stringsAsFactors = FALSE
  )
  emptyDFContents <- data.frame(
    CoffeeType = character(),
    attribute = character(),
    count = numeric()
  )
  colors <- c("Arabica" = "#8D5524", "Robusta" = "#C68642", "Liberica" = "#4b2e27", "Excelsa" = "#dca073")
  color_palette <- c("#8b572a", "#dca073", "#c19a6b", "#97694f", "#4b2e27", "#ffe6ba", "#ffdab3", "#4d4237", "#b28863", "#8c6333")
  consume <- read.csv('DataFalsification/Consumption.csv')
  consume <- consume %>% group_by(continent, type_of_coffee_bean) %>% summarise(yearly_consumption = sum(yearly_consumption))
  content_colors <- c("Milk" = "white",
                        "SteamedMilk" = "lavender",
                        "Milk.Foam" = "azure",
                        "Chocolate" = "chocolate",
                        "Syrup" = "maroon",
                        "Ice" = "lightblue",
                        "Caramel" = "goldenrod",
                        "WhippedCream" = "beige",
                        "HazelnutSyrup" = "peru",
                        "CocoaPowder" = "#4b2e27",
                        "Coffee" = "saddlebrown",
                        "Water" = "dodgerblue",
                        "IceCream" = "khaki")
  
  
  
  
  #default plots
  output$blendBarChart <- renderPlot({
    ggplot(empty_df, aes(x = AvgPrice, y = Overall.Rating)) +
      geom_bar(stat = "identity", fill = "#8D5524", width = 0.5) +
      geom_text(aes(label = Overall.Rating), hjust = -0.2, size = 5) +
      theme_solarized() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_y_continuous(limits = c(0, 10)) +
      ggtitle("Attributes of the Blend") +
      coord_flip()
    })
  output$blendScatterPlot <- renderPlotly({
    scatter_plot <- ggplot(empty_df, aes(x = AvgPrice, y = Overall.Rating, color = Coffee.Beans)) +
    geom_point(size = 4) +
    labs(x = "Price", y = "Rating") +
    ggtitle("Scatter Plot of Price vs Rating by Coffee Beans") +
    theme_solarized() +
    scale_color_manual(values = colors) + 
    scale_x_continuous(limits = c(1.5, 5)) + 
    scale_y_continuous(limits = c(6.5, 9.0))
    
    ggplotly(scatter_plot)
    })
  output$contentStackedBarChart <- renderPlotly({
    stackedBarChart <- ggplot(emptyDFContents, aes(fill=attribute, y=count, x=CoffeeType)) + 
      geom_bar(position="fill", stat="identity", width = 0.5) +
      scale_fill_manual(values = content_colors) + 
      labs(y = "Percentages", x = "Types of Coffee Drinks") +
      theme_solarized()
    
    ggplotly(stackedBarChart)
  })
  
  
  #usefull intermediary tables
  coffeeTypes <- blends %>% select(Coffee.Beans) %>% unique() %>% rename(Type = Coffee.Beans)
  blendsPrices <- blends %>% select(Blend, Overall.Rating, Coffee.Beans, AvgPrice)
  dailyPricesDF <- dailyPrices %>% filter(row_number()%%7 == 1) %>% gather(Type, Price, -Date)
  dailyPricesDF$Date <- as.Date(dailyPricesDF$Date)
  continent_consumption <- consume %>%
    group_by(continent) %>%
    summarize(total_consumption = sum(yearly_consumption))
  continentConsumption <- consume %>%
    left_join(continent_consumption, by = "continent") %>%
    mutate(percentage = yearly_consumption / total_consumption)
  
  
  #Functions
  generateDoughnut <- function(data, countryName){
    if(countryName != "World"){
      usedData <- data %>% 
        filter(country == countryName) %>%
        mutate(percentage = number_of_people / sum(number_of_people) * 100) %>%
        arrange(percentage) %>%
        mutate(ymax = cumsum(percentage), ymin = c(0, head(ymax, n=-1))) %>%
        mutate(labelPosition = (ymax + ymin) / 2) %>%
        mutate(label = sprintf("%s %.2f%%", coffee_type, percentage))
      countryName = paste("in", countryName)
    } else {
      usedData <- data %>% 
        group_by(coffee_type) %>% 
        summarise(number_of_people = sum(number_of_people)) %>%
        mutate(percentage = number_of_people / sum(number_of_people) * 100) %>%
        arrange(percentage) %>%
        mutate(ymax = cumsum(percentage), ymin = c(0, head(ymax, n=-1))) %>%
        mutate(labelPosition = (ymax + ymin) / 2) %>%
        mutate(label = sprintf("%s %.2f%%", coffee_type, percentage))
      countryName = "Around the World"
    }
    
    ggplot(usedData, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = coffee_type)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      coord_polar(theta = "y") +
      xlim(c(2,4)) +
      theme_void() + 
      scale_fill_manual(values = color_palette) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.background = element_rect(fill = "#fdf6e3")) + 
      ggtitle(paste("Most popular types of Coffee Drinks", countryName))
  }
  
  
  #Graphs
  plots <- list()
  countries <- c("World", "Poland", "USA", "Germany", "France", "Italy", "Spain", "Brazil", "Australia")
  for (country in countries) {
    plots[[country]] <- generateDoughnut(coffeeDrinkTypes, country)
  }
  
  
  
  
  output$dailyPrices <- renderPlotly({
    linechart <- ggplot(dailyPricesDF, aes(x=Date, y=Price, color=Type)) + 
      geom_line() + 
      geom_point( size = 1) +
      ggtitle("Daily Prices of Coffee Beans") + 
      xlab("Date") + 
      ylab("Price") +
      theme_solarized() +
      scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
      scale_color_manual(values = c("Arabica" = "#8D5524", "Robusta" = "#C68642", "Liberica" = "#4b2e27", "Excelsa" = "#dca073"))

    ggplotly(linechart)
  })
  
  
  output$blendsAromas <- DT::renderDataTable({
    DT::datatable(data.frame(blendsNames),
                  options = list(scrollY = 200,
                                 pageLength = 24,
                                 info = FALSE,
                                 paging = FALSE,
                                 searching = TRUE,
                                 ordering = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  selection = list(mode = 'single', target = 'row'),
                  rownames = FALSE)
  })
  observeEvent(input$blendsAromas_rows_selected, {
    selected_row(input$blendsAromas_rows_selected)
    print(selected_row)
    
    if (!is.null(selected_row)) {
      blend(blends[selected_row()[1], ])
      blend <- blend() %>%
        select(Aroma, Flavor, Acidity, Balance, Sweetness) %>%
        gather(attribute, rating)
      
      output$blendBarChart <- renderPlot({
        ggplot(blend, aes(x = attribute, y = rating)) +
          geom_bar(stat = "identity", fill = "#8D5524", width = 0.5) +
          geom_text(aes(label = rating), hjust = -0.2, size = 5) +
          theme_solarized() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
          scale_y_continuous(limits = c(0, 10)) +
          ggtitle("Attributes of the Blend") +
          coord_flip()
      })
    }
  })
  
  
  output$coffeeTypes <- DT::renderDataTable({
    DT::datatable(data.frame(coffeeTypes),
                  options = list(scrollY = 200,
                                 pageLength = length(coffeeTypes$Type),
                                 info = FALSE,
                                 paging = FALSE,
                                 searching = FALSE,
                                 ordering = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  selection = list(mode = 'multiple', target = 'row'),
                  rownames = FALSE)
  })
  observeEvent(input$coffeeTypes_rows_selected, {
    selected_rows(input$coffeeTypes_rows_selected)
    selectedTypes <- coffeeTypes[selected_rows(), ]
    print(selectedTypes)
    
    if(!is.null(selected_rows)){
      temp <- blendsPrices %>% filter(Coffee.Beans %in% selectedTypes)
      
      output$blendScatterPlot <- renderPlotly({
        scatter_plot <- ggplot(temp, aes(x = AvgPrice, y = Overall.Rating, color = Coffee.Beans)) +
          geom_point(size = 4) +
          labs(x = "Price", y = "Rating") +
          ggtitle("Scatter Plot of Price vs Rating by Coffee Beans") +
          theme_solarized() +
          scale_color_manual(values = colors) + 
          scale_x_continuous(limits = c(1.5, 5)) + 
          scale_y_continuous(limits = c(6.5, 9.0))
        
        
        ggplotly(scatter_plot)
      })
    }
  })
  
  
  output$continentConsumptionPieCharts <- renderPlot({
    ggplot(data = continentConsumption, aes(x = "", y = percentage, group = type_of_coffee_bean, fill = type_of_coffee_bean)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      facet_grid(. ~continent) +
      theme_solarized() +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0(type_of_coffee_bean, "\n", format(round(percentage*100, 2)), "%")), position = position_stack(vjust = 0.5), color = "white") +
      scale_fill_manual(values = colors) +
      labs(x = NULL, y = NULL) +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
  
  
  output$popularTypesOfCoffee <- renderPlot({
    plots[["World"]]
  })
  observeEvent(input$worldBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["World"]]
    })
  })
  observeEvent(input$polandBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["Poland"]]
    })
  })
  observeEvent(input$usaBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["USA"]]
    })
  })
  observeEvent(input$germanyBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["Germany"]]
    })
  })
  observeEvent(input$franceBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["France"]]
    })
  })
  observeEvent(input$italyBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["Italy"]]
    })
  })
  observeEvent(input$spainBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["Spain"]]
    })
  })
  observeEvent(input$brazilBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["Brazil"]]
    })
  })
  observeEvent(input$australiaBut, {
    output$popularTypesOfCoffee <- renderPlot({
      plots[["Australia"]]
    })
  })
  
  output$typesContent <- DT::renderDataTable({
    DT::datatable(data.frame(coffeeDrinkTypesNames),
                  options = list(scrollY = 200,
                                 pageLength = 10,
                                 info = FALSE,
                                 paging = FALSE,
                                 searching = TRUE,
                                 ordering = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  selection = list(mode = 'multiple', target = 'row'),
                  rownames = FALSE)
  })
  observeEvent(input$typesContent_rows_selected, {
    selected_rows2(input$typesContent_rows_selected)
    selectedCoffeeDrinkTypesNames <- coffeeDrinkTypesNames[selected_rows2()]
    
    if (!is.null(selected_rows2)) {
      print(selectedCoffeeDrinkTypesNames)
      
      output$contentStackedBarChart <- renderPlotly({
        usedDataContent <- coffeeContents %>%
          gather(key = "attribute", value = "count", -CoffeeType) %>%
          filter(CoffeeType %in% selectedCoffeeDrinkTypesNames)
        
        stackedBarChart <- ggplot(usedDataContent, aes(fill=attribute, y=count, x=CoffeeType)) + 
          geom_bar(position="fill", stat="identity", width = 0.5) +
          scale_fill_manual(values = content_colors) + 
          labs(y = "Percentages", x = "Types of Coffee Drinks") +
          theme_solarized()
        
        ggplotly(stackedBarChart)
      })
      
      }
    })
   
  observeEvent(event_data("plotly_selected"), {
    selected_data <- event_data("plotly_selected")
    View(selected_data)
    
    if (!is.null(selected_data)) {
      # Perform actions with the selected data
      # Access the selected data using selected_data$data
      # ...
    }
  })
  
  
}
