#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(dashboardthemes)

# Define UI for application that draws a histogram
dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Kofi Dashboard"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
      menuItem("Blends", tabName = "Blends", icon = icon("chart-bar")),
      menuItem("Types of Coffee", tabName = "TypesOfCoffee", icon = icon("coffee")),
      menuItem("Help/About", tabName = "Help", icon = icon("question-circle"))
    ),
    tags$style(HTML('
      .sidebar-bottom {
        display: flex;
        justify-content: center;
        align-items: flex-end;
        padding-bottom: 10px;
      }
    ')),
    tags$div(
      class = "sidebar-bottom",
      tags$img(src = "PP_znak_pełny_RGB.png", height = "100px", width = "100px")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Overview",
              box(
                plotlyOutput("dailyPrices")
              ),
              box(
                actionButton("clearBut", "Clear"),
                actionButton("arabicaBut", "Arabica"),
                actionButton("robustaBut", "Robusta"),
                actionButton("libericaBut", "Liberica"),
                actionButton("excelsaBut", "Excelsa"),
                plotOutput("continentConsumptionPieCharts")
  
              )
      ),
      tabItem(tabName = "Blends",
              box(
                DT::dataTableOutput("blendsAromas"),
                plotOutput("blendBarChart")
              ),
              box(
                DT::dataTableOutput("coffeeTypes"),
                plotlyOutput("blendScatterPlot")
              )
      ),
      tabItem(tabName = "TypesOfCoffee",
              box(
                actionButton("worldBut", "World"),
                actionButton("polandBut", "Poland"),
                actionButton("usaBut", "USA"),
                actionButton("germanyBut", "Germany"),
                actionButton("franceBut", "France"),
                actionButton("italyBut", "Italy"),
                actionButton("spainBut", "Spain"),
                actionButton("brazilBut", "Brazil"),
                actionButton("australiaBut", "Australia"),
                plotOutput("popularTypesOfCoffee")
              ),
              box(
                DT::dataTableOutput("typesContent"),
                plotlyOutput("contentStackedBarChart")
              )
      ),
      tabItem(tabName = "Help",
              h2("About"),
              p("Welcome to our Coffee Analysis Dashboard! We are passionate about coffee and dedicated to providing insights and information about the world of coffee. Our web application aims to bring together data, visualizations, and analysis to help coffee enthusiasts, professionals, and curious minds explore the fascinating realm of coffee."),
              p("With our interactive charts and graphs, you can dive into the world of coffee blends, aromas, flavors, and more. Discover the diverse range of coffee types, their characteristics, and how they are rated by experts. Explore the relationship between price and rating, and find the perfect blend that suits your taste preferences."),
              p("We also offer valuable insights into coffee consumption trends worldwide. Delve into the data to understand coffee consumption patterns across different continents and popular coffee drink types. Our visualizations provide a comprehensive view of coffee preferences and highlight the most popular coffee drinks globally."),
              p("Whether you're a coffee aficionado, a coffee shop owner, or simply someone who enjoys a good cup of joe, our Coffee Analysis Dashboard is here to inform and entertain. We believe that understanding the intricacies of coffee enhances the overall coffee experience, and we're excited to share our findings with you."),
              p("So grab a cup of your favorite coffee, explore the various sections of our dashboard, and embark on a journey of coffee discovery. Cheers!"),
              h3("Main Functionalties"),
              p("In the dashboard we currently feature 3 tabs: Overview, Blends, Types of Coffee"),
              p("On the 'Overview' page you can see the price trend of 4 main types of coffee beans, as well as percentage yearly consumption of each of them"),
              p("On 'Blends' page you can see a scatter plot of blends rating to it's price and from what types of beans it was made. You can also select blends and check their specifications"),
              p("On 'Types of Coffee' page you can see how popular different types of coffee drinks are around the world and in particular country as well as how each type is made and compare it against other types"),
              h3("Contact"),
              p("For further assistance, please refer to the", a("documentation", href = "https://example.com/documentation"), "or contact me on my mail", a("tymon.dydowicz@onet.pl", href = "tymon.dydowicz@onet.pl"), "."),
              # Add more content as needed
      )
    )
  )
)
