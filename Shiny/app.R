library(shiny)
library(shinydashboard)
library(tidyverse)
library(gridExtra)
library(viridis)
library(grid)
library(scales)
library(shinythemes)
first = function(x) {data %>%
    arrange(desc(gdp_per_capita)) %>%
    filter(Year == x) %>%
    slice(1:10) %>%
    ggplot(aes(gdp_per_capita, happiness_score, fill = gdp_per_capita)) +
    geom_point(aes(size = gdp_per_capita, col = viridis(10))) +
    geom_line(col = 'black') +
    geom_smooth(col = 'purple') +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 8)) +
    labs(title = paste("Year", x, sep = " "), x = "Gdp per Capita", y = "Happiness Score")}


ui = dashboardPage(skin = 'purple',
  dashboardHeader(title="Saikat Bera, MDS202228, saikatb@cmi.ac.in", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "menu1"),
      menuItem("Univariate Analysis",tabName = "menu2"),
      menuItem("Bivariate Analysis",tabName = "menu3"),      
      menuItem("Conclusion",tabName = "menu4")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("menu2",
              tabsetPanel(
                tabPanel("Change from 2015-2020", fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("variable", "choose the variable", choices = c('happiness_score', 'gdp_per_capita', 'family', 'health', 'freedom', 'generosity', 'government_trust', 'dystopia_residual')),
                    actionButton("go6", "apply")
                  ),
                  mainPanel(plotOutput("chart5"), textOutput("write5"))
                )
              )),
                tabPanel("Histograms", fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("column1", "choose the continent",choices = c("All", "Africa", "Asia", "Australia", "Europe", "North America", "South America")),
                      selectInput("var1", "choose variable", choices = c('happiness_score', 'gdp_per_capita', 'family', 'health', 'freedom', 'generosity', 'government_trust', 'dystopia_residual')),
                      actionButton("go1", "apply")
                    ),
                    mainPanel(plotOutput("chart1"),textOutput("write1"))
                  )
                )),
                tabPanel("Mean Happiness Score", fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("column2", "choose the continent", choices = c("All", "Africa", "Asia", "Australia", "Europe", "North America", "South America")),
                      actionButton("go5", "apply")
                    ),
                    mainPanel(plotOutput("chart4"), textOutput("write4"))
                  )
                ))
              )),
      tabItem("menu3",
              tabsetPanel(
                tabPanel("Line Plots", fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("year", "Choose a Year", choices = c('All', '2015', '2016', '2017', '2018', '2019', '2020')),
                      actionButton("go4", "apply")
                    ),
                    mainPanel(plotOutput("chart3"), textOutput("write3"))
                  )
                )),
                tabPanel("Scatter Plots", fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("varx", "choose variable x", choices = c('happiness_score', 'gdp_per_capita', 'family', 'health', 'freedom', 'generosity', 'government_trust', 'dystopia_residual')),
                      actionButton("go2", "apply"),
                      selectInput("vary", "choose variable y", choices = c('happiness_score', 'gdp_per_capita', 'family', 'health', 'freedom', 'generosity', 'government_trust', 'dystopia_residual')),
                      actionButton("go3", "apply")
                    ),
                    mainPanel(plotOutput("chart2"), textOutput("write2"))
                  )
                ))
              )),
      tabItem("menu1", h1("World Happiness And Corruption 2015-2020"),fluidPage(theme = shinytheme('yeti'),
        tags$b(tags$h2("Abstract :-")),
        tags$ul(tags$h4("Both corruption and happiness are of concern to academics, governments, and policymakers. The World
Bank and UNDP define corruption as “the misuse or the abuse of public office for private gain”. World
happiness is all about prosperity and economic growth. The real obstacle to world economic growth and
stability are corrupt international leaders. Corruption occurs in developed as well as developing countries due
to the opposing power of authorities and private interests. Therefore, the danger of corruption has become
an attractive issue of debate in recent years and I chose to dig into this.")), 
        tags$b(tags$h2("Introduction :-")),
        tags$ul(tags$h4("Finding correlations between other variables (like government trust, freedom, family size,
social support, CPI score etc.) and happiness score as well as visualizing which variables mostly affect the
happiness rank would be our primary challenge.")),
        tags$b(tags$h2("Our Hypothesis :-")),
        tags$ul(tags$h4("World happiness is correlated with government corruption. In particular they have
a strong negative correlation. That means high government corruption results in lower happiness in their
citizens.")),
        tags$b(tags$h3("Our Data :-"), tags$a(href = "https://www.kaggle.com/datasets/eliasturk/world-happiness-based-on-cpi-20152020", "Link to the Data Set")),
        tags$b(tags$h2("Data-Set Description :-")),
        tags$ul(tags$h4("ELIAS TURK preprocessed and aggregated all valid data for countries with
                       existent data from the year 2015 to 2020 using Pandas and he added the corruption perception index taken
                       from transparency. Joining CPI(Corruption Perception Index) was just a curiosity to find underlying answers
                       and findings. ELIAS TURK then merged the data sets based on Country and Year to transform the data
                       into a long data format. Government trust may already be taken from CPI scores, but CPI scores make it
                       clearer to understand and contrast it with the dependent variables in our case, happiness score.")),
        tags$b(tags$h2("Variables of Interest :-")),
        tableOutput("table"),
        tags$h4(tags$em("Note That :-"), "The missing data is filled with 0’s.")
      ))
  )
)
)

server = function(input, output) {
  data = read.csv("E:\\Coding\\Sourish Das\\Visualisation Project\\Data\\WorldHappiness_Corruption_2015_2020.csv")
  
  observeEvent(input$go1,
               if(input$column1 == "All"){
                 if(input$var1 == "happiness_score"){
                 output$chart1 = renderPlot(
                                              data %>%
                                              ggplot(aes(happiness_score, fill = continent, group=continent)) +
                                              geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                                              theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                                                    legend.justification = c(0, 1),
                                                    legend.position = c(0, 1),
                                                    legend.key.size = unit(1.25, 'mm'),
                                                    legend.title = element_text(size=10),
                                                    legend.text = element_text(size=8),
                                                    plot.title = element_text(hjust = 0.5),
                                                    plot.subtitle = element_text(hjust = 0.5)) +
                                              labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                                                   x = "Happiness Score", y = "Count", fill = "Continent")
                 )
                 output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "gdp_per_capita"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(gdp_per_capita, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "happiness_score"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(happiness_score, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "family"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(family, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "health"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(health, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "freedom"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(freedom, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "generosity"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(generosity, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "government_trust"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(government_trust, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }
                 else if(input$var1 == "dystopia_residual"){
                   output$chart1 = renderPlot(
                     data %>%
                       ggplot(aes(dystopia_residual, fill = continent, group=continent)) +
                       geom_histogram(bins = 20, col = 'black', alpha = 0.5) +
                       theme(legend.background = element_rect(fill = "white", size = 1, colour = "white"),
                             legend.justification = c(0, 1),
                             legend.position = c(0, 1),
                             legend.key.size = unit(1.25, 'mm'),
                             legend.title = element_text(size=10),
                             legend.text = element_text(size=8),
                             plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5)) +
                       labs(title = "Histogram Of Happiness Score \n(Grouped By Continent)",
                            x = "Happiness Score", y = "Count", fill = "Continent")
                   )
                   output$write1 = renderText("Agreegated histogram of Happiness Score with different Continent indexed by different color")
                 }}
               else {if (input$var1 == 'happiness_score'){
                 output$chart1 = renderPlot(data %>%
                                                 filter(continent == input$column1) %>% 
                                                 ggplot(aes(happiness_score, fill = continent)) +
                                                 geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                 facet_wrap(~continent) +
                                                 theme(legend.position = "none",
                                                       plot.title = element_text(hjust = 0.5),
                                                       plot.subtitle = element_text(hjust = 0.5)) +
                                                 labs(title = "Histogram Of Happiness Score",
                                                      x = "Happiness Score", y = "Count"))
               output$write1 = renderText("Histogram of Happiness Score for different Continent")
               }
                else if (input$var1 == 'gdp_per_capita'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(gdp_per_capita, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }
                 else if (input$var1 == 'family'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(family, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }
                 else if (input$var1 == 'freedom'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(freedom, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }
                 else if (input$var1 == 'health'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(health, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }
                 else if (input$var1 == 'generosity'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(generosity, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }
                 else if (input$var1 == 'government_trust'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(government_trust, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }
                 else if (input$var1 == 'dystopia_residual'){
                   output$chart1 = renderPlot(data %>%
                                                filter(continent == input$column1) %>% 
                                                ggplot(aes(dystopia_residual, fill = continent)) +
                                                geom_histogram(bins = 20, col = 'black', alpha = 0.5 ) +
                                                facet_wrap(~continent) +
                                                theme(legend.position = "none",
                                                      plot.title = element_text(hjust = 0.5),
                                                      plot.subtitle = element_text(hjust = 0.5)) +
                                                labs(title = "Histogram Of Happiness Score",
                                                     x = "Happiness Score", y = "Count"))
                   output$write1 = renderText("Histogram of Happiness Score for different Continent")
                 }})
                 output$chart2 = renderPlot({ var = data[ ,c(input$varx, input$vary)]
                                               data %>%
                                               ggplot(aes(var[,1], var[,2], color = continent)) +
                                               geom_point() +
                                               labs(x = input$varx , y = input$vary)
                   })
                 output$write2 = renderPrint({paste("Scatterplot of", input$varx, "vs", input$vary)})
  observeEvent(input$go4,
               if(input$year == 'All'){
                 output$chart3 = renderPlot(
                   grid.arrange(first(2015), first(2016), first(2017),
                                first(2018), first(2019), first(2020), ncol = 3,
                                top = textGrob("Relationship between Gdp per Capita And Happiness Score"))
                   
                 )
               }
               else if(input$year == '2015'){
                 output$chart3 = renderPlot(
                   first(2015)
                 )
               }
               else if(input$year == '2016'){
                 output$chart3 = renderPlot(
                   first(2016)
                 )
               }
               else if(input$year == '2017'){
                 output$chart3 = renderPlot(
                   first(2017)
                 )
               }
               else if(input$year == '2018'){
                 output$chart3 = renderPlot(
                   first(2018)
                 )
               }
               else if(input$year == '2019'){
                 output$chart3 = renderPlot(
                   first(2019)
                 )
               }
               else if(input$year == '2020'){
                 output$chart3 = renderPlot(
                   first(2020)
                 )
               })
  observeEvent(input$go5,
               if(input$column2 == "All"){
                 output$chart4 = renderPlot(
                   data %>%
                     group_by(continent, Year) %>%
                     summarise(mean_happy = mean(happiness_score)) %>%
                     ggplot(aes(as.double(Year), mean_happy, color = continent)) +
                     geom_line() +
                     geom_point(size = 4, alpha = 0.5) +
                     labs(title = "Change of mean Happiness over Years 2015-2020",
                          subtitle = "For different Continents",
                          x = "Year", y = "Mean Happiness Score") +
                     theme(plot.title = element_text(hjust = 0.5),
                           plot.subtitle = element_text(hjust = 0.5))
                   
                 )
               }
               else {
                 output$chart4 = renderPlot(
                   data %>%
                     group_by(continent, Year) %>%
                     summarise(mean_happy = mean(happiness_score)) %>%
                     filter(continent == input$column2) %>% 
                     ggplot(aes(as.double(Year), mean_happy, color = continent)) +
                     geom_line() +
                     geom_point(size = 4, alpha = 0.5) +
                     theme(legend.position = "none") +
                     labs(title = "Change of mean Happiness over Years 2015-2020",
                          subtitle = paste("For the Continent of", input$column2),
                          x = "Year", y = "Mean Happiness Score") +
                     theme(plot.title = element_text(hjust = 0.5),
                           plot.subtitle = element_text(hjust = 0.5))
                   
                 )
               })
    output$chart5 = renderPlot({
      data %>%
        filter(Year %in% c(max(Year), min(Year))) %>%
        select(Country, input$variable, Year) %>%
        pivot_wider(names_from = Year, values_from = input$variable, names_prefix = "year_") %>%
        group_by(Country) %>%
        summarise(change_in_happiness = year_2020 - year_2015) %>%
        mutate(Country = fct_lump(Country, 10, w = abs(change_in_happiness))) %>%
        mutate(Country = fct_reorder(Country, -change_in_happiness)) %>%
        filter(Country != "Other") %>%
        ggplot(aes(change_in_happiness, Country, fill = Country)) +
        geom_col() +
        scale_x_continuous(labels = comma) +
        labs(title = "Change in Happiness Score from 2015 to 2020",
             subtitle = "For Top 10 countries with most change",
             x = "Change in Happiness Score") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    })
    output$table<-renderTable({
      x=c("1. Country :","2. Happiness Score :","3. GDP per Capita :",
          "4. Family :", "5. Health :", "6. Freedom :","7. Generosity :",
          "8. Government Trust :", "9. Dystopia Residual :", "10. Continent :",
            "11. Year :", "12. Social Support :", "13. CPI Score :")
      y=c("Country",	"happiness_score",	"gdp_per_capita",	"family",	"health",	"freedom",	"generosity",	"government_trust",	"dystopia_residual",	"continent",	"Year",	"social_support",	"cpi_score")
      z=c("There is data on 132 unique countries over years.", "An average of responses to the primary life evaluation question from the Gallup
          World Poll (GWP). ( Scale : 0-10)", "Gross Domestic Product (GDP) per capita shows a country’s GDP divided by
          its total population.", "The extent to which Family contributes to the calculation of the Happiness Score.", "The extent to which Health(Life Expectancy) contributes to the calculation of the Happiness
          Score.", "The extent to which Freedom contributes to the calculation of the Happiness Score.", "A numerical value calculated based on poll participants’ perceptions of generosity in
          their country.", "The extent to which Perception of Corruption contributes to Happiness Score.", "A score based on a hypothetical comparison to the world’s saddest country.", "",
          "The data is collected across 6 years, i.e., 2015-2020.", "Social support is the perception and actuality that one is cared for, has assistance
          available from other people, and most popularly, that one is part of a supportive social network.", "Corruption perception index (CPI) is an index which ranks countries by their perceived
          levels of public sector corruption, as determined by expert assessments and opinion surveys.")
      nature_of_variable= data.frame(cbind("Variable Name"=x, "Column Name"=y, "Description"=z))
      nature_of_variable
    })
               
}

shinyApp(ui, server)

