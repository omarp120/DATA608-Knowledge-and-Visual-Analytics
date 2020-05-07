#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(coronavirus)
library(geofacet)
library(openintro)
library(shinythemes)

###Initial project looked at Coronavirus cases by State and made use of the concept of small multiples

###United States data
#usC19 <- coronavirus[coronavirus$Country.Region == "US" & !(coronavirus$Province.State %in% c("Diamond Princess", "Grand Princess", "Guam", "Puerto Rico", "Virgin Islands", "Washington, D.C.")),]
#usC19$state <- state2abbr(usC19$Province.State)

#usC19 <- usC19 %>% group_by(state,type) %>% mutate(cv=cumsum(cases))
#usC19_wide <- spread(usC19, type, cv)
#usC19_wide$net <- usC19_wide$confirmed - usC19_wide$death - usC19_wide$recovered

###China data
#cn <- coronavirus[coronavirus$Country.Region == "China",]
#cn <- subset(cn, select=-c(Province.State, Lat, Long))
#cn1 <- cn %>% group_by(date,type) %>% mutate(cv1=sum(cases))
#cn1 <- subset(cn1, select=-c(cases))
#cn1 <- cn1 %>% distinct()

#cn1 <- cn1 %>% group_by(type) %>% mutate(cv2=cumsum(cv1))
#cn1 <- subset(cn1, select=-c(cv1))
#cn_wide <- spread(cn1, type, cv2)
#cn_wide$net <- cn_wide$confirmed - cn_wide$death - cn_wide$recovered

#ggplot(data=cn_wide) +
#  geom_line(aes(x = date, y = net)) +
#  labs(title = "Active COVID-19 Cases in China", subtitle = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository") +
#  theme_tufte(base_size = 11, base_family = "serif")


###Pivoted project based on changes in the data

#Initial dataset
gb <- coronavirus

#Running sums by country
gb1 <- subset(gb, select=-c(Province.State, Lat, Long))
gb1 <- gb1 %>% group_by(date, type, Country.Region) %>% mutate(cv1=sum(cases))
gb1 <- subset(gb1, select=-c(cases))
gb1 <- gb1 %>% distinct()

gb1 <- gb1 %>% group_by(Country.Region, type) %>% mutate(cv2=cumsum(cv1))
gb1 <- subset(gb1, select=-c(cv1))
gb_wide <- spread(gb1, type, cv2)
gb_wide$net <- gb_wide$confirmed - gb_wide$death - gb_wide$recovered

#Global sum
gs <- gb[gb$type == 'confirmed',]
gs1 <- subset(gs, select=-c(Country.Region, type))
gs2 <- gs1 %>% group_by(date) %>% mutate(cv1=sum(cases))
gs2 <- subset(gs2, select=-c(cases))
gs2 <- gs2 %>% distinct()

#Global running sum
gr <- subset(gb, select=-c(Country.Region, Province.State, Lat, Long))
gr1 <- gr %>% group_by(date, type) %>% mutate(cv1=sum(cases))
gr1 <- subset(gr1, select=-c(cases))
gr1 <- gr1 %>% distinct()

gr1 <- gr1 %>% group_by(type) %>% mutate(cv2=cumsum(cv1))
gr1 <- subset(gr1, select=-c(cv1))
gr_wide <- spread(gr1, type, cv2)
gr_wide$net <- gr_wide$confirmed - gr_wide$death - gr_wide$recovered

#Add text for explanations! 2 paragraphs

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  tags$head(tags$style("#info1{color: black;
                             font-size: 24px;
                       }"
                     )
  ),
  
  tags$head(tags$style("#info2{color: black;
                             font-size: 24px;
                       }"
                     )
  ),
  
  tags$head(tags$style("#intro{color: black;
                             font-size: 24px;
                       }"
                     )
  ),
  
  tags$head(tags$style("#ref{color: black;
                             font-size: 24px;
                       }"
                     )
  ),
  
  img(src="c-19.png", align = "right",height='50px',width='400px'),
  
  navbarPage("COVID-19 Pandemic: Is the Curve Flattening? by Omar Pineda Jr.",
             tabPanel("Introduction", textOutput("intro")
             ),
             tabPanel("Daily Confirmed Cases", 
                      tabsetPanel(type = "tabs",
                                  tabPanel("Info", textOutput("info1")),
                                  tabPanel("Globally", plotOutput("plt0"),
                                           sliderInput("Date_range_selector", "Select Date Range:",
                                                       min = as.Date(min(gb$date)), 
                                                       max = as.Date(max(gb$date)), 
                                                       value = c(as.Date(min(gb$date)), as.Date(max(gb$date)))
                                           )
                                  ),
                                  tabPanel("By Country", plotOutput("plt1"),
                                           sliderInput("Date_range_selector1", "Select Date Range:",
                                                       min = as.Date(min(gb$date)), 
                                                       max = as.Date(max(gb$date)), 
                                                       value = c(as.Date(min(gb$date)), as.Date(max(gb$date)))
                                           ),
                                           selectInput("country", "Country:",
                                                         width = "auto",
                                                         selected = "US",
                                                         choices=gb_wide$Country.Region, 1
                                           )
                                           
                                  )
                      )
             ),
             tabPanel("Total Active Cases",
                      tabsetPanel(type = "tabs",
                                  tabPanel("Info", textOutput("info2")),
                                  tabPanel("Globally", plotOutput("plt3"),
                                           sliderInput("Date_range_selector2", "Select Date Range:",
                                                       min = as.Date(min(gb$date)), 
                                                       max = as.Date(max(gb$date)), 
                                                       value = c(as.Date(min(gb$date)), as.Date(max(gb$date)))
                                           )
                                  ),
                                  tabPanel("By Country", plotOutput("plt4"),
                                           sliderInput("Date_range_selector3", "Select Date Range:",
                                                       min = as.Date(min(gb$date)), 
                                                       max = as.Date(max(gb$date)), 
                                                       value = c(as.Date(min(gb$date)), as.Date(max(gb$date)))
                                           ),
                                           selectInput("country1", "Country:",
                                                       width = "auto",
                                                       selected = "China",
                                                       choices=gb_wide$Country.Region, 1
                                           )
                                  )
                      )
             ),
             tabPanel("References", 
                      p("https://ramikrispin.github.io/coronavirus/, which compiles:"),
                      p("Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository: https://systems.jhu.edu/research/public-health/ncov/"),
                      p("World Health Organization (WHO): https://www.who.int/"),
                      p("DXY.cn. Pneumonia. 2020. http://3g.dxy.cn/newh5/view/pneumonia"),
                      p("BNO News: https://bnonews.com/index.php/2020/02/the-latest-coronavirus-cases/"),
                      p("National Health Commission of the People’s Republic of China (NHC): http:://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml"),
                      p("China CDC (CCDC): http:://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm"),
                      p("Hong Kong Department of Health: https://www.chp.gov.hk/en/features/102465.html"),
                      p("Macau Government: https://www.ssm.gov.mo/portal/"),
                      p("Taiwan CDC: https://sites.google.com/cdc.gov.tw/2019ncov/taiwan?authuser=0"),
                      p("US CDC: https://www.cdc.gov/coronavirus/2019-ncov/index.html"),
                      p("Government of Canada: https://www.canada.ca/en/public-health/services/diseases/coronavirus.html"),
                      p("Australia Government Department of Health: https://www.health.gov.au/news/coronavirus-update-at-a-glance"),
                      p("European Centre for Disease Prevention and Control (ECDC): https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases")
             )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plt0 <- renderPlot({
    ggplot(data=gs2[between(gs2$date, input$Date_range_selector[1], input$Date_range_selector[2]),], aes(x = date, y = cv1)) +
      geom_bar(stat = "identity") +
      labs(title = "Global Daily Confirmed New COVID-19 Cases", subtitle = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository") +
      theme_tufte(base_size = 11, base_family = "serif")
  })
  
  output$plt1 <- renderPlot({
    ggplot(data=gb[gb$type == 'confirmed' & gb$Country.Region == input$country & between(gb$date, input$Date_range_selector1[1], input$Date_range_selector1[2]),], aes(x = date, y = cases)) +
      geom_bar(stat = "identity") +
      labs(title = "Daily Confirmed New COVID-19 Cases by Country", subtitle = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository") +
      theme_tufte(base_size = 11, base_family = "serif")
  })
  
  output$plt3 <- renderPlot({
    ggplot(data=gr_wide[between(gr_wide$date, input$Date_range_selector2[1], input$Date_range_selector2[2]),]) +
      geom_line(aes(x = date, y = confirmed, col = "blue")) +
      geom_line(aes(x = date, y = net, col = "black")) +
      labs(title = "Global Total Confirmed vs Active (Confirmed - Recovered - Deceased) COVID-19 Cases", subtitle = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository") +
      scale_color_discrete(name = "Counts", labels = c("Net Active", "Confirmed")) +
      theme_tufte(base_size = 11, base_family = "serif") 
  })
  
  output$plt4 <- renderPlot({
    ggplot(data=gb_wide[gb_wide$Country.Region == input$country1 & between(gb_wide$date, input$Date_range_selector3[1], input$Date_range_selector3[2]),]) +
        geom_line(aes(x = date, y = confirmed, col = "blue")) +
        geom_line(aes(x = date, y = net, col = "black")) +
        labs(title = "Total Confirimed vs Active (Confirmed - Recovered - Deceased) COVID-19 Cases by Country", subtitle = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository") +
        scale_color_discrete(name = "Counts", labels = c("Net Active", "Confirmed")) +
        theme_tufte(base_size = 11, base_family = "serif") 
  })
  
  output$info1 <- renderText({
    "This section visualizes a flattening of the curve in terms of the number of new COVID-19 cases confirmed daily. Globally, it appears that the curve is either plateauing or
          on a decline, signaling that efforts to slow the pandemic such as quarantines and social distancing have been effective ('Globally' tab). We can also see this curve by individual
          countries ('By Country' tab). The default view shows the US, which seems to be on the decline in terms of new cases as well. If we
          select China, for example, we can see that there have been very few new daily cases in the last few weeks. There is
          also a filter to select the range of dates to view."
    })
  
  output$info2 <- renderText({
    "This section visualizes a flattening of the curve in terms of the running sum (total number) of confirmed
    COVID-19 cases. For comparison, we also plot the number of 'active' cases which we define as the running sum of confirmed cases - 
    recovered cases - deaths. Note that recovered cases were not included in this data source after 3/23/2020, but we can use
    the date filter to see how the curves looked back when recoveries were included. Our visualization shows that the total number of new confirmed cases has
    reached over 3 million. If we filter for China, there is a plateau in their total number of confirmed cases and a bell curve for their active
    cases as most of their recoveries were before 3/23/2020. If we slowly slide the date range over, we can see a dynamic visualization
    of how the curve has shaped out over time. Although the curve seems to be flattening, it is difficult to claim this definitively without complete
    records of the number of recovered cases."
    })
  
  output$intro <- renderText({
    "This dashboard is an interactive R Shiny app that leverages ggplot to visualize the trend in new and active COVID-19 cases globally and
     by country. This visualization is particularly relevant as many initiatives to contain the virus strive to flatten the curve, 
     essentially decreasing the total number of people carrying the virus and shortening the length of the pandemic. Other visualizations
     have focused on the number of new confirmed cases, but this dashboard attempts to look at the number of active carriers of the virus. We leverage Rami Krispin’s “coronavirus” R library which sources data from the Johns Hopkins University
     Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository. I have transformed the data using tidyr and also applied some of Tufte’s principles such as the data-ink ratio.
     The underlying data is at a country's province/state level in terms of daily new cases by type (confirmed, recovered or deaths). Note that after 3/23/2020 the US data became available
     as an aggregation rather than by individual states, which caused a pivot in this project's goal. The recovered cases globally were also removed
     from the dataset after the end of March.
    "
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

