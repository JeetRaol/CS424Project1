library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)

aqi1980 <- read.table(file = "annual_aqi_by_county_1980.csv", sep = ",", header = TRUE)
aqi1981 <- read.table(file = "annual_aqi_by_county_1981.csv", sep = ",", header = TRUE)
aqi1982 <- read.table(file = "annual_aqi_by_county_1982.csv", sep = ",", header = TRUE)
aqi1983 <- read.table(file = "annual_aqi_by_county_1982.csv", sep = ",", header = TRUE)
aqi1983 <- read.table(file = "annual_aqi_by_county_1983.csv", sep = ",", header = TRUE)
aqi1984 <- read.table(file = "annual_aqi_by_county_1984.csv", sep = ",", header = TRUE)
aqi1985 <- read.table(file = "annual_aqi_by_county_1985.csv", sep = ",", header = TRUE)
aqi1986 <- read.table(file = "annual_aqi_by_county_1986.csv", sep = ",", header = TRUE)
aqi1987 <- read.table(file = "annual_aqi_by_county_1986.csv", sep = ",", header = TRUE)
aqi1987 <- read.table(file = "annual_aqi_by_county_1987.csv", sep = ",", header = TRUE)
aqi1988 <- read.table(file = "annual_aqi_by_county_1988.csv", sep = ",", header = TRUE)
aqi1989 <- read.table(file = "annual_aqi_by_county_1989.csv", sep = ",", header = TRUE)
aqi1990 <- read.table(file = "annual_aqi_by_county_1990.csv", sep = ",", header = TRUE)
aqi1991 <- read.table(file = "annual_aqi_by_county_1991.csv", sep = ",", header = TRUE)
aqi1992 <- read.table(file = "annual_aqi_by_county_1992.csv", sep = ",", header = TRUE)
aqi1993 <- read.table(file = "annual_aqi_by_county_1993.csv", sep = ",", header = TRUE)
aqi1994 <- read.table(file = "annual_aqi_by_county_1994.csv", sep = ",", header = TRUE)
aqi1995 <- read.table(file = "annual_aqi_by_county_1995.csv", sep = ",", header = TRUE)
aqi1996 <- read.table(file = "annual_aqi_by_county_1996.csv", sep = ",", header = TRUE)
aqi1997 <- read.table(file = "annual_aqi_by_county_1997.csv", sep = ",", header = TRUE)
aqi1998 <- read.table(file = "annual_aqi_by_county_1998.csv", sep = ",", header = TRUE)
aqi1999 <- read.table(file = "annual_aqi_by_county_1999.csv", sep = ",", header = TRUE)
aqi2000 <- read.table(file = "annual_aqi_by_county_2000.csv", sep = ",", header = TRUE)
aqi2001 <- read.table(file = "annual_aqi_by_county_2001.csv", sep = ",", header = TRUE)
aqi2002 <- read.table(file = "annual_aqi_by_county_2002.csv", sep = ",", header = TRUE)
aqi2003 <- read.table(file = "annual_aqi_by_county_2003.csv", sep = ",", header = TRUE)
aqi2004 <- read.table(file = "annual_aqi_by_county_2004.csv", sep = ",", header = TRUE)
aqi2005 <- read.table(file = "annual_aqi_by_county_2005.csv", sep = ",", header = TRUE)
aqi2006 <- read.table(file = "annual_aqi_by_county_2006.csv", sep = ",", header = TRUE)
aqi2007 <- read.table(file = "annual_aqi_by_county_2007.csv", sep = ",", header = TRUE)
aqi2008 <- read.table(file = "annual_aqi_by_county_2008.csv", sep = ",", header = TRUE)
aqi2009 <- read.table(file = "annual_aqi_by_county_2009.csv", sep = ",", header = TRUE)
aqi2010 <- read.table(file = "annual_aqi_by_county_2010.csv", sep = ",", header = TRUE)
aqi2011 <- read.table(file = "annual_aqi_by_county_2011.csv", sep = ",", header = TRUE)
aqi2012 <- read.table(file = "annual_aqi_by_county_2012.csv", sep = ",", header = TRUE)
aqi2013 <- read.table(file = "annual_aqi_by_county_2013.csv", sep = ",", header = TRUE)
aqi2014 <- read.table(file = "annual_aqi_by_county_2014.csv", sep = ",", header = TRUE)
aqi2015 <- read.table(file = "annual_aqi_by_county_2015.csv", sep = ",", header = TRUE)
aqi2016 <- read.table(file = "annual_aqi_by_county_2016.csv", sep = ",", header = TRUE)
aqi2017 <- read.table(file = "annual_aqi_by_county_2017.csv", sep = ",", header = TRUE)
aqi2018 <- read.table(file = "annual_aqi_by_county_2018.csv", sep = ",", header = TRUE)

allData <- rbind(aqi1980, aqi1981, aqi1982, aqi1983, aqi1984, aqi1985, aqi1986, aqi1987, aqi1988, aqi1989, aqi1990, aqi1991, aqi1992, aqi1993, aqi1994, aqi1995, aqi1996, aqi1997, aqi1998, aqi1999, aqi2000, aqi2001, aqi2002, aqi2003, aqi2004, aqi2005, aqi2006, aqi2007, aqi2008, aqi2009, aqi2010, aqi2011, aqi2012, aqi2013, aqi2014, aqi2015, aqi2016, aqi2017, aqi2018)
illinoisData <- subset(allData, subset = State == "Illinois")
cookData <- subset(illinoisData, subset = County == "Cook")

years <- c(1980:2018)

ui <- dashboardPage(
  dashboardHeader(title = "CS424 Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   selectInput("Year", "Select the year to see air quality information for Cook County, IL", years, selected = 2018),
                   sidebarMenu(
                     menuItem(text = "Visual", tabName = "visual", icon=icon("th")),
                     menuItem(text = "About", tabName = "about", icon=icon("clipboard"))
                   )
                  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "visual",
    fluidRow(
      column(4,
             fluidRow(
               box(title = "AQI Ratings by % of Days", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("pie1", height = 330)
                   )
             )
      ),
      column(4,
             fluidRow(
               box(title = "AQI Ratings by # of Days", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist1", height = 330)
               )
             )
      ),
      column(2,
             fluidRow(
               box(title = "AQI Ratings Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("tab1")
               )
             )
      ),
      column(2,
             fluidRow(
               box(title = "Leading Pollutant Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("tab2")
               )
             )
      )
    ),
    fluidRow(
      column(2,
             fluidRow(
               box(title = "CO as Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("copie", height = 125)
               )
             ),
             fluidRow(
               box(title = "PM2.5 as Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("pm25pie", height = 125)
               )
             )
      ),
      column(2,
             fluidRow(
               box(title = "NO2 as Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("no2pie", height = 125)
               )
             ),
             fluidRow(
               box(title = "PM10 as Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("pm10pie", height = 125)
               )
             )
      ),
      column(2,
             fluidRow(
               box(title = "Ozone as Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("ozonepie", height = 125)
               ),
               h5("Collected  data  may  not  be", align = "right")
             )
      ),
      column(2,
             fluidRow(
               box(title = "SO2 as Leading Pollutant", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("so2pie", height = 125)
               ),
               h5(HTML('&nbsp;'), "from  every  day  of  the  year.", align = "left")
             )
      ),
      column(4,
             fluidRow(
               box(title = "Leading Pollutants by # of Days", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("hist2", height = 330)
               )
             )
      )
    )
  ),
  tabItem(tabName = "about",
          h1(strong("About")),
          h2("Project created by: Jeet Raol"),
          h3("Libraries used to visualize data:"),
          h4("library(shiny)"),
          h4("library(shinydashboard)"),
          h4("library(ggplot2)"),
          h4("library(lubridate)"),
          h4("library(DT)"),
          h4("library(jpeg)"),
          h4("library(grid)"),
          h4("library(leaflet)"),
          h4("library(scales)"),
          h4("library(dplyr)"),
          h3("Where the data came from:"),
          h4("https://aqs.epa.gov/aqsweb/airdata/download_files.html")
  )
  )
  )
)

server <- function(input, output) {
  
  justOneYearReactive <- reactive({subset(cookData, cookData$Year == input$Year)})
  
  output$pie1 <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Good.Days"], justOneYear[1, "Moderate.Days"], justOneYear[1, "Unhealthy.for.Sensitive.Groups.Days"], justOneYear[1, "Unhealthy.Days"], justOneYear[1, "Very.Unhealthy.Days"], justOneYear[1, "Hazardous.Days"])
    categories <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Hazardous", "Very Unhealthy", "Unhealthy", "Unhealthy for Sensitive Groups", "Moderate", "Good")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      scale_fill_manual(values = c("dark red", "red", "orange", "yellow", "green", "dark green")) +
      labs(title = "AQI for Cook, Illinois") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "AQI Category"))
    
  })
  
  output$copie <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Days.CO"], justOneYear[1, "Days.with.AQI"] - justOneYear[1, "Days.CO"])
    categories <- c("CO", "Other")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Other", "CO")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "Leading Pollutant"))
    
  })
  
  output$no2pie <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Days.NO2"], justOneYear[1, "Days.with.AQI"] - justOneYear[1, "Days.NO2"])
    categories <- c("NO2", "Other")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Other", "NO2")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "Leading Pollutant"))
    
  })
  
  output$ozonepie <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Days.Ozone"], justOneYear[1, "Days.with.AQI"] - justOneYear[1, "Days.Ozone"])
    categories <- c("Ozone", "Other")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Other", "Ozone")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "Leading Pollutant"))
    
  })
  
  output$so2pie <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Days.SO2"], justOneYear[1, "Days.with.AQI"] - justOneYear[1, "Days.SO2"])
    categories <- c("SO2", "Other")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Other", "SO2")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "Leading Pollutant"))
    
  })
  
  output$pm25pie <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Days.PM2.5"], justOneYear[1, "Days.with.AQI"] - justOneYear[1, "Days.PM2.5"])
    categories <- c("PM2.5", "Other")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Other", "PM2.5")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "Leading Pollutant"))
    
  })
  
  output$pm10pie <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    cookillinoisaqi <- c(justOneYear[1, "Days.PM10"], justOneYear[1, "Days.with.AQI"] - justOneYear[1, "Days.PM10"])
    categories <- c("PM10", "Other")
    cookillinois_df <- data.frame(categories, cookillinoisaqi) %>%
      
      mutate(categories = factor(categories, levels = c("Other", "PM10")),
             cumulative = cumsum(cookillinoisaqi),
             midpoint = cumulative - cookillinoisaqi / 2,
             label = paste0(round(cookillinoisaqi / sum(cookillinoisaqi) * 100, 1), "%"))
    
    ggplot(cookillinois_df, aes(x = 1, weight = cookillinoisaqi, fill = categories)) +
      geom_bar(width=1,position = "stack") +
      coord_polar(theta="y") +
      geom_text(aes(x=1.7, y=midpoint, label=label)) +
      guides(fill = guide_legend(title = "Leading Pollutant"))
    
  })
  
  output$hist1 <- renderPlot({
    justOneYear <- justOneYearReactive()
    cookillinois_df <- data.frame(
    categories = factor(c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
                         levels=c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")),
    cookillinoisaqi = c(justOneYear[1, "Good.Days"], justOneYear[1, "Moderate.Days"], justOneYear[1, "Unhealthy.for.Sensitive.Groups.Days"], justOneYear[1, "Unhealthy.Days"], justOneYear[1, "Very.Unhealthy.Days"], justOneYear[1, "Hazardous.Days"])
    )
    aqidays = c(justOneYear[1, "Good.Days"], justOneYear[1, "Moderate.Days"], justOneYear[1, "Unhealthy.for.Sensitive.Groups.Days"], justOneYear[1, "Unhealthy.Days"], justOneYear[1, "Very.Unhealthy.Days"], justOneYear[1, "Hazardous.Days"])
    
    ggplot(data=cookillinois_df, aes(x=categories, y=cookillinoisaqi, fill=categories)) +
      geom_bar(stat="identity", fill=c("dark green", "green", "yellow", "orange", "red", "dark red")) +
      labs(x="AQI Category", y = paste0("# of Days (out of ", sum(aqidays),")"))
  })
  
  output$hist2 <- renderPlot({
    justOneYear <- justOneYearReactive()
    cookillinois_df <- data.frame(
      types = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"),
      cookillinoisaqi = c(justOneYear[1, "Days.CO"], justOneYear[1, "Days.NO2"], justOneYear[1, "Days.Ozone"], justOneYear[1, "Days.SO2"], justOneYear[1, "Days.PM2.5"], justOneYear[1, "Days.PM10"])
    )
    pollutiondays = c(justOneYear[1, "Days.CO"], justOneYear[1, "Days.NO2"], justOneYear[1, "Days.Ozone"], justOneYear[1, "Days.SO2"], justOneYear[1, "Days.PM2.5"], justOneYear[1, "Days.PM10"])
    
    ggplot(data=cookillinois_df, aes(x=types, y=cookillinoisaqi, fill=types)) +
      geom_bar(stat="identity", fill= "steelblue") +
      labs(x="Pollution Type", y = paste0("# of Days (out of ", sum(pollutiondays),")"))
  })
  
  output$tab1 <- DT::renderDataTable(
    DT::datatable({ 
      justOneYear <- justOneYearReactive()
      cookillinois_df <- data.frame(
        Category = factor(c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
                            levels=c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")),
        NumberofDays = c(justOneYear[1, "Good.Days"], justOneYear[1, "Moderate.Days"], justOneYear[1, "Unhealthy.for.Sensitive.Groups.Days"], justOneYear[1, "Unhealthy.Days"], justOneYear[1, "Very.Unhealthy.Days"], justOneYear[1, "Hazardous.Days"]))
    }, 
    options = list(searching = FALSE, paging = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$tab2 <- DT::renderDataTable(
    DT::datatable({ 
      justOneYear <- justOneYearReactive()
      cookillinois_df <- data.frame(
        Type = c("CO", "NO2", "Ozone", "SO2", "PM2.5", "PM10"),
        NumberofDays = c(justOneYear[1, "Days.CO"], justOneYear[1, "Days.NO2"], justOneYear[1, "Days.Ozone"], justOneYear[1, "Days.SO2"], justOneYear[1, "Days.PM2.5"], justOneYear[1, "Days.PM10"]))
    }, 
    options = list(searching = FALSE, paging = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui, server)