library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(shinyWidgets)
library(stringr)
library(lubridate)
library(countrycode)
library(sp)
library(sf)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(plotly)

# Function to add appropriate spaces without repeating code
linebreaks <- function(n){HTML(strrep(br(), n))}

###############################################
#  READ IN DATA / DATA PREPARATION
###############################################
data <- read.csv('owid-covid-data.csv')
data <- data[!grepl('OWID', data$iso_code),]
data[is.na(data)] <- 0
data$name <- countrycode(data$iso_code, 'iso3c', 'country.name')
data$date <- as.Date(data$date)

world <- read_sf('custom.geo.json')

# Grouped Data
group_data <- data %>%
  select(name, total_cases, total_deaths, total_cases_per_million, people_fully_vaccinated, people_fully_vaccinated_per_hundred,
         total_deaths_per_million, total_tests, total_tests_per_thousand,
         total_vaccinations, total_vaccinations_per_hundred) %>%
  group_by(name) %>%
  summarise(name= nth(name, -2L), total_cases=nth(total_cases, -2L), total_deaths=nth(total_deaths, -2L),
            total_cases_per_million=nth(total_cases_per_million, -2L), people_fully_vaccinated=nth(people_fully_vaccinated, -2L),
            people_fully_vaccinated_per_hundred=nth(people_fully_vaccinated_per_hundred,-2L), total_deaths_per_million=nth(total_deaths_per_million,-2L),
            total_tests=nth(total_tests, -2L), total_tests_per_thousand=nth(total_tests_per_thousand,-2L),
            total_vaccinations=nth(total_vaccinations, -2L), total_vaccinations_per_hundred=nth(total_vaccinations_per_hundred, -2L))

group_data <- sp::merge(x=group_data, y=world, by='name', all.x = T)

# Demographics Data
ca <- read.csv('california_demo.csv')
ca_prop <- read.csv('california_demo_prop.csv')
death_prop <- ca_prop[1:7,]
death_race <- ca[1:7,]
case_prop <- ca_prop[9:15,]
case_race <- ca[9:15,]

###############################################
#  USER INTERFACE
###############################################
ui <- dashboardPage(
  
  title='Covid-19 Dashboard',
  
  # Header
  dashboardHeader(title='COVID 19 Dashboard'),
  
  
  # Sideboard
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geographic Data", tabName = 'geo', icon=icon('globe-americas')),
      menuItem("Regional Data", tabName = 'reg', icon=icon('hand-holding-medical')),
      menuItem("Demographic Data", tabName = 'demo', icon=icon('restroom')),
      menuItem("Raw Data Table", tabName = 'table', icon=icon('table'))
    )
  ),
  
  # Main Body
  dashboardBody(
    
    shinyDashboardThemes(
      theme='blue_gradient'
    ),
    
    tabItems(
      
      # GEOGRAPHIC DATA UI TAB
      tabItem(tabName='geo',
              column(width=2,
                     fluidRow(
                       linebreaks(25),
                       selectInput('metric', '', choices=c('Cases', 'Deaths', 'Vaccination'), selected ='Cases'),
                       selectInput('quant', '', choices=c('Total', 'Per Capita'), selected='Total')
                       
                     )
               ),
              column(width=10, 
                     fluidRow(
                       tags$style(type = "text/css", ".leaflet-container {background-color:rgba(255,0,0,0.0);} "),
                       h2('World Map of Covid-19 Pandemic', style="color: #184464; text-align: center"),
                       leafletOutput('world_map', width='100%', height = 750),
                       p('Note: Some data may be missing or outdated. This data is sourced from: '),
                       a('Our World In Data', href= 'https://ourworldindata.org/coronavirus')
                     )
              )
      ),
      
      # REGIONAL DATA UI TAB
      tabItem(tabName = 'reg',
              h1('Regional Statistics', style="color: #184464; text-align: center"),
              
              fluidRow(
                column(width=6,
                          h2('North America'),
                          h4('59,348,249 Total Cases'),
                          plotlyOutput('NRA')),
                column(width=6,
                          h2('South America'),
                          h4('39,022,166 Total Cases'),
                          plotlyOutput('SA'))
              ),
              
              fluidRow(
                linebreaks(4),
                column(width=6,
                        h2('Europe'),
                        h4('74,508,737 Total Cases'),
                        plotlyOutput('EU')),
                column(width=6,
                        h2('Asia'),
                        h4('82,288,806 Total Cases'),
                        plotlyOutput('AS'))
              ),
              
              fluidRow(
                linebreaks(4),
                column(width=6,
                         h2('Africa'),
                         h4('8,761,409 Total Cases'),
                         plotlyOutput('AF')),
                column(width=6,
                         h2('Oceania'),
                         h4('372,122 Total Cases'),
                         plotlyOutput('OC'))
              )
      ),
      
      # DEMOGRAPHIC UI TAB
      tabItem(tabName = 'demo',
              linebreaks(4),
              column(width=3,
                     h1('Racial Inequalities in the United States', style="color: #184464; text-align: center"),
                     linebreaks(15),
                     checkboxGroupInput('race', 'Race',
                                        choices=c('Latino', 'White', 'Asian', 'African American', 'Multi-Race',
                                                  'American Indian or Alaska Native', 'Native Hawaiian and other Pacific Islander'),
                                        selected=c('Latino', 'White', 'Asian', 'African American', 'Multi-Race',
                                                   'American Indian or Alaska Native', 'Native Hawaiian and other Pacific Islander'))
               ),
               column(width=9,
                   fluidRow(
                     plotlyOutput('race_case')
                   ),
                   linebreaks(2),
                   fluidRow(
                     plotlyOutput('race_death')
                   )
                )
      ),
      
      # DATATABLE UI TAB
      tabItem(tabName = 'table',
              dataTableOutput('world_datatable')
      )
    )
  )
)

server <- function(input, output, session){
  
  
  # WORLD MAP
  output$world_map <- renderLeaflet({
    if(input$quant == 'Total'){
      if(input$metric == 'Cases'){
        pal <- colorBin('Blues', domain=group_data$total_cases,
                        bins = c(0,5000,50000,500000,5000000,50000000), na.color='#FFFFFF')
        title <- 'Total Cases'
        fillColor <- pal(group_data$total_cases)
        values <- group_data$total_cases
      }
      
      if(input$metric == 'Deaths'){
        pal <- colorBin('YlOrRd', domain=group_data$total_deaths,
                        bins = c(0,10000,100000,250000,500000,1000000), na.color='#FFFFFF')
        title <- 'Total Deaths'
        fillColor <- pal(group_data$total_deaths)
        values <- group_data$total_deaths
      }
      
      if(input$metric == 'Vaccination'){
        pal <- colorBin('BuGn', domain=group_data$total_vaccinations,
                        bins = c(0,1000000, 10000000, 100000000, 1000000000, 10000000000), na.color='#FFFFFF')
        title <- 'Total Doses Administered'
        fillColor <- pal(group_data$total_vaccinations)
        values <- group_data$total_vaccinations
      }
      
      popup_text <- paste0("<strong>", group_data$name, '</strong>', "<br>",
                           "Cases: ", format(group_data$total_cases, big.mark = ','), "<br>",
                           "Deaths: ", format(group_data$total_deaths, big.mark = ','), "<br>",
                           "Fully Vaccinated People: ", format(group_data$people_fully_vaccinated, big.mark=','), "<br>",
                           "Total Doses: ", format(group_data$total_vaccinations, big.mark = ','))
    }
    
    if(input$quant == 'Per Capita'){
      if(input$metric == 'Cases'){
        pal <- colorBin('Blues', domain=group_data$total_cases_per_million,
                        bins = c(0,5000,50000,100000,150000,300000), na.color='#FFFFFF')
        title <- 'Total Cases per Million'
        fillColor <- pal(group_data$total_cases_per_million)
        values <- group_data$total_cases_per_million
      }
      
      if(input$metric == 'Deaths'){
        pal <- colorBin('YlOrRd', domain=group_data$total_deaths_per_million,
                        bins = c(0,1000,2000,3000,4500,7500), na.color='#FFFFFF')
        title <- 'Total Deaths per Million'
        fillColor <- pal(group_data$total_deaths_per_million)
        values <- group_data$total_deaths_per_million
      }
      
      if(input$metric == 'Vaccination'){
        pal <- colorBin('BuGn', domain=group_data$total_vaccinations_per_hundred,
                        bins = c(0,50, 100, 150, 200, 300), na.color='#FFFFFF')
        title <- 'Total Doses Administered per Hundred'
        fillColor <- pal(group_data$total_vaccinations_per_hundred)
        values <- group_data$total_vaccinations_per_hundred
      }
      
      popup_text <- paste0("<strong>", group_data$name, '</strong>', "<br>",
                           "Cases per Million: ", format(round(group_data$total_cases_per_million,0), big.mark = ','), "<br>",
                           "Deaths per Million: ", format(round(group_data$total_deaths_per_million,0), big.mark = ','), "<br>",
                           "Vaccine Doses per Hundred: ", format(round(group_data$total_vaccinations_per_hundred,0), big.mark = ','))
    }

    group_data %>%
      leaflet() %>%
      addPolygons(data = group_data$geometry,
                  popup= popup_text,
                  color='black',
                  fillColor= fillColor,
                  fillOpacity = 0.6,
                  weight=1.5,
                  smoothFactor = 0.2) %>%
      addLegend(pal = pal,
                values = values,
                title= title) %>%
      setView(0,0,zoom=2)
  })
  
  # REGIONAL GRAPHS
  
  # North America
  output$NRA <- renderPlotly({
    plot_data <- subset(data, continent == 'North America')
    plot_data <- plot_data %>%
      group_by(date=floor_date(date, 'month')) %>%
      summarize(new_cases=sum(new_cases))
    
    plot_data %>%
      plot_ly(x=~ date, y=~new_cases, type='bar', color=I(alpha('#ff3b97', 0.5))) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(font=list(color='black')) %>%
      layout(xaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>% 
      layout(yaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>%
      layout(showlegend=F)
  })
  
  # South America
  output$SA <- renderPlotly({
    plot_data <- subset(data, continent == 'South America')
    plot_data <- plot_data %>%
      group_by(date=floor_date(date, 'month')) %>%
      summarize(new_cases=sum(new_cases))
    
    plot_data %>%
      plot_ly(x=~ date, y=~new_cases, type='bar', color=I(alpha('#ffff00', 0.5))) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(font=list(color='black')) %>%
      layout(xaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>% 
      layout(yaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>%
      layout(showlegend=F)
  })
  
  # Europe
  output$EU <- renderPlotly({
    plot_data <- subset(data, continent == 'Europe')
    plot_data <- plot_data %>%
      group_by(date=floor_date(date, 'month')) %>%
      summarize(new_cases=sum(new_cases))
    
    plot_data %>%
      plot_ly(x=~ date, y=~new_cases, type='bar', color=I(alpha('#00ff96', 0.5))) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(font=list(color='black')) %>%
      layout(xaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>% 
      layout(yaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>%
      layout(showlegend=F)
  })
  
  # Asia
  output$AS <- renderPlotly({
    plot_data <- subset(data, continent == 'Asia')
    plot_data <- plot_data %>%
      group_by(date=floor_date(date, 'month')) %>%
      summarize(new_cases=sum(new_cases))
    
    plot_data %>%
      plot_ly(x=~ date, y=~new_cases, type='bar', color=I(alpha('#00e1ff', 0.5))) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(font=list(color='black')) %>%
      layout(xaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>% 
      layout(yaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>%
      layout(showlegend=F)
  })
  
  # Africa
  output$AF <- renderPlotly({
    plot_data <- subset(data, continent == 'Africa')
    plot_data <- plot_data %>%
      group_by(date=floor_date(date, 'month')) %>%
      summarize(new_cases=sum(new_cases))
    
    plot_data %>%
      plot_ly(x=~ date, y=~new_cases, type='bar', color=I(alpha('#b069ff', 0.5))) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(font=list(color='black')) %>%
      layout(xaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>% 
      layout(yaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>%
      layout(showlegend=F)
  })
  
  # Oceania
  output$OC <- renderPlotly({
    plot_data <- subset(data, continent == 'Oceania')
    plot_data <- plot_data %>%
      group_by(date=floor_date(date, 'month')) %>%
      summarize(new_cases=sum(new_cases))
    
    plot_data %>%
      plot_ly(x=~ date, y=~new_cases, type='bar', color=I(alpha('#d73402', 0.5))) %>% 
      layout(plot_bgcolor='transparent') %>% 
      layout(paper_bgcolor='transparent') %>%
      layout(font=list(color='black')) %>%
      layout(xaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>% 
      layout(yaxis=list(title=list(text='', font=list(color='black')), showgrid=F)) %>%
      layout(showlegend=F)
  })
  
  
  # RACE GRAPHS
  
  # Race Cases Graph
  output$race_case <- renderPlotly({
    
    selected_races <- input$race
    plot_data <- subset(case_prop, race %in% selected_races)
    text_data <- subset(case_race, race %in% selected_races)
    
    plot_data %>%
      plot_ly(x=~death_percent, y=~race, type='bar', name='% of Cases',
              text=paste0(round(text_data$death_percent * 100,1), '%'), 
              hoverinfo='text', hovertext=paste0(text_data$race, ' people make up ', text_data$death_percent * 100, '% of all Covid cases.'),
              orientation='h', marker=list(color='FFF9C4', line=list(color='#566573', width=1))) %>%
      add_trace(x=~better_off, name = 'Better Off', text=paste0(round(text_data$better_off * 100,1), '%'),
                hoverinfo='text', hovertext=paste0(text_data$race, ' people are better off regarding Covid cases. They make up ', 
                                                   text_data$population_percent * 100, '% of the population, but only account for ', text_data$death_percent*100, '% of the cases.'),
                marker=list(color='#7DCEA0')) %>%
      add_trace(x=~worse_off, name= 'Worse Off', text=paste0(round(text_data$worse_off * 100,1), '%'),
                hoverinfo='text', hovertext=paste0(text_data$race, ' people are disproportionately affected by Covid cases. They make up ', 
                                                   text_data$population_percent * 100, '% of the population, but account for ', text_data$death_percent*100, '% of the cases.'),
                marker=list(color='#E74C3C')) %>%
      add_trace(x=~population_percent, name='% Of Population', text=paste0(round(text_data$population_percent * 100,1), '%'),
                hoverinfo='text', hovertext=paste0(text_data$race, ' people make up ', text_data$population_percent*100, '% of the population.'),
                marker=list(color='#D6EAF8')) %>%
      layout(xaxis= list(title=''),
             yaxis=list(title=''),
             barmode='stack') %>%
      layout(xaxis=list(tickformat='.0%')) %>%
      layout(plot_bgcolor= 'transparent') %>% 
      layout(paper_bgcolor= 'transparent') %>%
      layout(title='Covid Cases by Race')
  })
  
  # Race Deaths Graph
  output$race_death <- renderPlotly({
    
    selected_races <- input$race
    plot_data <- subset(death_prop, race %in% selected_races)
    text_data <- subset(death_race, race %in% selected_races)
    
    plot_data %>%
      plot_ly(x=~death_percent, y=~race, type='bar', name='% of Deaths',
              text=paste0(round(text_data$death_percent * 100,1), '%'),
              hoverinfo='text', hovertext=paste0(text_data$race, ' people make up ', text_data$death_percent * 100, '% of all Covid deaths.'),
              orientation='h', marker=list(color='FFF9C4', line=list(color='#566573', width=1))) %>%
      add_trace(x=~better_off, name = 'Better Off', text=paste0(round(text_data$better_off * 100,1), '%'),
                hoverinfo='text', hovertext=paste0(text_data$race, ' people are better off regarding Covid deaths. They make up ', 
                                                    text_data$population_percent * 100, '% of the population, but only account for ', text_data$death_percent*100, '% of the deaths.'),
                marker=list(color='#7DCEA0')) %>%
      add_trace(x=~worse_off, name= 'Worse Off', text=paste0(round(text_data$worse_off * 100,1), '%'),
                hoverinfo='text', hovertext=paste0(text_data$race, ' people are disproportionately affected by Covid deaths. They make up ', 
                                                   text_data$population_percent * 100, '% of the population, but account for ', text_data$death_percent*100, '% of the deaths.'),
                marker=list(color='#E74C3C')) %>%
      add_trace(x=~population_percent, name='% Of Population', text=paste0(round(text_data$population_percent * 100,1), '%'),
                hoverinfo='text', hovertext=paste0(text_data$race, ' people make up ', text_data$population_percent*100, '% of the population.'),
                marker=list(color='#D6EAF8')) %>%
      layout(xaxis= list(title=''),
             yaxis=list(title=''),
             barmode='stack') %>%
      layout(xaxis=list(tickformat='.0%')) %>%
      layout(plot_bgcolor= 'transparent') %>% 
      layout(paper_bgcolor= 'transparent') %>%
      layout(title='Covid Deaths by Race')
  })
  # WORLD DATATABLE
  output$world_datatable <- renderDataTable(group_data[,1:9])
  
  
}
    
shinyApp(ui, server)

