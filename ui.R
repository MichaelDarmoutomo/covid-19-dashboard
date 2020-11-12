library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(rAmCharts)
library(leaflet)

# Define dashboardPage
dashboardPage(
    
    # Header
    dashboardHeader(title=shinyDashboardLogoDIY(
        boldText = "COVID-19",
        mainText = "Dashboard",
        textSize = 16,
        badgeText = "",
        badgeTextColor = "white",
        badgeTextSize = 2,
        badgeBackColor = "rgb(24,188,156)",
        badgeBorderRadius = 3
    ),
                    
        menuItemOutput("message"),
        menuItemOutput("message2")
        
    ), # End of header
    
    # Sidebar
    dashboardSidebar(
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem("Interactive World Map", icon = icon("map"),
                     menuSubItem("Cumulative Data", tabName = "worldmap"),
                     menuSubItem("Daily Data", tabName = "worldmap2")
            ),
            menuItem("Global Statistics", tabName = "dashboard", icon = icon("globe-africa")),
            menuItem("COVID-19 Per Country", icon = icon("flag"), tabName = "country"),
            menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis", badgeLabel = "new",
                     badgeColor = "green"),
            menuItem("Raw Data", icon = icon("database"), tabName="rawdata")
        )
    ), # End of sidebar
    
    # Body
    dashboardBody(

        # Theme
        shinyDashboardThemes(
            theme = "poor_mans_flatly"
        ),
        
        # Manually fix color of title
        tags$head(
            tags$style(HTML(" 
            .box.box-solid.box-primary>.box-header h3, .box.box-primary>.box-header h3 {
                color: rgb(33,37,41);
            }
            
            text {
                font-family: Arial;
            }
        "))
        ),

        tabItems(
            tabItem("dashboard",
            fluidRow(
                column(width = 6,
                    box(
                        title = "Global Statistics", width = NULL, status = "primary", solidHeader = FALSE,
                        collapsible = FALSE, style="color: 'red'",
                        fluidRow(
                            column(width = 1, offset=2,
                                   fluidRow(
                                       textOutput("total_confirmed"),
                                       tags$style(type="text/css", "#total_confirmed {font-size:20px;font-weight: bold;}")
                                       ),
                                   fluidRow(
                                       p("Cases")
                                   )
                            ),
                            column(width = 2, offset=2,
                                   fluidRow(
                                       textOutput("total_deaths"),
                                       tags$style(type="text/css", "#total_deaths {font-size:20px;font-weight: bold;}")
                                   ),
                                   fluidRow(
                                       p("Deaths")
                                   )
                            ),
                            column(width = 2, offset=1,
                                   fluidRow(
                                       textOutput("total_recovered"),
                                       tags$style(type="text/css", "#total_recovered {font-size:20px;font-weight: bold;}")
                                   ),
                                   fluidRow(
                                       p("Recovered")
                                   )
                            )
                        ),
                        fluidRow(align="center",
                                 column(12,
                                    amChartsOutput("confirmedPlot", width = "95%", height="300px")
                                 )
                        )
                    ),
                    box(
                        title = "Statistics per day", width = NULL, status = "primary", solidHeader = FALSE,
                        collapsible = FALSE,
                        fluidRow(align="center",
                                 column(12,
                                        amChartsOutput("conf_perday", width = "95%", height="300px")
                                 )
                        )
                    )
                ),
                 
                column(width = 6,
                   fluidRow(
                       box(title = "Increasing/Decreasing cases per day (based on week)", width = NULL,
                           status = "primary", solidHeader = FALSE,
                           collapsible = FALSE,
                           fluidRow(
                               column(width = 12,
                                      leafletOutput("statusMap")
                               )
                           )
                       )
                   )
                )
            )
            ),
            tabItem("worldmap",
                    tags$style(type = "text/css", "#world_map {margin: -15px}"), 
                    tags$style(type = "text/css", "#world_map {height: calc(100vh - 50px) !important;}"),
                    fluidRow(
                        column(12,
                               leafletOutput("world_map",width = "auto")#, height="980px")
                        )
                    ),
                    absolutePanel(top = 147, left = 255,draggable = TRUE,cursor = "move", width=253,
                                  box(id="OptionsBox",title="Options", width=NULL, status = "primary", solidHeader = FALSE,
                        collapsible = TRUE,
                                  # h2("Options"), # title
                                  sliderInput("day", "Choose the day", value = jh_covid19_data$date[nrow(jh_covid19_data)], min = min(jh_covid19_data$date), max = max(jh_covid19_data$date)),
                                  radioButtons(inputId = "mapdata", label = "Choose the data",
                                               choices = c("Number of confirmed cases" = "confirmed", "Number of deaths" = "deaths", "Number of recovered" = "recovered"),
                                               selected = "confirmed"),
                                  selectInput(inputId = "maptype", label = "Select if you want a per 1M map or a map based on the total number of cases ", selected = 3,
                                              choices = c("Total per 1 million population" = 1, "Total number" = 2), multiple = FALSE)
                    ))
            ),
            tabItem("worldmap2",
                    tags$style(type = "text/css", "#world_map2 {margin: -15px}"), 
                    tags$style(type = "text/css", "#world_map2 {height: calc(100vh - 50px) !important;}"),
                    fluidRow(
                        column(12,
                               leafletOutput("world_map2",width = "auto")#, height="980px")
                        )
                    ),
                    absolutePanel(top = 147, left = 255,draggable = TRUE,cursor = "move", width=253,
                                  box(id="OptionsBox",title="Options", width=NULL, status = "primary", solidHeader = FALSE,
                                      collapsible = TRUE,
                                  # h2("Options"), # title
                                  sliderInput("day2", "Choose the day", value = jh_covid19_data$date[nrow(jh_covid19_data)], min = (min(jh_covid19_data$date)+1), max = max(jh_covid19_data$date)),
                                  radioButtons(inputId = "mapdata2", label = "Choose the data",
                                               choices = c("Number of confirmed cases" = "confirmed", "Number of deaths" = "deaths", "Number of recovered" = "recovered"),
                                               selected = "confirmed"),
                                  selectInput(inputId = "maptype2", label = "Select if you want a per 1M map or a map based on the total number of cases ", selected = 3,
                                              choices = c("Total per 1 million population" = 1, "Total number" = 2), multiple = FALSE)
                    ))
            ),
            tabItem("country",
                fluidRow(
                    column(width = 3,
                           box(width=NULL,
                               fluidRow(
                                   column(width= 12,
                                          pickerInput("country", "Choose a country:", countries, selected="France",
                                                      options = list(`live-search` = TRUE))
                                   )
                               )
                           ),
                           fluidRow(
                               column(width = 12,
                                      infoBoxOutput("countryBoxConfirmed", width=NULL)
                               )
                            ),
                           fluidRow(
                               column(width = 12,
                                      infoBoxOutput("countryBoxDeaths", width=NULL)
                               )
                           ),
                           fluidRow(
                               column(width = 12,
                                      infoBoxOutput("countryBoxRecovered", width=NULL)
                               )
                           )
                           
                     
                    ),
                    column(width = 9,
                           box(width = NULL,
                               status = "primary", solidHeader = FALSE,
                               leafletOutput("countryMap"),
                               p(""),
                               amChartsOutput("countryPlot")
                            )
                           )
                )
            ),
            tabItem("analysis",
                fluidRow(
                    column(width=12, amChartsOutput("pred_chart"))
                ),
                fluidRow(
                    column(width=12,
                           # checkboxGroupInput(inputId = "id_check_model", label = "Please select which variables should be used in the predictive model", selected = 7,
                           #                    choices = c("The day before" = 1, "2 days before" = 2, "3 days before" = 3, "4 days before" = 4, "5 days before" = 5, "6 days before" = 6, "7 days before" = 7))
                           checkboxGroupButtons(
                               inputId = "id_check_model",
                               label = "Please select which variables should be used in the predictive model",
                               choices = c("1 day before" = 1, 
                                           "2 days before" = 2, 
                                           "3 days before" = 3, 
                                           "4 days before" = 4,                                          
                                           "5 days before" = 5,  
                                           "6 days before" = 6, 
                                           "7 days before" = 7
                                           ),
                               selected = 1,
                               checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square", 
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-square-o", 
                                               style = "color: steelblue"))
                           )
                    )
                ),
                fluidRow(
                    column(width=6,offset=0,verbatimTextOutput("regression")),
                    column(width=6,offset=0,
                           fluidRow(
                               p("The model you fitted predicts that tomorrow there will be:")
                           ),
                           fluidRow(
                               textOutput("predicted_cases"),
                               tags$style(type="text/css", "#predicted_cases {font-size:20px;font-weight: bold;}")
                           )
                    )
                )
            ),
            
            tabItem("rawdata",
                    h1("Dataset"),
                    radioGroupButtons(
                        inputId = "datasetSelector",
                        label = "Select dataset",
                        choices = c("Confirmed Cases" = "confirmed", "Deaths" = "deaths", "Recovered" = "recovered"),
                        justified = TRUE
                    ),
                    dataTableOutput("rawdata"),
                    downloadButton('downloadData', 'Download'),
                    fluidRow(
                        column(12, 
                               p(""),
                               a(href="https://github.com/CSSEGISandData/COVID-19", "Click here to view the data online.")
                        )
                    ))
        )
        
        
    ) # End of body
    
)

