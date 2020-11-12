#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(formattable)
library(DT)
library(maps)
library(RColorBrewer)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    ###################
    #### Dashboard ####
    ###################
        
    output$total_confirmed <- renderText({ 
        paste("", comma(total$confirmed,0))
    })
    
    output$total_deaths <- renderText({ 
        paste("", comma(total$deaths,0))
    })
    
    output$total_recovered <- renderText({ 
        paste("", comma(total$recovered,0))
    })
    
    output$countriesBad <- renderDataTable({
        myDT <- DT::datatable(country_performance[,c(1,3)] %>% filter(slope > 0),
                      options = list(scrollY = '300px'),
                      rownames=FALSE)
        
    })
    
    output$countriesGood <- renderDataTable({
        DT::datatable(country_performance[,c(1,3)] %>% filter(slope < 0),
                      options = list(scrollY = '300px'),
                      rownames=FALSE)
    })
    
    rawData <- reactive({
        dataset <- (paste0("data.",input$datasetSelector))
        eval(parse(text=dataset))
    })
    
    output$rawdata <- renderDataTable({
       
        myDT <- DT::datatable(rawData(),
                              options = list(scrollX = TRUE))
        
    })

    output$confirmedPlot <- renderAmCharts({

        # draw the histogram with the specified number of bins
        amTimeSeries(df.total, 'date', c("confirmed", "deaths", "recovered"), linetype = c("line","line","line"),
                     # fillAlphas = 1,
                     scrollbarHeight=20)
    })
    
    output$conf_perday <- renderAmCharts({
        world_conf <- apply(data.confirmed[,5:ncol(data.confirmed)],2,FUN = sum)
        world_deaths <- apply(data.deaths[,5:ncol(data.confirmed)],2,FUN=sum)
        world_recovered <- apply(data.recovered[,5:ncol(data.confirmed)],2,FUN=sum)
        cases_day <- diff(world_conf)
        deaths_day <- diff(world_deaths)
        recovered_day <- diff(world_recovered)
        to_plot <- data.frame("New Cases"=cases_day,"Daily Deaths"=deaths_day,"Newly Recovered"=recovered_day)
        to_plot <- data.frame("Date"=as.POSIXct(substring(rownames(to_plot),2), format = "%m.%d.%y"),"New Cases"=cases_day,"Daily Deaths"=deaths_day,"Newly Recovered"=recovered_day)
        amTimeSeries(to_plot,"Date",c("New.Cases", "Daily.Deaths", "Newly.Recovered"),scrollbarHeight=20, linetype = c("line","line","line"))
    })
    
    library(ggplot2)
    output$myggplot <- renderPlot({
        colors <- c("Confirmed" = "orange", "Deaths" = "red", "Recovered" = "green")

        ggplot(df.total) + aes(x=date) +
            geom_area(aes(y=confirmed), fill="orange", color="orange", alpha=0.3) +
            geom_area(aes(y=deaths), fill="red", color="red", alpha=0.3) +
            geom_area(aes(y=recovered), fill="green", color="green", alpha=0.3) +
            # scale_color_manual(values = colors) +
            theme_bw() +
            theme(axis.line = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  plot.background = element_rect(fill=rgb(248/255,248/255,248/255)),
                  panel.background = element_rect(fill=rgb(248/255,248/255,248/255)),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
    })
    
    output$statusMap <- renderLeaflet({
        data <- merge(world, country_performance_today, by="iso3c")
        
        mybins <- c(-Inf,-10,10,Inf)
        mypalette <- colorBin( palette=c("green", "orange", "red"), domain=data$slope, na.color="transparent", bins=mybins, pretty=T)
        
        leaflet(data) %>% 
            addTiles()  %>% 
            setView( lat=36, lng=28 , .5) %>%
            addPolygons( 
                fillColor = ~mypalette(data$slope), 
                stroke=TRUE, 
                fillOpacity = 0.75, 
                color="white", 
                weight=0.9,
                label = data$country
            )
    })
    
        
    
        
    
    ###################
    #### COVID Map ####
    ###################
    complete <- reactive({
        complete <- merge(world,jh_covid19_data[jh_covid19_data$date==input$day,], by="iso3c")
        
        # Set all data points where we have 0 cases to NA
        complete$confirmed[which(complete$confirmed == 0)] = NA
        complete$deaths[which(complete$deaths == 0)] = NA
        complete$recovered[which(complete$recovered == 0)] = NA
        
        # Transform data to cases / per 1M inhabitants
        complete <- complete %>% mutate(confirmed_adj = confirmed / pop_est * 1000000) %>% 
            mutate(deaths_adj = deaths / pop_est * 1000000) %>% 
            mutate(recovered_adj = recovered / pop_est * 1000000)
    })
    
    output$world_map<-renderLeaflet({

        # Per 1M inhabitants
        if (input$maptype == 1) {
            typeData <- paste0(input$mapdata, "_adj")
            
            if (input$mapdata == "deaths") {
                mybins <- c(0, 0.1, 10, 50, 100, 500,Inf)
                typeText <- "Deaths"
            }
            else {
                mybins <- c(0, 100, 1000, 5000, 10000, 25000, 50000, Inf)    
                if (input$mapdata == "confirmed") {
                    typeText <- "Cases"
                } else {
                    typeText <- "Recovered"
                }
            }
            
            mypalette <- colorBin( palette="YlOrBr", domain=complete()[[typeData]], na.color="transparent", bins=mybins, pretty=F)
            
            # Prepare the text for tooltips:
            mytext <- paste(
                "Country: ", complete()$name,"<br/>", 
                "Cases per 1M: ", comma(complete()$confirmed_adj, 0), "<br/>",
                "Deaths per 1M: ", comma(complete()$deaths_adj, 0),
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(complete()) %>% 
                addTiles()  %>% 
                setView( lat=10, lng=0 , zoom=2.5) %>%
                addPolygons( 
                    fillColor = ~mypalette(complete()[[typeData]]), 
                    stroke=TRUE, 
                    fillOpacity = 0.7, 
                    color="white", 
                    weight=0.9,
                    label = mytext,
                    labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "13px", 
                        direction = "auto"
                    )
                ) %>%
                addLegend( pal=mypalette, values=~complete()[[typeData]], opacity=0.9, title = paste(typeText, "(Per 1M)"), position = "bottomleft" )
            
        }
        # Total number of cases
        else{
            if (input$mapdata == "deaths") {
                mybins <- c(0, 1,  50, 500, 10000, 50000,Inf) 
                typeText <- "Deaths"
            }
            else {
                mybins <- c(0,50000,100000,1000000,Inf)    
                if (input$mapdata == "confirmed") {
                    typeText <- "Cases"
                } else {
                    typeText <- "Recovered"
                }
            }
            mypalette <- colorBin( palette="YlOrBr", domain=complete()[[input$mapdata]], na.color="transparent", bins=mybins, pretty=F)
            
            # Prepare the text for tooltips:    
            mytext <- paste(
                "Country: ", complete()$name,"<br/>", 
                "Cases: ", comma(complete()$confirmed, 0), "<br/>",
                "Deaths: ", comma(complete()$deaths, 0),
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(complete()) %>% 
                addTiles()  %>% 
                setView( lat=10, lng=0 , zoom=2.5) %>%
                addPolygons( 
                    fillColor = ~mypalette(complete()[[input$mapdata]]), 
                    stroke=TRUE, 
                    fillOpacity = 0.7, 
                    color="white", 
                    weight=0.9,
                    label = mytext,
                    labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "13px", 
                        direction = "auto"
                    )
                ) %>%
                addLegend( pal=mypalette, values=~complete()[[input$mapdata]], opacity=0.9, title = typeText, position = "bottomleft" )
        }
    })

    complete2 <- reactive({
        complete2 <- merge(world,jh_covid19_data_daily[jh_covid19_data_daily$date==input$day2,], by="iso3c")
        
        # Transform data to cases / per 1M inhabitants
        complete2 <- complete2 %>% mutate(confirmed_adj = confirmed_diff / pop_est * 1000000) %>% 
            mutate(deaths_adj = deaths_diff / pop_est * 1000000) %>% 
            mutate(recovered_adj = recovered_diff / pop_est * 1000000)
        
        
    })
    
    output$world_map2 <- renderLeaflet({
        # Per 1M inhabitants
        if (input$maptype2 == 1) {
            typeData <- paste0(input$mapdata2, "_adj")
            
            if (input$mapdata2 == "deaths") {
                mybins <- c(0, 0.1, 1, 5, 10, 25, 50, Inf)
                typeText <- "Deaths"
            }
            else {
                mybins <- c(0, 1, 10, 50, 100, 500, 1000, Inf)    
                if (input$mapdata2 == "confirmed") {
                    typeText <- "Cases"
                } else {
                    typeText <- "Recovered"
                }
            }
            
            mypalette <- colorBin( palette="YlOrBr", domain=complete2()[[typeData]], na.color="transparent", bins=mybins, pretty=F)
            
            # Prepare the text for tooltips:
            mytext <- paste(
                "Country: ", complete2()$name,"<br/>", 
                "New cases: ", comma(complete2()$confirmed_adj, 0), "<br/>",
                "New deaths: ", comma(complete2()$deaths_adj, 0),
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(complete2()) %>% 
                addTiles()  %>% 
                setView( lat=10, lng=0 , zoom=2.5) %>%
                addPolygons( 
                    fillColor = ~mypalette(complete2()[[typeData]]), 
                    stroke=TRUE, 
                    fillOpacity = 0.7, 
                    color="white", 
                    weight=0.9,
                    label = mytext,
                    labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "13px", 
                        direction = "auto"
                    )
                ) %>%
                addLegend( pal=mypalette, values=~complete2()[[typeData]], opacity=0.9, title = paste(typeText, "(Per 1M)"), position = "bottomleft" )
            
        }
        # Total number of cases
        else{
            typeData <- paste0(input$mapdata2, "_diff")
            
            if (input$mapdata == "deaths") {
                mybins <- c(0, 1,  10, 100, 500, 1000,Inf) 
                typeText <- "Deaths"
            }
            else {
                mybins <- c(0,1,10,100,1000,5000,Inf)    
                if (input$mapdata == "confirmed") {
                    typeText <- "Cases"
                } else {
                    typeText <- "Recovered"
                }
            }
            mypalette <- colorBin( palette="YlOrBr", domain=complete2()[[typeData]], na.color="transparent", bins=mybins, pretty=F)
            
            # Prepare the text for tooltips:
            mytext <- paste(
                "Country: ", complete2()$name,"<br/>", 
                "New cases: ", comma(complete2()$confirmed_diff, 0), "<br/>",
                "New deaths: ", comma(complete2()$deaths_diff, 0),
                sep="") %>%
                lapply(htmltools::HTML)
            
            leaflet(complete2()) %>% 
                addTiles()  %>% 
                setView( lat=10, lng=0 , zoom=2.5) %>%
                addPolygons( 
                    fillColor = ~mypalette(complete2()[[typeData]]), 
                    stroke=TRUE, 
                    fillOpacity = 0.7, 
                    color="white", 
                    weight=0.9,
                    label = mytext,
                    labelOptions = labelOptions( 
                        style = list("font-weight" = "normal", padding = "3px 8px"), 
                        textsize = "13px", 
                        direction = "auto"
                    )
                ) %>%
                addLegend( pal=mypalette, values=~complete2()[[typeData]], opacity=0.9, title = typeText, position = "bottomleft" )
        }
    })
    
    #####################
    #### Per Country ####
    #####################
    
    SelectedCountry <- reactive({
        print(countryData()$iso3c[1])
        SelectedCountry <- maps::map("world", fill = TRUE, plot = FALSE, regions=countryData()$country[1], exact=TRUE)
    })
    
    output$countryMap <- renderLeaflet({
        leaflet(SelectedCountry()) %>%
            fitBounds(SelectedCountry()$range[1], SelectedCountry()$range[3], 
                      SelectedCountry()$range[2], SelectedCountry()$range[4]) %>%
            addPolygons(fillOpacity = 0.6,  smoothFactor = 0.25, stroke = TRUE, weight = 1) %>%
            addProviderTiles("OpenStreetMap",
                             options = providerTileOptions(noWrap = FALSE)
            )
    })
    
    output$countryBoxConfirmed <- renderInfoBox({
        infoBox(
            "total Cases", paste("", comma(countryData()[nrow(countryData()), "confirmed"],0)), icon = icon("viruses"),
            color = "orange", fill = TRUE
        )
    })
    
    output$countryBoxDeaths <- renderInfoBox({
        infoBox(
            "total Deaths", paste("", comma(countryData()[nrow(countryData()), "deaths"],0)), icon = icon("feather"),
            color = "red", fill = TRUE
        )
    })
    
    output$countryBoxRecovered <- renderInfoBox({
        infoBox(
            title="Total Recovered", paste("", comma(countryData()[nrow(countryData()), "recovered"],0)), icon = icon("heart"),
            color = "green", fill = TRUE
        )
    })
    
    
    countryData <- reactive({
        countryData <- jh_covid19_data[which(jh_covid19_data$country == input$country),]
        countryData$date <- as.POSIXct(countryData$date)
        print(countryData)
    })
    
    output$countryPlot <- renderAmCharts({
        
        # draw the histogram with the specified number of bins
        amTimeSeries(countryData(), 'date', c("confirmed", "deaths", "recovered"),
                     scrollbarHeight=20, main=input$country)
        
    })
    
    ##################
    #### Analysis ####
    ##################
    
    model <- reactive({
        world_conf<-apply(data.confirmed[,5:ncol(data.confirmed)],2,FUN = sum)
        world_deaths<-apply(data.deaths[,5:ncol(data.confirmed)],2,FUN=sum)
        world_recovered<-apply(data.recovered[,5:ncol(data.confirmed)],2,FUN=sum)
        cases_day<-diff(world_conf)
        deaths_day<-diff(world_deaths)
        recovered_day<-diff(world_recovered)
        data_for_model<-cbind(cases_day[8:length(cases_day)],
                              cases_day[7:length((cases_day-1))],
                              cases_day[6:length((cases_day-2))],
                              cases_day[5:length((cases_day-3))],
                              cases_day[4:length((cases_day-4))],
                              cases_day[3:length((cases_day-5))],
                              cases_day[2:length((cases_day-6))],
                              cases_day[1:length((cases_day-7))])
        data_for_model<-data.frame(data_for_model)
        y<-data_for_model[,1]
        x<-data_for_model[,(as.numeric(input$id_check_model) + 1)]
        data<-data.frame(y,x)
        model<-lm(y~.,data=data)
        model
    })
    
    output$regression<-renderPrint({
        summary(model())
    })
    
    output$predicted_cases<-renderText({
        world_conf<-apply(data.confirmed[,5:ncol(data.confirmed)],2,FUN = sum)
        cases_day<-diff(world_conf)
        paste(comma(model()$coefficients[1]+sum(model()$coefficients[-1]*cases_day[(length(cases_day)-length(as.numeric(input$id_check_model))+1):length(cases_day)]), 0), "new cases")
    })
    
    output$pred_chart<-renderAmCharts({
        world_conf <-apply(data.confirmed[,5:ncol(data.confirmed)],2,FUN = sum)
        cases_day<-diff(world_conf)
        predictions<-vector()
        
        for (i in (length(as.numeric(input$id_check_model))+1):length(cases_day)){
            predictions[i] <- model()$coefficients[1] + sum(model()$coefficients[-1] * cases_day[(i-length(as.numeric(input$id_check_model))):(i-1)])
        }
        
        # Confidence interval
        conf <- qnorm(1-0.025) * sd(predictions[8:length(predictions)]) / sqrt(length(predictions))
        upper<-predictions+conf
        lower<-predictions-conf
        
        # Plot
        to_plot <- data.frame("New Cases"=cases_day,"Predictions"=predictions)
        to_plot <- data.frame("Date"=as.POSIXct(substring(rownames(to_plot),2), format = "%m.%d.%y"),"New Cases"=cases_day,"Predicted"=predictions,"Upper"=upper,"Lower"=lower)
        print(to_plot)
        amTimeSeries(to_plot, "Date", list("New.Cases", c("Lower", "Predicted", "Upper")),
                     # linetype=c(1,1),
                     main="(Predicted) Cases",
                     color=c("blue","red"),
                     scrollbarHeight=20)
    })
    
    output$message<-renderMenu({
        world_conf <- apply(data.confirmed[,5:ncol(data.confirmed)],2,FUN = sum)
        world_deaths <- apply(data.deaths[,5:ncol(data.confirmed)],2,FUN=sum)
        world_recovered <- apply(data.recovered[,5:ncol(data.confirmed)],2,FUN=sum)
        cases_day <- diff(world_conf)
        deaths_day <- diff(world_deaths)
        recovered_day <- diff(world_recovered)
        dropdownMenu(type = "notifications", badgeStatus = "warning",headerText = "The situation today: ",
                     notificationItem(icon = icon("viruses"), status = "info",
                                      paste("Cases: ", comma(cases_day[length(cases_day)],0))
                     ),
                     notificationItem(icon = icon("feather"), status = "danger",
                                      paste("Deaths: ", comma(deaths_day[length(deaths_day)],0))
                     ),
                     notificationItem(icon = icon("heart"), status = "info",
                                      paste("Recoveries: ",comma(recovered_day[length(recovered_day)],0))
                     ))
    })
    
    output$message2<-renderMenu({
        dropdownMenu(type = "messages", icon = icon("address-card"), badgeStatus = "primary",headerText = "Authors: ",
                     notificationItem(icon = icon("users"), status = "info",
                                      "CHIRITA Andrei"),
                     notificationItem(icon = icon("users"), status = "info",
                                      "DARMOUTOMO Michael"
                     ))
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$datasetSelector, "-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(rawData(), file, row.names = FALSE)
        }
    )
    
})
