# MAP

library(leaflet)
data <- merge(jh_covid19_data, world[,c(10,35)])
data$confirmed[which(data$confirmed == 0)] = NA
data$deaths[which(data$deaths == 0)] = NA
data$recovered[which(data$recovered == 0)] = NA


data <- data %>% mutate(confirmed_adj = confirmed / pop_est * 1000000) %>% 
  mutate(deaths_adj = deaths / pop_est * 1000000) %>% 
  mutate(recovered_adj = recovered / pop_est * 1000000)

data.today <- data[data$date == "2020-11-06",]

# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="data/world_shape_file.zip")
# system("unzip data/world_shape_file.zip")

library(rgdal)
world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/data/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

world_spdf@data <- world_spdf@data %>% rename(iso3c = ISO3)
world_spdf <- merge(world_spdf, data.today, by="iso3c")

# # Clean the data object
# library(dplyr)
# world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
# world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000
# 
# # Create a color palette for the map:
# mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
# mypalette(c(45,43))

library(RColorBrewer)
mybins <- c(0,1000,5000,10000,25000,50000,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$confirmed_adj, na.color="transparent", bins=mybins, pretty=F)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  # "Area: ", world_spdf@data$AREA, "<br/>", 
  "Cases: ", round(world_spdf@data$confirmed_adj, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
m <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(confirmed_adj), 
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
  addLegend( pal=mypalette, values=~confirmed_adj, opacity=0.9, title = "Cases (Per 1M)", position = "bottomleft" )

m  

