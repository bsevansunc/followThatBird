#========================================================================================*
# ---- SET-UP ----
#========================================================================================*

library(shiny)
library(DT)
library(leaflet)
library(sp)
library(raster)
library(colorRamps)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(leaflet)
library(sp)
library(rgdal)

select <- dplyr::select
filter <- dplyr::filter

options(stringsAsFactors = FALSE)

options(shiny.fullstacktrace = TRUE)

#----------------------------------------------------------------------------------------*
# ---- Get data ----
#----------------------------------------------------------------------------------------*

# Tag technology table:

tagTable <- read.csv('data/tagTable.csv') %>% 
  mutate(Definition = str_replace_all(Definition, '\\<U+00A0>', ''))

# Tracking data:

tracks <- read_csv('data/trackingData.csv')

# Get biome layer:

biomesR <- raster('data/biomesR')

biomeLevels <- read_csv('data/biomeLevels.csv')

# Get agriculture layer:

agriculture <- raster('data/agIntensity')

# Make bird ID table:

birdIdTable <- tracks %>%
  select(species, birdID) %>%
  mutate(birdID = as.character(birdID)) %>%
  distinct

#----------------------------------------------------------------------------------------*
# ---- Leaflet setup ----
#----------------------------------------------------------------------------------------*

# Color set for biomes:

bioColors <- c("#9E0142", "#D9444E", "#F77E4A", "#FEC171", "#F3EB91", "#C2E6A0",
               "#79C9A5", "#3D8FBA", "#3D64BA", "#5E4FA2")

# Biome colors:

factPal <- colorFactor(bioColors, domain = 1:10, na.color = 'transparent')

# Agricultural intensity colors:

pal <- colorNumeric(colorRamps::matlab.like(10), c(0, values(agriculture)),
                    na.color = "transparent")

# Set colors for birds:

colorPalette <- colorFactor(
  colorRamps::primary.colors(length(unique(tracks$birdID))),
  tracks$birdID
)

# Lines:

makeLines <- function(tracks){
  birdIDs <- unique(tracks$birdID)
  tracksList <- vector('list', length = length(birdIDs))
  
  names(tracksList) <- birdIDs
  
  for(i in 1:length(tracksList)){
    tracksList[[i]] <- tracks %>%
      filter(birdID == birdIDs[i])
  }
  
  birdLines <- vector('list', length = length(birdIDs))
  
  for(i in 1:length(tracksList)){
    coordMat <- tracksList[[i]] %>%
      dplyr::select(longitude, latitude) %>%
      as.matrix
    birdLines[[i]] <- Lines(Line(coordMat),
                            ID = tracksList[[i]] %>%
                              .$birdID %>%
                              unique)
  }
  
  lines <- SpatialLinesDataFrame(SpatialLines(birdLines),
                                 data = tracks %>%
                                   dplyr::select(birdID) %>%
                                   distinct,
                                 match.ID = FALSE
  )
  return(lines)
}

# Basemap:

basemap <- leaflet(width = "50%", height="100%") %>%
  setView(-100, 23, 2) %>%
  addTiles(group = 'Street map') %>%
  addLayersControl(
    baseGroups = c('Street map','National Geographic','Aerial photo',
                   'Biomes', 'Agriculture'),
    options = layersControlOptions(collapsed = FALSE)
  )

#========================================================================================*
# ---- SERVER ----
#========================================================================================*

server <- function(input, output, session) {
  
  # Output table for tag technology:
  
  output$tagTable <- DT::renderDataTable({
    DT::datatable(tagTable, 
                  escape = FALSE, 
                  options = list(dom = 't'),
                  selection  = 'none')
  })
  
  #-------------------------------------------------------------------------------*
  # ---- TRACKING TABLE UI ELEMENTS ----
  #-------------------------------------------------------------------------------*
  
  # Choices of bird IDs for ui:
  
  output$uiBirdIDTable = renderUI({
    selectInput(
      inputId = 'birdIDTable',
      label = 'Identity of bird:',
      choices = c('Show all',
                  birdIdTable %>%
                    filter(species == input$sppTable) %>%
                    .$birdID),
      selectize = FALSE,
      selected = 'Show all'
    )
  })
  
  # Date range bounds are set by the data (passes to the ui):
  
  output$uiDateRangeTable <- renderUI({
    tracksOut <- tracks
    if(!is.null(input$sppTable)){
      if(input$sppTable != 'Show all'){
        tracksOut <- tracksOut %>%
          filter(species == input$sppTable)
      }
    }
    dateRange <- c(min(tracksOut$date), max(tracksOut$date))
    dateRangeInput('queryDatesTable','Date range:',
                   start = dateRange[1],
                   end = dateRange[2],
                   min = dateRange[1],
                   max = Sys.Date()
    )
  })
  
  # Picture for sidebar:
  
  output$birdPicTable <- renderText({
    HTML(
      paste0(
        '<img align="left" width="90%" hspace="10" vspace="10" ',
        paste0('alt= Photograph of a ', input$sppTable),
        paste0('title=', input$sppTable),
        switch(
          input$sppTable,
          'Pacific loon' = ' src="pacificLoon.jpg">',
          'Black-crowned night heron' = ' src="blackCrownNightHeron.jpg">',
          'Brown pelican' = ' src="brownPelican.jpg">',
          'Black-bellied plover' = ' src="blackBelliedPlover.jpg">',
          'Long-billed curlew' = ' src="lbCurlew.jpg">',
          "Swainson's hawk" = ' src="swainsons.jpg">'
        )
      )
    )
  })
  
  #-------------------------------------------------------------------------------*
  # ---- TRACKING TABLE ----
  #-------------------------------------------------------------------------------*
  
  # Initial data table:
  
  output$tableQuery <- DT::renderDataTable(rownames = FALSE, tracks)
  
  # Update data table output from query:
  
  observe({
    tracksOut <- tracks
    if(!is.null(input$sppTable)){
      if(input$sppTable != 'Show all'){
        tracksOut <- tracksOut %>%
          filter(species == input$sppTable)
      }
    }
    if(!is.null(input$queryDatesTable)){
      tracksOut <- tracksOut %>%
        filter(
          date >= input$queryDatesTable[1],
          date <= input$queryDatesTable[2]
        )
    }
    if(!is.null(input$birdIDTable)){
      if(input$birdIDTable != 'Show all'){
        tracksOut <- tracksOut %>%
          filter(birdID == input$birdIDTable)
      }
    }
    replaceData(dataTableProxy('tableQuery'), 
                tracksOut, rownames = FALSE)#trackingTableR(), rownames = FALSE)
  })
  
  #-------------------------------------------------------------------------------*
  # ---- TRACKING MAP UI ELEMENTS ----
  #-------------------------------------------------------------------------------*
  
  # Choices of bird IDs for ui:
  
  output$uiBirdIDMap = renderUI({
    selectInput(
      inputId = 'birdIDMap',
      label = 'Identity of bird:',
      choices = c('Show all',
                  tracks %>%
                    filter(species == input$sppMap) %>%
                    select(birdID) %>%
                    distinct %>%
                    .$birdID),
      selected = 'Show all',
      selectize = FALSE
    )
  })
  
  # Date range bounds are set by the data (passes to the ui):
  
  output$uiDateRangeMap <- renderUI({
    tracksOut <- tracks
    if(!is.null(input$sppMap)){
      if(input$sppMap != 'Show all'){
        tracksOut <- tracksOut %>%
          filter(species == input$sppMap)
      }
    }
    dateRange <- c(min(tracksOut$date), max(tracksOut$date))
    dateRangeInput('queryDatesMap','Date range:',
                   start = dateRange[1],
                   end = dateRange[2],
                   min = dateRange[1],
                   max = Sys.Date()
    )
  })
  
  # Picture for sidebar:
  
  output$birdPicMap <- renderText({
    HTML(
      paste0(
        '<img align="left" width="90%" hspace="10" vspace="10" ',
        paste0('alt= Photograph of a ', input$sppMap),
        paste0('title=', input$sppMap),
        switch(
          input$sppMap,
          'Pacific loon' = ' src="pacificLoon.jpg">',
          'Black-crowned night heron' = ' src="blackCrownNightHeron.jpg">',
          'Brown pelican' = ' src="brownPelican.jpg">',
          'Black-bellied plover' = ' src="blackBelliedPlover.jpg">',
          'Long-billed curlew' = ' src="lbCurlew.jpg">',
          "Swainson's hawk" = ' src="swainsons.jpg">'
        )
      )
    )
  })
  
  #--------------------------------------------------------------------------------------*
  # --- TRACKING MAP ----
  #--------------------------------------------------------------------------------------*
  
  # Base map:
  
  output$mapOut <- renderLeaflet(basemap)
  
  # Update map:
  
  observeEvent(input$mapIt, {
    
    # Point values:
    
    points <- tracks
    if(!is.null(input$sppMap)){
      if(input$sppMap != 'Show all'){
        points <- points %>%
          filter(species == input$sppMap)
      }
    }
    if(!is.null(input$birdIDMap)){
      if(input$birdIDMap != 'Show all'){
        points <- points %>%
          filter(birdID == input$birdIDMap)
      }
    }
    if(!is.null(input$queryDatesMap)){
      points <- points %>%
        filter(
          date >= input$queryDatesMap[1],
          date <= input$queryDatesMap[2]
        )
    }
    
    # Line values:
    
    lineValues <- makeLines(points)
    
    # Update map:
    
    leafletProxy('mapOut', data = points) %>%
      clearShapes() %>%
      clearMarkers() %>%
      fitBounds(
        min(points$longitude) - 1,
        min(points$latitude) - 1, 
        max(points$longitude) + 1,
        max(points$latitude) + 1
        ) %>%
      addPolylines(
        data = lineValues, 
        weight = 4,
        opacity = .3,
        color = 'white'
      ) %>%
      addPolylines(
        data = lineValues, 
        weight = 2, dashArray = "5, 5",
        opacity = 1,
        color = ~colorPalette(birdID)
      ) %>%
      addCircleMarkers(data = points,
                       ~longitude, ~latitude,
                       radius = ~4,
                       color = 'white',
                       fillColor = ~colorPalette(birdID),
                       stroke = TRUE,
                       popup = ~paste(
                         sep = "<br/>",
                         paste0('<b style="color:#0000FF">', birdID, '</b>'),
                         paste0("<b> Species: </b>", species),
                         paste0("<b> Date: </b>", date),
                         paste0("<b> Longitude: </b>", round(longitude, 4)),
                         paste0("<b> Latitude: </b>", round(latitude, 4))),
                       fillOpacity = 0.9
      )
  })
  
  # Add legends:
  
  observeEvent(input$mapOut_groups, {
    groupValue <- unique(input$mapOut_groups)[1]
    mapOut <- leafletProxy('mapOut') %>% 
      clearControls()
    if(groupValue == 'National Geographic'){
      mapOut <- mapOut %>%
        addProviderTiles('Esri.NatGeoWorldMap', group = 'National Geographic')
    }
    if(groupValue == 'Aerial photo'){
      mapOut <- mapOut %>%
        addProviderTiles('Esri.WorldImagery', group = 'Aerial photo')
    }
    if(groupValue == 'Biomes'){
      mapOut <- mapOut %>%
        addTiles() %>%
        addRasterImage(biomesR, colors = factPal, 
                       opacity = 1, project = FALSE,
                       group = 'Biomes') %>%
        addLegend('bottomleft', colors = bioColors,
                  labels = biomeLevels$biome,
                  opacity = 1)
    }
    if(groupValue == 'Agriculture'){
      mapOut <- mapOut %>%
        addTiles() %>%
        addRasterImage(agriculture, colors = pal, project = FALSE,
                       opacity = 1, group = 'Agriculture'
        ) %>%
        addLegend(position = 'bottomleft',
                  pal = pal,
                  values = 0:100,
                  opacity = 1,
                  title = "Agricultural intensity",
                  labFormat = labelFormat(suffix = "%")
        )
      
    }
  })
  
  # ----- END ---- #
}