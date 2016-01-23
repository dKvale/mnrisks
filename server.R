# Server for mnrisks

library('dplyr')
library('leaflet')
#install_github("choroplethr", "arilamstein")
#devtools::install_github('walkerke/tigris')
library(shiny)
library(markdown)


# BG population and risk data
df_blocks <- readRDS('map_data//bg_data.rdata')

df_county <- group_by(df_blocks, County) %>% 
             summarize()

# Name population column
names(df_pop_county) <- c("GEOID", "Population")

#-- Load county and blockgroup shapefile with `tigris` package
#counties <- tigris::counties(state=c("MN"), cb=F)
counties <- readRDS('map data//counties.rdata')

#lock_groups <- tigris::block_groups(state=c("MN"), cb=F)
block_groups <- readRDS('map data//block_groups.rdata')

#-- Add population and risk data
counties     <- tigris::geo_join(counties, df_county, "GEOID", "GEOID")
block_groups <- tigris::geo_join(block_groups, df_blocks, "GEOID", "GEOID")


#-- Server for mnrisks
shinyServer(function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$bgmap <- renderLeaflet({
    
    #-- Set legend parameters
    
    #-- define hex colors for legend
    legend_colors <- c("#ffffcc", "#c7e9b4", "#7fcdbb", 
                       "#41b6c4", "#2c7fb8", "#253494") 
    
    # define population breaks for legend
    block_groups$var_bins <- cut(block_groups$Population, 
                             breaks=c(0, 100, 500, 1000, 1500, 2000, 3000), 
                             labels=legend_colors,
                             include.lowest=T)

    #-- Plot using leaflet
    
    # Create map and assign popup options
    map <- leaflet(block_groups) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(stroke=T,
                  color=~var_bins,
                  weight=1.1,
                  smoothFactor=0.3,
                  fillOpacity=.75,
                  fillColor= ~var_bins,
                  popup=paste0("<b>", block_groups$GEOID, "</b><br> 
                                Population: ", block_groups$variable)) %>%
      addLegend("topleft",
                colors  = legend_colors,
                labels  = c("0-5K","5K-10K","10K-25K","25K-50K","50K-250K","250K-10M"),
                title   = "Population (2012)",
                opacity = 1)
    
    # Open map
    map
  })
  
  
  output$map <- renderLeaflet({
    
#-- Set legend parameters
    
    #-- define hex colors for legend
    legend_colors <- c("#ffffcc", "#c7e9b4", "#7fcdbb", 
                       "#41b6c4", "#2c7fb8", "#253494") 
    
    # define population breaks for legend
    counties$Pop_bins <- cut(counties$Population, 
                             breaks=c(0, 5000, 10000, 25000, 50000, 250000, 10000000), 
                             labels=legend_colors,
                             include.lowest=T)
    
    # Use a colorBin function (optional)            
    #Pop_bins <- colorBin(legend_colors, counties$Population, c(0, 5000, 10000, 25000, 50000, 250000, 10000000), pretty = FALSE)
    
#-- Plot using leaflet
    
    # Create map and assign popup options
    map <- leaflet(counties) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(stroke=T,
                  color=~Pop_bins,
                  weight=1.2,
                  smoothFactor=0.3,
                  fillOpacity=.75,
                  fillColor= ~Pop_bins,
                  popup=paste0("<b>", counties$NAME, "</b><br> 
                                Population: ", counties$Population)) %>%
      addLegend("topleft",
                colors  = legend_colors,
                labels  = c("0-5K","5K-10K","10K-25K","25K-50K","50K-250K","250K-10M"),
                title   = "Population (2012)",
                opacity = 1)
    
    # Open map
    map
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
})

