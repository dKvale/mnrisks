
# ui.R
tabPanel("County Map",
         div(class="outer",
             
             leafletOutput("map", width="100%", height="100%"),
             
             absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                           width = 330, height = "auto",
                           
                           h3("Map Layers"),
                           
                           selectInput("variable2", "Variable", vars, selected = "Population"),
                           selectInput("county_var2", "County", county_vars, selected = "All"),
                           h5("Map Options"),
                           checkboxInput("receptors2", "Show point source receptors", value=F),
                           br()
                           
             ))),


#server.R 

#-- COUNTY MAP  
output$map <- renderLeaflet({
  
  print(input$county_var2)
  print(input$variable2)
  
  variable <- input$variable2
  
  #-- Filter receptors
  #if(input$receptors2) {
  # recept_temp <- recept_map
  #} 
  
  #-- Filter counties
  if(input$county_var2 != "All" ) {
    counties_temp <- subset(counties, County_Name %in% input$county_var2)
    
  } else {
    counties_temp <- counties
  }
  #-- Set legend parameters
  
  #-- define hex colors for legend
  legend_colors <- c("#ffffcc", "#c7e9b4", "#7fcdbb", 
                     "#41b6c4", "#2c7fb8", "#253494") 
  
  if(variable == "Percent_diff_Cancer_Risk") {
    
    legend_colors <- c('#b2182b','#ef8a62','#fddbc7',
                       '#f7f7f7',
                       '#d1e5f0','#67a9cf','#2166ac')
  }
  
  # define population breaks for legend
  if(variable == "Population"){
    counties_temp$var_bins <- cut(counties_temp[[variable]], 
                                  breaks = c(0, 5000, 10000, 25000, 50000, 250000, 10000000), 
                                  labels=legend_colors,
                                  include.lowest=T)
    
    breaks <- c("0","5K","10K","25K","50K","250K+", "5M+")
    
  } else if(variable == "Percent_diff_Cancer_Risk") {
    
    breaks <- c(-10, -.25, -.1, 0, .1, .25, .5, 10)
    
    counties_temp$var_bins <- cut(counties_temp[[variable]], 
                                  breaks= breaks, 
                                  labels=legend_colors,
                                  include.lowest=T)
    
    breaks <- c(-.5, -.25, -.1, 0, '+0.1', '+0.25', 99, '+0.5')
  } else {
    breaks <- quantile(counties_temp[[variable]], c(0,.13,.33,.55,.72,.90, 1), na.rm=T) * c(1,1,1,1,1,1,1.01)
    
    counties_temp$var_bins <- cut(counties_temp[[variable]], 
                                  breaks= breaks, 
                                  labels=legend_colors,
                                  include.lowest=T)
    
    breaks <- as.character(signif(breaks, 2))
  }
  
  
  
  # Use a colorBin function (optional)            
  #Pop_bins <- colorBin(legend_colors, counties$Population, c(0, 5000, 10000, 25000, 50000, 250000, 10000000), pretty = FALSE)
  
  #-- Plot using leaflet
  
  # Create map and assign popup options
  map <- leaflet(counties_temp) %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(stroke=T,
                color=~var_bins,
                weight=1.2,
                smoothFactor=0.3,
                fillOpacity=.7,
                fillColor= ~var_bins,
                popup=paste0("<b>", counties_temp$County_Name, "</b><br> ",
                             variable, ": ", signif(counties_temp[[variable]], 2)))
  
  
  
  map %>% addLegend("topleft",
                    colors  = rev(legend_colors),
                    labels  = rev(breaks[-7]),
                    title   = variable,
                    opacity = .96)
  
})
