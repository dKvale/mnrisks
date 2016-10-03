if(input$top50 && !variable %in% c("Population", "Percent_in_Poverty")) {
  top50_sub <- filter(top50, Pollutant == paste(strsplit(input$pollutant_var1, "_")[[1]][1:2], collapse = "_"))
  bgmap <- bgmap %>% 
    addCircleMarkers(lng = top50_sub$Long, lat = top50_sub$Lat, weight=1.2,
                     radius=5,
                     color = "Red",
                     opacity = .6,
                     fillColor = "Grey",
                     fillOpacity = .65,
                     popup=paste(variable, ": <b>", top50_sub[[variable]], "</b></br>",
                                 "Receptor: <b>", top50_sub$Receptor, "</b>"))
  
}
