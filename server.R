# Server for mnrisks
library(dplyr)
library(leaflet)
library(shiny)
library(markdown)
library(sp)
library(scales)


###########
#source('prep_maps.R')#
###########

# LOAD block group risks and demographics
df_blocks  <- readRDS('map data//bg_data.rdata')[ , c(2:4,6:7,9:10,12)]
#saveRDS(df_blocks, 'map data//bg_data.rdata')


# Load demography
library(readr)

demo <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/Mnrisks/mnrisks 2008/Env. Justice/EJ_demog_traffic/Demog&Traffic&MNRiskS_text.csv")

demo <- demo[ , c("GEOID_num","POPTOTAL", "POPOVER5", "AREA", "WHITENH")]

names(demo)[1] <- "GEOID"

df_blocks <- left_join(df_blocks, demo)

df_blocks$POP_Under5 <- df_blocks$POPTOTAL - df_blocks$POPOVER5 

df_blocks$NONWHITE_or_HISPANIC <- df_blocks$POPTOTAL - df_blocks$WHITENH

df_blocks$Frx_Non_white_or_Hispanic <- df_blocks$NONWHITE_or_HISPANIC / df_blocks$POPTOTAL

df_blocks$POP_Under5_density <- df_blocks$POP_Under5 / df_blocks$AREA 


# LOAD cities
cities <- readRDS("map data//city_blockgroups.rdata")
#cities$City <- gsub("St[.] Paul", "St Paul", cities$City)

df_blocks <- left_join(df_blocks, cities)


# Load sources
all_sources <- readRDS("map data//city_blockgroups.rdata")


# LOAD receptor maps
top50 <- data.frame(readRDS('map data//Top_50_receptors.rdata')[ , -c(9)], stringsAsFactors = F)
#recept_map <- readRDS('map data//risk_points_3county.rdata')
#saveRDS(recept_map, 'map data//risk_points_3county.rdata')

#unlabeled  <- readRDS('map data//missing_receptors.rdata')
#unlabeled  <- readRDS("M:\\MNRiskS 2011 development\\Receptors\\LAKES receptors shapefile.Rdata")
#unlabeled  <- subset(unlabeled, County %in% "ST. LOUIS")
#saveRDS(unlabeled, 'map data//missing_receptors.rdata')

#names(recept_map@data) <- c("GEOID", "Cancer_Risk")
#recept_map$GEOID <- as.numeric(recept_map$GEOID)
#recept_map@data <- left_join(recept_map@data[ ,1:2], df_blocks[ ,c("GEOID", "County")])
#saveRDS(recept_map, file = 'map data//risk_points.rdata')
#saveRDS(subset(recept_map, County %in% c("Ramsey", "St. Louis", "Itasca")), file = 'map data//risk_points_3county.rdata')


# County demographics
if(FALSE) {
df_county <- group_by(df_blocks, COUNTY_FIPS) %>% 
             summarize(FID = min(FID, na.rm=T),
                       METRO = max(METRO, na.rm=T), 
                       Traffic_Density = sum(Traffic_Density * Area, na.rm=T) / sum(Area), 
                       County = max(County, na.rm = T),
                       Population = sum(Population, na.rm=T),
                       Population_Density = sum(Population, na.rm=T) / sum(Area * 3.22831e-7),
                       Percent_in_Poverty = 100 * sum(Poverty150_Count) / max(1, sum(Population)),
                       Cancer_Risk_Receptor_Avg  = mean(Cancer_Risk_Receptor_Avg , na.rm=T),
                       Hazard_Index_Receptor_Avg = mean(Hazard_Index_Receptor_Avg, na.rm=T),
                       Cancer_Risk_Spatial_Mean  = sum(Cancer_Risk_Spatial_Mean * Area, na.rm=T) / sum(Area),
                       Hazard_Index_Spatial_Mean = sum(Hazard_Index_Spatial_Mean * Area, na.rm=T) / sum(Area),
                       Percent_diff_Cancer_Risk  = (Cancer_Risk_Receptor_Avg - Cancer_Risk_Spatial_Mean) / ((Cancer_Risk_Receptor_Avg + Cancer_Risk_Spatial_Mean)/2)
                       )
}

#-- Load county and blockgroup shapefile with `tigris` package
#counties <- tigris::counties(state=c("MN"), cb=F)
counties <- readRDS('map data//counties.rdata')

#-- Block_groups <- tigris::block_groups(state=c("MN"), cb=F)
block_groups <- readRDS('map data//block_groups.rdata')

#-- Add population and risk data
#counties <- tigris::geo_join(counties, df_county, "County_FIPS", "COUNTY_FIPS")
df_blocks <- tigris::geo_join(block_groups, df_blocks, "GEOID", "GEOID")

#counties <- subset(counties, !is.na(County_Name))
#block_groups <- subset(block_groups, !is.na(Population))



#-- Server for mnrisks
shinyServer(function(input, output, session) {
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })

##-- BLOCK GROUP MAP    
  output$bgmap <- renderLeaflet({
    
    print(input$county_var1)
    print(input$city_var1)
    print(input$variable1)
    print(input$receptors1)
    print(input$pollutant_var1)
    
    variable <- input$variable1
    
    if(is.null(input$city_var1)) city_select <- "All"
    else city_select <- input$city_var1
    
    if(is.null(input$county_var1)) county_select <- "All"
    else county_select <- input$county_var1
    
    demographic <- variable %in% c("Frx_Non_white_or_Hispanic", "Population", "Percent_in_Poverty", "Population_Density", "Traffic_Density", "POP_Under5")
    
    #-- Add population and risk data
    if(!demographic) {
      block_risk <- readRDS(paste0("map data/Pollutants/", input$pollutant_var1, "_risks.Rds"))
      #block_risk <- left_join(block_risk, df_blocks[ ,-c(4,8:9)])
     
      block_risk <- tigris::geo_join(block_groups, block_risk[ ,c(names(block_risk)[c(1:5,24:25)], variable)], "GEOID", "GEOID")
      #print(block_risk[1, ])
    } else {
      block_risk <- df_blocks
    }
    
      
    #-- Filter receptors
    if(input$receptors1 && !demographic) {
      recept_temp <- filter(top50, CAS == input$pollutant_var1)
      recept_temp$variable <- recept_temp[ , gsub("_mean", "", variable)]
    } 
    
    if(FALSE) {
    #-- Filter region
    if(input$region_var1 != "All") {
      block_risk <- subset(block_risk, Region %in% input$region_var1)
      
      if(input$receptors1 & !demographic) {
        recept_temp   <- filter(recept_temp, County %in% block_risk$County)
      }
    } 
    }
    
    
    #-- Filter counties
    if(!"All" %in% county_select) {
       block_risk <- subset(block_risk, County %in% toupper(county_select))
       
       if(input$receptors1 & !demographic) {
         if(nrow(recept_temp) > 1) recept_temp   <- filter(recept_temp, County %in% block_risk$County)
       }
    } 
    
    #-- Filter city
    if(!"All" %in% city_select) {
      block_risk <- subset(block_risk, City %in% city_select)
      
      if(input$receptors1 & !demographic) {
      if(nrow(recept_temp) > 1) recept_temp   <- filter(recept_temp, City %in% block_risk$City)
      }
    } 
    
    #-- Filter sources
    #if(!is.null(input$source_var1)) {
    #if(input$gas1 | input$facility1 | input$airport1 | input$fires1 | input$hwy1) {
    #  sources <- readRDS('map data//source_locations.rdata')
   #   recept_temp$variable <- recept_temp[ , gsub("_mean", "", variable)]
   # } 
    
    
    #-- Set legend parameters
    
    #-- define hex colors for legend
    if(variable == "Percent_diff_Cancer_Risk") {
      
      legend_colors <- c('#b2182b','#ef8a62','#fddbc7',
                         '#f7f7f7',
                         '#d1e5f0','#67a9cf','#2166ac')
  
      #legend_colors <- c('#762a83','#af8dc3','#e7d4e8',
      #                   '#f7f7f7',
      #                   '#d9f0d3','#7fbf7b','#1b7837') 
    } else {
      legend_colors <- c("#ffffcc", "#c7e9b4", "#7fcdbb", 
                        "#41b6c4", "#2c7fb8", "#253494") 
    }
    # Define breaks for legend
    if(variable == "Population"){
      
      breaks <- c(0, 75, 700, 1000, 1500, 2000, 200000)
      
      block_risk$var_bins <- cut(block_risk[[variable]], 
                                      breaks= breaks, 
                                      labels=legend_colors,
                                      include.lowest=T)
      
      breaks <- c("75","700","1,000","1,500","2,000","3,000+", "8,000")
      
    } else if(variable == "Percent_diff_Cancer_Risk") {
      
      breaks <- c(-10, -.25, -.1, 0, .1, .25, .5, 10)
      
      block_risk$var_bins <- cut(block_risk[[variable]], 
                                        breaks= breaks, 
                                        labels=legend_colors,
                                        include.lowest=T)
      
      breaks <- c(-.5, -.25, -.1, 0, '+0.1', '+0.25', 99, '+0.5')
      
    } else {
      breaks <- quantile(block_risk[[variable]], c(0,0.17,0.33,0.51,0.68,0.91, 1), na.rm=T) * c(1,1,1,1,1,1,1.01)
      
      if(breaks[2] < breaks[3]/10) breaks[2] <- breaks[3]/2
      
      block_risk$var_bins <- cut(block_risk[[variable]], 
                                        breaks= breaks, 
                                        labels= legend_colors,
                                        include.lowest=T)
      
      if(input$receptors1 & grepl("Cancer", variable) & !demographic) {
        if(nrow(recept_temp) >1) {
        recept_temp$var_bins <- cut(recept_temp$Cancer_Risk, 
                                    breaks= breaks * c(0,1,1,1,1,1,100), 
                                    labels= legend_colors,
                                    include.lowest=T)
      }
      }
      breaks <- as.character(signif(breaks, 2))
    }

    
    if(!grepl("Cancer", variable) & input$receptors1 & !demographic) {
      if(nrow(recept_temp) > 1) {
      recept_temp$var_bins <- "#d3d3d3"
    }}
    
    #-- Plot using leaflet
    ##-- Create map and assign popup options
    bgmap <- leaflet(block_risk) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(stroke=T,
                  color="#d3d3d3",
                  weight=1.2,
                  smoothFactor=0.3,
                  fillOpacity=.71,
                  fillColor= ~var_bins,
                  popup=paste0("<b><span style='font-size:1.5em;'>", signif(block_risk[[variable]], 2), "</span> </b><br>",
                                gsub("_", " ", variable), "<br><br>",
                                block_risk$County, ": ", block_risk$GEOID, " "))
                               
    
    if(input$receptors1 & !demographic) {
      if(nrow(recept_temp) > 1) {
      bgmap <- bgmap %>% 
               addCircleMarkers(lng = recept_temp$Long, lat = recept_temp$Lat, weight=1.3,
                 radius=3.2,
                 color = "#7e7e7e",
                 opacity = .6,
                 fillColor = recept_temp$var_bins,
                 fillOpacity = .65,
                 popup=paste("<b><span style='font-size:1.5em;'>", signif(recept_temp$variable, 2), "</span> </b><br>",
                             gsub("_", " ", variable), "<br><br>",
                             recept_temp$County, ": ", recept_temp$GEOID, " "))
      }
      }
    
    
    bgmap %>% addLegend("topleft",
                colors  = rev(legend_colors),
                labels  = rev(breaks[-7]),
                title   = gsub("_", " ", variable),
                opacity = .96)
    
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
})

