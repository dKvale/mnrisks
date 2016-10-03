library(readxl)


# Load sources
pt_sources <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Facility and point source release points\\Point_source_release_point_coordinates_&_stack_parameters.dbf")

names(pt_sources) <- c("Facility_ID", "Source_Name", "Lat",  "Long",  "Release_Point", "Release_Type", "Fugitive", "Short_Desc", "Stack_Height_m", "Diameter_m", "Exit_Velocity_m_s", "Temp_K")

pt_sources$Source_Type <- "Permitted facility"

#write.dbf(pt_sources, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Point Source Release Pts Params & Coordinates\\Point_source_release_point_coordinates_&_stack_parameters.dbf")

pt_sources <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Facility and point source release points\\Point_source_release_point_coordinates_&_stack_parameters.dbf")


# Allocated point names
ap_sources <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Allocated Point\\Allocated_point_sources.dbf")

ap_sources <- ap_sources[ , -c(13:14)]

names(ap_sources) <- c("Source_ID", "Lat", "Long", "County", "Source_Name", "GEO_ADDRESS", "City", "Zip_Code", "Source_Type")

ap_names <- read_excel("M:\\Emissions\\2011 Emissions\\Allocated point\\allocatedpoint_sources.xlsx")

names(ap_names)[6] <- "Source_ID"

ap_sources <- left_join(ap_sources, ap_names[ ,c(4:7,10,12,13)])

type_desc <- data_frame(Type = sort(unique(ap_sources$Type)), Source_Type = c("Dry cleaners", "Crematory", "Fluorescent light recycling", "Public wastewater treatment","Solid waste landfill", "Toxic Release Inventory"))

ap_sources <- left_join(ap_sources, type2)

ap_sources$SOURCE_TYP <- NULL

ap_sources$Type <- NULL

ap_sources$Source_Type <- ap_sources$Type_desc

ap_sources$Type_desc <- NULL

#write.dbf(ap_sources, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Allocated Point\\Allocated_point_sources.dbf")



# Gas stations
gas_sources <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Gas stations\\Gas_station_coordinates.dbf")

names(gas_sources)[c(1,9)] <- c("Source_Name","Frx_of_County_Sales")

gas_sources <- read.csv("M:\\MNRiskS 2011 development\\Send to Lakes\\Emissions\\Gas Stations Apportioned for LAKES v3.csv")

gas_sources <- group_by(gas_sources, ID, LATITUDE, LONGITUDE, Name, Address, City, State, Zip, County, FIPS, Units) %>%
  summarize(Sum_of_Emissions = sum(Emissions))

gas_sources$Source_Type <- "Gas station"

write.dbf(data.frame(gas_sources, stringsAsFactors = F), "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Gas stations\\Gas_station_coordinates.dbf")

#write.csv(gas_sources, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Gas stations\\Gas_stations_w_coordinates.csv"), row.names = F)


# Airports
airp <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Airports\\Airports.dbf")

names(airp) <- c("County", "Source_ID",  "Sum_of_Emissions", "Units", "Source_Name", "Lat", "Long")  

airp$Source_Type <- "Airport"

#write.dbf(airp, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Airports\\Airports.dbf")



# Prescribed burns
p_burn <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Fires\\Prescribed_burns.dbf")

p_burn <- p_burn[ , -c(11:14)]

names(p_burn) <- c("County", "Source_ID",  "Process_SCC_Code", "Sum_of_Emissions", "Lat", "Long", "Begin_Date", "End_Date", "Duration_hrs", "Hectares")  

p_burn$Source_Type <- "Prescribed burn"

write.dbf(p_burn, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Fires\\Prescribed_burns.dbf")


# Wildfires
w_fire <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Fires\\Wildfires.dbf")

w_fire <- w_fire[ , -c(11:14)]

names(w_fire) <- c("County", "Source_ID",  "Process_SCC_Code", "Sum_of_Emissions", "Lat", "Long", "Begin_Date", "End_Date", "Duration_hrs", "Hectares")  

w_fire$Source_Type <- "Wildfire"

write.dbf(w_fire, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Fires\\Wildfires.dbf")




# High traffic
roads <- read.dbf("M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Onroad High Traffic\\Onroad_Hi_Traffic.dbf")

roads <- roads[ , -c(11:14)]

names(roads) <- c("County", "Source_ID",  "Process_SCC_Code", "Sum_of_Emissions", "Lat", "Long", "Begin_Date", "End_Date", "Duration_hrs", "Hectares")  

roads$Source_Type <- "High traffic roads"

roads <- 

write.dbf(roads, "M:\\MNRiskS 2011 Results\\Special Requests\\Minneapolis\\Onroad High Traffic\\Onroad_Hi_Traffic.dbf")


# Convert all to lat/long





# Geo-locate sources (blockgroups, county, city, region)





# Combine all
all_sources <-

 


# SAVE ALL
saveRDS(pt_sources, "map data//facility_locations.rdata")  
  
saveRDS(pt_sources, "map data//facility_locations.rdata")
saveRDS(ap_sources, "map data//allocated_point_locations.rdata")
saveRDS(gas_sources, "map data//gas_stations.rdata")
saveRDS(airp, "map data//airports.rdata")


# Combine fires
fires <- rbind(p_burn, w_fire)
saveRDS(fires, "map data//wildfires.rdata")

saveRDS(roads, "map data//high_traffic_roads.rdata")


##
