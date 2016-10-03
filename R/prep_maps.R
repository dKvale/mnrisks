library(dplyr)

# BG population and risk data
bg_data <- read.csv("M:\\DLK files\\MnRisk\\EJ_demog_traffic\\Demog&Traffic&MNRiskS_text.csv", stringsAsFactors = F)
bg_data <- bg_data[ , c("FID", "GEOID_num", "METRO", "MEAN_tdens", "COUNTY", "POPTOTAL", "POVERTY150", "AREA")]
bg_data <- mutate(bg_data, POVERTY_frx = 100 * POVERTY150/POPTOTAL,
                  PopDensity = POPTOTAL / (AREA * 3.22831e-7))

names(bg_data) <- c("FID", "GEOID", "METRO", "Traffic_Density", "COUNTY_FIPS", "Population", "Poverty150_Count","Area", "Percent_in_Poverty",  "Population_Density")

#-- Join County name
counties <- read.csv("map data//county_names.csv", stringsAsFactors=F)

names(counties)[1] <- "COUNTY_FIPS"

bg_data <- left_join(bg_data, counties[ , 1:3])

# Join Block group MNRISK results
bg_averages <- read.csv("M:\\MNRiskS 2011 Results\\Spatial Average\\BG_averages.csv", stringsAsFactors = F)

names(bg_averages) <- c("GEOID", 
                        "Cancer_Risk_Spatial_Mean", "Hazard_Index_Spatial_Mean",
                        "Cancer_Risk_Receptor_Avg", "Hazard_Index_Receptor_Avg",
                        "Percent_diff_Cancer_Risk", "Spatial_to_Recpt_Cancer_Ratio")

bg_data <- left_join(bg_data, bg_averages)

bg_data$County <- str_trim(bg_data$County)

saveRDS(bg_data, file = 'map data//bg_data.rdata')
