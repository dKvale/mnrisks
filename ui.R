# user interface
library(shiny)
library(leaflet)
library(markdown)
library(scales)

#-- Variable options
vars <- c("Non-Cancer - ambient air inhalation risk (annual mean)" = "Inhalation_Hazard_mean", 
          "Cancer - ambient air inhalation risk (estimated lifetime)" = "Inhalation_Cancer_mean", 
          "Population (2010)" = "Population", 
          "Population Density (2010)" = "Population_Density",
          "Children under 5 (2010)" = "POP_Under5",
          "Identify as Hispanic or non-white" = "Frx_Non_white_or_Hispanic",
          "% in Poverty" = "Percent_in_Poverty",
          "Traffic Density" = "Traffic_Density")
          
block_vars <- c("All", 271230361001, 271230413022)

#county_vars <- c("All", "Hennepin","Ramsey", "St. Louis", "Itasca")

county_vars <- c("All", unique(df_blocks$County))

region_vars <- c("All", "Northeast", "Metro (7 County)", "Northwest", "Central", "Southwest", "Southeast")

city_vars   <- c("All", "Minneapolis", "St. Paul", "Duluth", "Rochester", "St. Cloud", (unique(df_blocks$City)[!unique(df_blocks$City) %in% c("Minneapolis", "St. Paul", "Duluth", "Rochester", "St. Cloud")]))

source_vars <- c("None", "All", "Gas stations", "Facilities & point sources","Airports", "Wildfires & prescribed burns", "High traffic roadways")

#cat(paste(paste0('"', a$Pollutant, '"', " = ", '"', a$CAS,'"'), collapse = "\n"))

pollutant_vars <- c("All" = "All",
                    "Acetaldehyde" = "75-07-0",
                    "Acetamide" = "60-35-5",
                    "Acetone" = "67-64-1",
                    "Acetonitrile" = "75-05-8",
                    "Acetophenone" = "98-86-2",
                    "Acrolein" = "107-02-8",
                    "Acrylamide" = "79-06-1",
                    "Acrylic Acid" = "79-10-7",
                    "Acrylonitrile" = "107-13-1",
                    "Aldehydes, Unspeciated" = "E761379",
                    "Allyl Chloride" = "107-05-1",
                    "Aminobiphenyl 4-" = "92-67-1",
                    "Ammonia" = "7664-41-7",
                    "Aniline" = "62-53-3",
                    "Antimony" = "7440-36-0",
                    "Arsenic" = "7440-38-2",
                    "Atrazine" = "1912-24-9",
                    "Barium" = "7440-39-3",
                    "Benzene" = "71-43-2",
                    "Benzo(a)Fluoranthene" = "203-33-8",
                    "Benzyl chloride" = "100-44-7",
                    "Beryllium" = "7440-41-7",
                    "Biphenyl" = "92-52-4",
                    "Bis(2-chlorethyl)ether" = "111-44-4",
                    "Bromoform (Tribromomethane)" = "75-25-2",
                    "Butadiene 1,3-" = "106-99-0",
                    "Butyl Cellosolve" = "111-76-2",
                    "Cadmium" = "7440-43-9",
                    "Carbon Disulfide" = "75-15-0",
                    "Carbon Tetrachloride" = "56-23-5",
                    "Catechol" = "120-80-9",
                    "Chlorine" = "7782-50-5",
                    "Chloroacetophenone 2-" = "532-27-4",
                    "Chlorobenzene" = "108-90-7",
                    "Chloroethane" = "75-00-3",
                    "Chloroform (Trichloromethane)" = "67-66-3",
                    "Chloroprene" = "126-99-8",
                    "Chromium (All)" = "Chromium (All)",
                    "Cobalt" = "7440-48-4",
                    "Cresol (All)" = "Cresol (All)",
                    "Cumene (Isopropylbenzene)" = "98-82-8",
                    "Cyanide" = "57-12-5",
                    "Dibenzo(AH)Acridine" = "226-36-8",
                    "Dibenzo(CG)Carbazole" = "194-59-2",
                    "Dibenzofuran" = "132-64-9",
                    "Dichlorobenzenes" = "25321-22-6",
                    "Dichlorobiphenyl 4,4'-" = "2050-68-2",
                    "Dichloroethane 1,1-" = "75-34-3",
                    "Dichloroethane, 1,2- (Ethylene Dichloride)" = "107-06-2",
                    "Dichloroethylene 1,1-" = "75-35-4",
                    "Dichloroethylene, cis-1,2-" = "156-59-2",
                    "Dichlorophenoxyacetic acid 2,4- (2,4-D)" = "94-75-7",
                    "Dichloropropane, 1,2-" = "78-87-5",
                    "Dichloropropene, 1,3-" = "542-75-6",
                    "Diethanolamine" = "111-42-2",
                    "Diethyl Sulfate" = "64-67-5",
                    "Dimethoxybenzidine, 3,3'-" = "119-90-4",
                    "Dimethyl Phthalate" = "131-11-3",
                    "Dimethylanil" = "121-69-7",
                    "Dimethylbenzidine, 3,3'-" = "119-93-7",
                    "Dimethylformamide N,N" = "68-12-2",
                    "Dinitrophenol, 2,4-" = "51-28-5",
                    "Dinitrotoluene, 2,4-" = "121-14-2",
                    "Dioxane, 1,4-" = "123-91-1",
                    "Dioxin_Furans" = "Dioxin_Furans",
                    "Epichlorohydrin (1-Chloro-2,3- epoxypropane)" = "106-89-8",
                    "Epoxybutane 1,2-" = "106-88-7",
                    "Ethyl Acrylate" = "140-88-5",
                    "Ethylbenzene" = "100-41-4",
                    "Ethylene Dibromide" = "106-93-4",
                    "Ethylene Glycol" = "107-21-1",
                    "Ethylene Imine" = "151-56-4",
                    "Ethylene Oxide" = "75-21-8",
                    "Formaldehyde" = "50-00-0",
                    "Glycol Ethers" = "Glycol Ethers",
                    "Hexachloro-1,3-butadiene (Perchlorobutadiene)" = "87-68-3",
                    "Hexachlorobenzene" = "118-74-1",
                    "Hexachlorocyclopentadiene" = "77-47-4",
                    "Hexachloroethane (Perchloroethane)" = "67-72-1",
                    "Hexamethylene 1,6 Diisocyante" = "822-06-0",
                    "Hydrogen Chloride" = "7647-01-0",
                    "Hydrogen Fluoride" = "7664-39-3",
                    "Hydroquinone" = "123-31-9",
                    "Isophorone" = "78-59-1",
                    "Lead" = "7439-92-1",
                    "Maleic Anhydride" = "108-31-6",
                    "Manganese" = "7439-96-5",
                    "Mercury (All)" = "Mercury (All)",
                    "Methanol" = "67-56-1",
                    "Methyl Bromide (Bromomethane)" = "74-83-9",
                    "Methyl Cellosolve" = "109-86-4",
                    "Methyl Chloride (Chloromethane)" = "74-87-3",
                    "Methyl Ethyl Ketone (2-Butanone)" = "78-93-3",
                    "Methyl Hydrazine" = "60-34-4",
                    "Methyl Iodide" = "74-88-4",
                    "Methyl Isobutyl Ketone" = "108-10-1",
                    "Methyl Isocyanate" = "624-83-9",
                    "Methyl Methacrylate" = "80-62-6",
                    "Methyl Tert Butyl Ether" = "1634-04-4",
                    "Methylene Chloride" = "75-09-2",
                    "Methylene Dianiline 4,4-" = "101-77-9",
                    "Methylenediphenyl Diisocyanate 4,4-" = "101-68-8",
                    "Naphthalene" = "91-20-3",
                    "NHexane" = "110-54-3",
                    "Nickel" = "7440-02-0",
                    "Nitrobenzene" = "98-95-3",
                    "Nitrophenol, 4-" = "100-02-7",
                    "Nitropropane 2-" = "79-46-9",
                    "Nitrosodimethylamine n-" = "62-75-9",
                    "PAHs" = "PAHs",
                    "PCBs" = "PCBs",
                    "Pentachloronitrobenzene (PCNB)" = "82-68-8",
                    "Pentachlorophenol" = "87-86-5",
                    "Phenol" = "108-95-2",
                    "Phenylenediamine N" = "106-50-3",
                    "Phosphine" = "7803-51-2",
                    "Phthalates" = "Phthalates",
                    "Phthalic anhydride (1,2-Benzene dicarboxylic anhydride)" = "85-44-9",
                    "PM2.5 Diesel" = "MDPM25",
                    "Propane Sultone 1,3-" = "1120-71-4",
                    "Propylene" = "115-07-1",
                    "Propylene Oxide" = "75-56-9",
                    "Selenium" = "7782-49-2",
                    "Styrene" = "100-42-5",
                    "Tetrachloroethane, 1,1,2,2-" = "79-34-5",
                    "Tetrachloroethylene (Perchloroethylene)" = "127-18-4",
                    "Tetrafluoroethane 1,1,1,2- (HFC-134a)" = "811-97-2",
                    "Toluene" = "108-88-3",
                    "Toluene 2,4 Diisocyante" = "584-84-9",
                    "Toluidine, o-" = "95-53-4",
                    "Trichlorobenzene, 1,2,4-" = "120-82-1",
                    "Trichloroethane, 1,1,1-" = "71-55-6",
                    "Trichloroethane, 1,1,2-" = "79-00-5",
                    "Trichloroethylene" = "79-01-6",
                    "Trichlorophenol, 2,4,5-" = "95-95-4",
                    "Trichlorophenol, 2,4,6-" = "88-06-2",
                    "Triethylamine" = "121-44-8",
                    "Trifluralin" = "1582-09-8",
                    "Trimethylbenzene 1,2,4-" = "95-63-6",
                    "Trimethylbenzene, 1,3,5-" = "108-67-8",
                    "Trimethylbenzenes, Unspeciated" = "25551-13-7",
                    "Vinyl Acetate" = "108-05-4",
                    "Vinyl Bromide" = "593-60-2",
                    "Vinyl Chloride" = "75-01-4",
                    "Xylenes (Mixed Isomers)" = "1330-20-7",
                    "Zinc" = "7440-66-6")

shinyUI(navbarPage("MNRISKS 2011",
                   tabPanel("Block Group Map",
                            div(class="outer",
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("CSS//styles.css")
                                  #includeScript("gomap.js")
                                ),
                                
                                leafletOutput("bgmap", width="100%", height="100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h3("Map Layers"),
                                              
                                              #selectInput("color", "Color", vars),
                                              selectInput("variable1", "Summary", vars, selected = "Inhalation_Hazard_mean"),
                                              
                                              selectInput("pollutant_var1", "Pollutant", pollutant_vars, selected = "50-00-0"),
                                              #selectInput("region_var1", "Region", region_vars, selected = "All"),
                                              
                                              selectizeInput("county_var1", "County", county_vars, selected = "All", multiple=TRUE),
                                              selectizeInput("city_var1", "City", city_vars, selected = c("Minneapolis", "St. Paul"), multiple=TRUE),
                                           
                                              h5("Add Layers"),
                                              #checkboxInput("receptors1", "Show point source receptors", value=F),
                                              selectizeInput("source_var1", "Source locations", source_vars, selected = "None", multiple=TRUE),
                                              
                                              checkboxInput("receptors1", "Top 50 receptors", value=F),
                                              br()
                                              
                                ))),
                   
                   
                  
                   navbarMenu("More",
                              ##tabPanel("Data Table", DT::dataTableOutput("table")),
                              #tabPanel("Plots",
                               #        sidebarLayout(
                               #          sidebarPanel(
                              #             radioButtons("plotType", "Plot type",
                               #                         c("Scatter"="p", "Line"="l"))),
                               #          mainPanel(
                               #            plotOutput("plot")))
                              #),
                              tabPanel("About",
                                       fluidRow(
                                         column(6,
                                                includeMarkdown("about.md")),
                                         column(3,
                                                img(src = "https://upload.wikimedia.org/wikipedia/commons/b/b1/Minnesota_population_map_cropped.png",
                                                    height=400)))))))

#----#
