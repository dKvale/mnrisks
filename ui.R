# user interface
library(shiny)
library(leaflet)
library(markdown)

#-- Variable options
vars <- c("Population", "Cancer Risk (receptor average)", "Cancer risk (spatial average)", "Hazard Index")

block_vars <- c("All", 270017701001)

county_vars <- c("All", "Ramsey")

shinyUI(navbarPage("MN-RISKS",
                   tabPanel("Block Group Map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("CSS//styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("bgmap", width="100%", height="100%"),
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("MN-RISKS"),
                                              
                                              #selectInput("color", "Color", vars),
                                              selectInput("variable", "Map Variable", "Population", selected = "Cancer Risk, receptor average"),
                                              selectInput("county_var", "County", county_vars, selected = "All")
                                              selectInput("block_var", "Block Group", block_vars, selected = "All")
                                              
                                              #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                              #                 # Only prompt for threshold when coloring or sizing by superzip
                                              #                numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                              #                )
                                              #plotOutput("histCentile", height = 200),
                                              #plotOutput("scatterCollegeIncome", height = 250)
                                              
                                ))),
                   tabPanel("County Map",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                            
                            leafletOutput("map", width="100%", height="100%"),
                        
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          
                                          h2("MN-RISKS"),
                                          
                                          #selectInput("color", "Color", vars),
                                          selectInput("variable", "Map Variable", "Population", selected = "Population"),
                                          selectInput("county_var", "County", county_vars, selected = "All")
                                          
                                          #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                          #                 # Only prompt for threshold when coloring or sizing by superzip
                                          #                numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                          #                )
                                          #plotOutput("histCentile", height = 200),
                                          #plotOutput("scatterCollegeIncome", height = 250)
                             
                            ))),
                   tabPanel("Data Table",
                            DT::dataTableOutput("table")
                   ),
                   tabPanel("Plots",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("plotType", "Plot type",
                                             c("Scatter"="p", "Line"="l"))),
                              mainPanel(
                                plotOutput("plot")))
                   ),
                   navbarMenu("More",
                              
                              tabPanel("Plot",
                                       sidebarLayout(
                                         sidebarPanel(
                                           radioButtons("plotType", "Plot type",
                                                        c("Scatter"="p", "Line"="l"))
                                         ),
                                         mainPanel(
                                           plotOutput("plot")))
                              ),
                              tabPanel("About",
                                       fluidRow(
                                         column(6,
                                                includeMarkdown("about.md")),
                                         column(3,
                                                img(src = "https://upload.wikimedia.org/wikipedia/commons/b/b1/Minnesota_population_map_cropped.png"))))
                   )))


#----#
