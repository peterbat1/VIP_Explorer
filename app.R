#
# Very Important Places (VIP) Explorer
#
# Conservation Priority Exploration Tool accompanying the pupblication:
#    Beaumont et al (202?).....
# 
# 
# Peter D. Wilson
# Adjunct Fellow
# Dept. of Biological Sciences
# Faculty of Science and Engineering
# Macquarie University, Sydney, Australia
#
# Developed on behalf of the VIP team:
#   Linda J. Beaumont (1),
#   David A. Nipperess (1,2),
#   Peter D. Wilson (1,3),
#   John B. Baumgartner (4),
#   Manuel Esperon-Rodriguez (5)
#
#   1. Department of Biological Sciences, Macquarie University, North Ryde, NSW 2109
#   2. NSW Department of Planning, Industry and Environment, Locked Bag 5022, Parramatta NSW 2124
#   3. Royal Botanic Gardens and Domain Trust, Sydney, NSW 2000
#   4. Centre of Excellence for Biosecurity Risk Analysis (CEBRA), School of BioSciences, University of Melbourne, Parkville, Victoria, Australia     
#   5. Hawkesbury Institute for the Environment, Western Sydney University, Locked Bag 1797, Penrith NSW 2751 Australia
#
#
# 2020-09-15; 2020-10-03; 2020-10-28; 2021-10-01; 2021-10-08: Added code to stop app on browser or browser tab close

library(shiny)
library(shinybusy)
library(shinyscreenshot)
library(raster)
library(leaflet)
library(leafem)
library(sf)
library(jsonlite)
library(geojsonsf)
library(rstudioapi)

############################## GLOBAL DATA ###################################
# Load data layers and intialise global data structures
#d <- readRDS("www/data/combined_new_updatedForestNonforest_withBiome.rds")
d <- readRDS("www/data/combined_new_updatedForestNonforest.rds")

#d <- na.omit(d)
goodCells <- which(d$clim_rcp85_2070 < 3.5)
d <- d[goodCells, ]
cellInd <- d[, "cell", drop = TRUE]

# Default percentiles defining thresholds
thr_comp_high <- 75  # Complementarity
thr_habitat_high <- 75 # Landuse state
thr_climate_high <- 75 #  Climate novelty

# Raster used as the base for thresholded layers
baseRas <- raster::raster("www/data/land_mask2.tif")

# Colours for the layers
climateChangeColour <- "blue"
habitatLossColour <- "red"
vipColour <- "orange"
wickedColour <- "black"

# Lookup xref between clade names and abbreviations used in data table column names
cladeNames <- c("Tetrapoda", "Mammalia", "Amphibia", "Squamata", "Aves", "Crocodilia", "Testudines")
abbrevClade <- c("tetr", "mamm", "amph", "squa", "bird", "croc", "turt")
names(abbrevClade) <- cladeNames


#####################################
###### Function written by John B. Baumgartner
polygonize <- function(srcfile, dstfile, mask, ogr_format, fieldname, band, connect8 = FALSE)
{
  options <- if(isTRUE(connect8)) 'CONNECT8=8' else character(0)
  contour_options <- character(0)
  if(missing(mask)) mask <- character(0)
  .Call("_sf_CPL_polygonize", PACKAGE = "sf", srcfile, mask, 
        'GTiff', ogr_format, layer = dstfile, options, 0, #iPixValField, 
        contour_options, use_contours = FALSE, use_integer = TRUE)
}


#####################################
ui <- fluidPage(
  ## Method to cause shiny app to stop running when browser (or browser tab)
  ## is closed. See companion observe() in server function.
  ## Source: Winston Chang https://groups.google.com/g/shiny-discuss/c/JptpzKxLuvU
  ## last accessed 2021-10-08
  tags$script(type = "text/javascript", "
      $(function() { // Run when DOM ready
        $(window).bind('beforeunload', function(e) {
          Shiny.onInputChange('quit', true); // sets input$quit to true
        });
      });
   "),
  
  
  # Styling of action buttons following:
  # https://stackoverflow.com/questions/60083061/how-to-vertically-center-action-buttons-in-a-shiny-app answer by MKa on 2020-02-07
  tags$style(
    HTML('
          #resetcol {
             display: flex;
             align-items: center;
             justify-content: center;
             padding-top:25px;
             padding-bottom:25px
             }
             
          #reset {
             background-color:#ff7f50;
             color:white;
             font-weight:bold;
             }
         
         #help {
             background-color:#6495ed;
             color:white;
             font-weight:bold;
         }
         
         #about {
             background-color:#87ceeb;
             color:white;
             font-weight:bold;
         }
         
         #close {
             background-color:#6495ed;
             color:white;
             font-weight:bold;
         }
         
         .snap {
             background-color:#ffdab9;
         
         }
         
         .modal-dialog {
         width: fit-content !important;
         }
         ')
  ),
  
  
  # Application title
  #titlePanel("VIP Explorer"),
  fluidRow(style = "padding-top:10px; padding-left:5px;",
           column(width = 3,
                  fluidRow(style = "padding-bottom:2px; padding-left:5px;",
                           actionButton("help", "Help"),
                           actionButton("about", "About"),
                           actionButton("reset", "Reset"),
                           screenshotButton(label = "Save map", id = "classifiedMap", class = "snap")),
                  fluidRow(
                    style = "background-color:#e6e6fa; padding-bottom:2px; padding-left:5px;",
                    h4(" Complementarity:"),
                    p("Define Very Important Places (VIPs) by selecting complementarity type, clade and setting a threshold"),
                    column(width = 12,
                           fluidRow(
                             column(width = 7,
                                    selectInput("type",
                                                "Complementarity Type:",
                                                c("Species Richness", "Phylogenetic Diversity"),
                                                selected = "Phylogenetic Diversity")
                             ),
                             column(width = 5, selectInput("clade",
                                                           "Clade:",
                                                           cladeNames)
                             )
                           ),
                           sliderInput("complCut",
                                       "Percentile:",
                                       min = 0,
                                       max = 100,
                                       value = thr_comp_high,
                                       step = 5)
                    )
                  ),
                  fluidRow(
                    style = "background-color:#cae1ff; padding-left:5px;",
                    h4(" Climate Change Exposure:"),
                    p("Set climate impact threshold using RCP8.5 and 2050 data"),
                    column(width = 12,
                           sliderInput("climateCut",
                                       "Percentile:",
                                       min = 0,
                                       max = 100,
                                       value = thr_climate_high,
                                       step = 5))
                  ),
                  fluidRow(
                    style = "background-color:#e6e6fa; padding-bottom:2px; padding-left:5px;",
                    h4(" Loss of Habitat Condition:"),
                    p("Set habitat loss impact threshold using RCP8.5 and 2050 data"),
                    column(width = 12,
                           sliderInput("habitatLossCut",
                                       "Percentile:",
                                       min = 0,
                                       max = 100,
                                       value = thr_habitat_high,
                                       step = 5))
                  ),
                  fluidRow(
                    style = "background-color:#cae1ff; padding-bottom:2px; padding-left:5px;",
                    h4(" Apply thresholds to:"),
                    p("Defines 'wicked places' within VIPs by applying climate and habitat thresholds"),
                    column(width = 6,
                           selectInput("rcp",
                                       "RCP:",
                                       c("4.5", "8.5"),
                                       selected = "8.5")
                    ),
                    column(width = 6,
                           selectInput("time",
                                       "Future time:",
                                       c("2050", "2070"))
                    )
                  )
           ),
           
           column(width = 9,
                  fluidRow(htmlOutput("mapTitle"),
                           leafletOutput("classifiedMap", width = "100%", height = "768"))
           )
  )
)


#########################################
server <- function(input, output, session) {
  
  ############ Reset to default thresholds
  observeEvent(input$reset, {
    updateSelectInput(session, "type", selected = "Phylogenetic Diversity")
    updateSelectInput(session, "clade", selected = "Tetrapoda")
    updateSelectInput(session, "rcp", selected = "4.5")
    updateSelectInput(session, "time", selected = "2050")
    updateSliderInput(session, inputId = "climateCut", value = thr_climate_high)
    updateSliderInput(session, inputId = "habitatLossCut", value = thr_habitat_high)
    updateSliderInput(session, inputId = "complCut", value = thr_comp_high)
    
    proxy <- leafletProxy("classifiedMap")
    proxy %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  ############ Climate novelty
  climateVals <- reactive({
    if (input$rcp == "4.5")
    {
      climateVar <- paste0("clim_rcp45_", input$time)
    }
    else
    {
      climateVar <- paste0("clim_rcp85_", input$time)
    }
    
    return(d[, climateVar, drop = TRUE])
  })
  
  
  climate_percentiles <- reactive({
    return(quantile(climateVals(), probs = seq(0, 1, 0.05)))
  })
  
  
  climateChangeRas <- reactive({
    climateBins <- c(min(climateVals()), climate_percentiles()[paste0(as.character(input$climateCut), "%")],
                     max(climateVals()))
    newVals <- .bincode(climateVals(), climateBins)
    newRas <- baseRas
    newRas[cellInd] <- newVals
    
    return(newRas)
  })
  
  
  climateChangeJSON <- reactive({
    show_modal_spinner(spin = "flower", text = "Updating Climate Change layer", color = climateChangeColour)
    high_change <- climateChangeRas()
    high_change[high_change == 1] <- NA
    raster::writeRaster(high_change, f <- tempfile(fileext='.tif'))
    polygonize(f, shpName <- tempfile(fileext = '.shp'), ogr_format = 'ESRI Shapefile', connect8 = TRUE)
    
    shp_4326 <- sf::st_transform(sf::st_read(shpName, quiet = TRUE), 4326)
    
    # Remove polygons representing holes
    ii <- which(shp_4326$Value == -2147483648)
    if (length(ii) > 0) shp_4326 <- shp_4326[-ii, ]
    
    # Convert to geojson and return geojson object
    sf::st_write(shp_4326, geoJsonName <- tempfile(fileext = '.geojson'), quiet = TRUE)
    remove_modal_spinner()
    
    return(jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE))
  })
  
  
  ############ Habitat Loss
  habitatLossVals <- reactive({
    if (input$rcp == "4.5")
    {
      habitatLossVar <- paste0("landuseState_rcp45_", input$time, "_new")
    }
    else
    {
      habitatLossVar <- paste0("landuseState_rcp85_", input$time, "_new")
    }
    
    return(1 - d[, habitatLossVar, drop = TRUE])
  })
  
  
  habitatLoss_percentiles <- reactive({
    return(quantile(habitatLossVals(), probs = seq(0, 1, 0.05)))
  })
  
  
  habitatLossRas <- reactive({
    habitatLossBins <- c(min(habitatLossVals()), habitatLoss_percentiles()[paste0(as.character(input$habitatLossCut), "%")],
                         max(habitatLossVals()))
    newVals <- .bincode(habitatLossVals(), habitatLossBins)
    newRas <- baseRas
    newRas[cellInd] <- newVals
    return(newRas)
  })
  
  
  habitatLossJSON <- reactive({
    show_modal_spinner(spin = "flower", text = "Updating Habitat Loss layer", color = habitatLossColour)
    high_loss <- habitatLossRas()
    high_loss[high_loss == 1] <- NA
    raster::writeRaster(high_loss, f <- tempfile(fileext='.tif'))
    polygonize(f, shpName <- tempfile(fileext = '.shp'), ogr_format = 'ESRI Shapefile', connect8 = TRUE)
    
    shp_4326 <- sf::st_transform(sf::st_read(shpName, quiet = TRUE), 4326)
    
    # Remove polygons representing holes
    ii <- which(shp_4326$Value == -2147483648)
    if (length(ii) > 0) shp_4326 <- shp_4326[-ii, ]
    
    # Convert to geojson and return geojson object
    sf::st_write(shp_4326, geoJsonName <- tempfile(fileext = '.geojson'), quiet = TRUE)
    #sf::st_write(shp_4326, "~/Downloads/habLossPolygons.geojson")
    remove_modal_spinner()
    
    return(jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE))
  })
  
  
  ########### VIP = Complementarity
  complVals <- reactive({
    if (input$type == "Species Richness")
    {
      complVar <- paste0("compSD_", abbrevClade[input$clade])
    }
    else
    {
      complVar <- paste0("compPD_", abbrevClade[input$clade])
    }
    
    return(d[, complVar, drop = TRUE])
  })
  
  
  compl_percentiles <- reactive({
    return(quantile(complVals(), probs = seq(0, 1, 0.05)))
  })
  
  
  vipRas <- reactive({
    complBins <- c(min(complVals()), compl_percentiles()[paste0(as.character(input$complCut), "%")],
                   max(complVals()))
    newVals <- .bincode(complVals(), complBins)
    newRas <- baseRas
    newRas[cellInd] <- newVals
    
    return(newRas)
  })
  
  
  vipJSON <- reactive({
    show_modal_spinner(spin = "flower", text = "Updating VIPs layer", color = vipColour)
    high_comp <- vipRas()
    high_comp[high_comp == 1] <- NA
    raster::writeRaster(high_comp, f <- tempfile(fileext='.tif'))
    polygonize(f, shpName <- tempfile(fileext = '.shp'), ogr_format = 'ESRI Shapefile', connect8 = TRUE)
    
    shp_4326 <- sf::st_transform(sf::st_read(shpName, quiet = TRUE), 4326)
    
    # Remove polygons representing holes
    ii <- which(shp_4326$Value == -2147483648)
    if (length(ii) > 0) shp_4326 <- shp_4326[-ii, ]
    
    # Convert to geojson and return geojson object
    sf::st_write(shp_4326, geoJsonName <- tempfile(fileext = '.geojson'), quiet = TRUE)
    #sf::st_write(shp_4326, "~/Downloads/vipPolygons.geojson")
    remove_modal_spinner()
    
    return(jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE))
  })
  
  
  ########### Wicked places
  wickedPlacesJSON <- reactive({
    show_modal_spinner(spin = "flower", text = "Updating Wicked Places layer", color = wickedColour)
    vip <- vipRas()
    vip[vip == 1] <- NA
    climateChange <- climateChangeRas()
    climateChange[climateChange == 1] <- NA
    habitatLoss <- habitatLossRas()
    habitatLoss[habitatLoss == 1] <- NA
    wickedRas <- vip + climateChange + habitatLoss
    wickedRas[wickedRas[] == 6] <- 1
    
    # Squish the sf object into a JSON text stream
    raster::writeRaster(wickedRas, f <- tempfile(fileext='.tif'))
    polygonize(f, shpName <- tempfile(fileext = '.shp'), ogr_format = 'ESRI Shapefile', connect8 = TRUE)
    
    shp_4326 <- sf::st_transform(sf::st_read(shpName, quiet = TRUE), 4326)
    
    # Remove polygons representing holes
    ii <- which(shp_4326$Value == -2147483648)
    if (length(ii) > 0) shp_4326 <- shp_4326[-ii, ]
    
    # Convert to geojson and return geojson object
    sf::st_write(shp_4326, geoJsonName <- tempfile(fileext = '.geojson'), quiet = TRUE)
    remove_modal_spinner()
    
    return(jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE))
  })
  
  
  ################# Update leaflet display ####################
  # Method to trigger a single map update derived from Method 1 in a post by 'Pork Chop' here:
  # https://stackoverflow.com/questions/41960953/how-to-listen-for-more-than-one-event-expression-within-a-shiny-observeevent
  # last accessed 2021-10-08
  readyToUpdate <- reactive({
    list(vipJSON(), habitatLossJSON(), climateChangeJSON(), wickedPlacesJSON())
  })
  
  # Use a control object to place info and settings on map so they are part of a captured screenshot
  settingsInfo <- reactive({
    HTML(paste0("<h4 style = padding-top:0px;>",
                input$clade, ": Complementarity of ", input$type,
                ": RCP", input$rcp,
                ": Future time ", input$time, "</h4>"))
  })
  
  # Update classifiedMap with latest ensemble of polygons
  observeEvent(readyToUpdate(), {
    proxy <- leafletProxy("classifiedMap")
    
    proxy %>%
      clearGeoJSON() %>%
      addGeoJSON(vipJSON(), color = vipColour, group = "VIPs") %>%
      addGeoJSON(habitatLossJSON(), color = habitatLossColour, group = "Habitat loss") %>%
      addGeoJSON(climateChangeJSON(), color = climateChangeColour, group = "Climate change") %>%
      addGeoJSON(wickedPlacesJSON(), color = wickedColour, group = "Wicked places")
  })
  
  
  output$classifiedMap <- renderLeaflet({
    leaflet() %>%
      fitBounds(-180, 90, 180, -90) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addTiles(group = "OSM default") %>%
      addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap") %>%
      addProviderTiles("CartoDB.Voyager", group = "CartoDB.Voyager") %>%
      addProviderTiles("Stamen.Terrain", group = "Stamen.Terrain") %>%
      addLayersControl(baseGroups = c("OSM default", "Esri.WorldStreetMap", "CartoDB.Voyager", "Stamen.Terrain"),
                       overlayGroups = c("VIPs", "Wicked places", "Climate change", "Habitat loss"),
                       position = "topright",
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Climate change") %>%
      hideGroup("Habitat loss") %>%
      addLogo(img = "VIP_Explorer.png", src = "remote", url = "www/", position = "topleft", width = 200, height = 65) %>%
      addControl(settingsInfo(), position = "bottomleft") %>%
      addScaleBar(position = "bottomleft") %>%
      addLegend(position = "bottomright",
                values = c(1, 2, 3, 4),
                colors = c(vipColour, wickedColour, climateChangeColour, habitatLossColour),
                opacity = 0.7,
                labels = c("VIP", "Wicked place","Climate change", "Habitat loss"))
    
  })
  
  
  output$mapTitle <- renderUI(HTML(paste0("<h3 style = padding-left:25px; padding-top:0px;>",
                                          input$clade, ": Complementarity of ", input$type,
                                          ": RCP", input$rcp,
                                          ": Future time ", input$time, "</h3>")))
  
  
  #####################
  observeEvent(input$about, {
    showModal(modalDialog(
      tags$h2("About VIP Explorer"),
      tags$p("VIP Explorer is an R Shiny app based on Beaumont et al. (202?) which allows users to examine relationships between complementarity, climate and land use change beyond that possible in the source publication."),
      tags$p(tags$strong("Very Import Places (VIPs)"), "are those regions which currently support a high proportion of evolutionarily distinct terrestrial tetrapod lineages. VIPs are defined using a threshold applied to the distribution of complementarity values; complementarity is a measure of uniqueness or 'irreplaceability' and may be measured using species richness or phylogenetic diversity. A VIP region is one with complementarity values greater than a selected percentile (default 75%)."),
      tags$p("Within VIPs, the app allows users to consider the impact of two primary stressors on biodiversity, exposure to climate change and loss of habitat condition. By adjusting thresholds for 'high' impacts due to these two primary stressors, users can see the distribution of", tags$strong("wicked places"), "that is, those areas within VIPs facing the highest levels of threat from climate change and habitat modification. Like VIPs, thresholds for loss of habitat condition and exposure to climate change are calculated using global distributions of these measure, and a default percentile of 75% is used to estimate threshold values for each. Data for the two stressors differ from complementarity because they change through time and with selected climate change scenario, so we also must also select a scenario and future time to which we apply thresholds. To make valid comparisons across scenarios and times, thresholds for the two stressors are calculated on the global distribution of values under climate change scenario RCP8.5 and future time 2050, and then applied to the selected scenario and time."),
      tags$p("Things you may consider investigating using this app include:"),
      tags$ul(tags$li("Sensitivity of results to the thresholds selected to define VIPs and wicked places"),
              tags$li("Differences in VIP results across the major vertebrate clades.")),
      tags$p("If you use any of the results generated by this app, please acknowledge your use of the app and cite the original paper:"),
      tags$blockquote("Beaumont et al. (202?) etc"),
      tags$hr(style="height:2px;border-width:0;color:lightgray;background-color:lightgray"),
      tags$p(style="font-size:85%;", tags$strong("Acknowledgements:")),
      tags$p(style="font-size:85%;", "Climate data was sourced from the", tags$a("CHELSA climate data", href = "https://chelsa-climate.org"), "repository. See Karger et al. (2017) Climatologies at high resolution for the earth’s land surface areas. Scientific Data 4:170122."),
      tags$p(style="font-size:85%;", "Species distribution polygons used to compute the spatial distribution of species richness and phylogenetic complementarity were derived from data obtained under licence from:", tags$a("IUCN Red List data set", href = "https://www.iucnredlist.org/resources/spatial-data-download"), ",", tags$a("BirdLife International and the Handbook of the Birds of the World", href = "http://datazone.birdlife.org/species/requestdis"), "and the", tags$a("Global Assessment of Reptile Distributions (GARD)", href = "http://www.gardinitiative.org/data.html"), "."),
      tags$p(style="font-size:85%;", "Species taxonomy and nomenclature were resolved using", tags$a("GBIF", href = "https://www.gbif.org"), "accessed via the web interface and through the R package", tags$a("rgbif", href = "https://CRAN.R-project.org/package=rgbif"), "BirdLife International, and GARD."),
      tags$p(style="font-size:85%;", "Phylogenetic trees were assembled from data provided by the", tags$a("VertLife initiative", href = "https://vertlife.org"),  "combined with crocodile and turtle phylogenetic information from", tags$a("Colston et al. (2020)" , href = "https://bmcecolevol.biomedcentral.com/articles/10.1186/s12862-020-01642-3"), "."),
      tags$p(style="font-size:85%;", "Land use change data was computed using data from", tags$a("Land-Use Harmonization 2", href = "https://luh.umd.edu"), "."),
      footer = actionButton("close", "Close")
    ))
  })
  
  
  #####################
  observeEvent(input$help, {
    showModal(modalDialog(easyClose = TRUE, 
                          tags$h2(style="color:blue;", "VIP Explorer Help"),
                          tags$h3(style="color:blue;", "Concepts"),
                          tags$p("This is a short summary of the concepts and methods used to compute", tags$strong("Very Important Places (VIPs)"), "and", tags$strong("Wicked Places"), "(based on Beaumont et al. (202?)) to help you interpret the map generated by this app. Please see the original publication for details."),
                          tags$h4(style="color:MediumBlue;", "Complementarity"),
                          tags$p("Complementarity of a location measures the uniqueness of the assemblage of species at that location. It is a way of quantifying", tags$em("irreplaceability"), "which is the probability that a selected location has a totally unique assemblage of species present."),
                          tags$p("Complementarity may be calculated using either species richness or the amount of phylogenetic distinctness present in the species found at the location. Applying a threshold to the global distribution of complementarity values defines VIPs as those locations with complementarity", tags$strong("above"), "the threshold (default 75%), and which therefore should be the focus of effective conservation action."),
                          tags$h4(style="color:MediumBlue;", "Wicked places"),
                          tags$p("Areas within VIPs predicted to experience high levels of stress from loss of habitat condition", tags$strong("and"), "exposure to climate change are defined as", tags$em("wicked places"), "."),
                          tags$p("A", tags$em("high"), "level of stress is determined by applying thresholds to the histograms of values for each stressor. Locations with values of", tags$strong("Climate Change Exposure"), "and" ,tags$strong("Loss of Habitat Condition"), tags$em("above"), "the respective threshold are considered", tags$em("high"), "."),
                          tags$h3(style="color:blue;", "Using the app"),
                          tags$h4(style="color:MediumBlue;", "Complementarity"),
                          tags$p("Choose a combination of", tags$strong("Complementarity type"), "(either Species Richness or Phylogenetic Diversity) and", tags$strong("Clade"), "(e.g. Tetrapods, Mammals, Birds, etc.) from the pull-down selections. VIPs and Wicked places will be re-computed using the current threshold settings when a change is made."),
                          tags$p("Choose a threshold value for the three factors defining", tags$strong("VIPs"), "and", tags$strong("Wicked places"), "using the three sliders. Note that the sliders can only move in steps of 5 percentage points (a compromise between resolution and computing costs)."),
                          tags$p("The thresholds for", tags$strong("Climate Change Exposure"), "and" ,tags$strong("Loss of Habitat Condition"), "are based on percentiles of the distribution of each in 2050 under RCP8.5 conditions, and are applied to climate and landuse data for a selected combination of RCP scenario and future time."),
                          tags$p("Choose an RCP scenario (RCP4.5 or RCP8.5) and future time (2050 or 2070) to which the thresholds for", tags$strong("Climate Change Exposure"), "and" , tags$strong("Loss of Habitat Condition"), "will be applied to compute", tags$strong("Wicked Places"), "."),
                          tags$h4(style="color:MediumBlue;", "Choosing displayed map layers"),
                          tags$p("A layer control is present in the top right corner of the map display. You can choose a background or base map style. You can also choose which of the 4 computed layers are displayed. By default, only", tags$strong("VIP"), "and", tags$strong("Wicked places"), " layers are shown."),
                          tags$h4(style="color:MediumBlue;", "Other buttons"),
                          tags$p("The", tags$strong("Reset") ,"button returns all settings to their defaults and redraws the map to further experimentation."),
                          tags$p("The", tags$strong("Save map"), "button takes a screenshot of the current map as a PNG-formatted image; you can choose the file name and folder into which the file will be saved."),
                          footer = actionButton("close", "Close")
    ))
  })
  
  
  # Observer to respond to on-click event on the 'Close' button in a modal dialog
  # Inspired by answer posted by 'Shree' on
  # https://stackoverflow.com/questions/57484835/override-dismiss-button-in-r-shiny-modal-dialogue
  # last accessed 2021-10-21
  observeEvent(input$close, {
    removeModal()
  })
  
  
  # Observer to stop the app on closing of browser or tab
  observe({
    # If input$quit is unset (NULL) do nothing; if it's anything else, quit
    # by invoking stopApp()
    if (is.null(input$quit)) return()
    
    stopApp()
  }) 
  
}


#########################################
shinyApp(ui = ui, server = server)
