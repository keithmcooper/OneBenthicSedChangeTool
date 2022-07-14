###########################################################################################
####                          ONE BENTHIC SEDIMENT CHANGE TOOL                         ####
###########################################################################################

## Load packages
library(shiny)
library(mapview)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(sf)
library (RPostgres)
library(DBI)
library(plyr)
library(dplyr)
library(pool)
library(sp)
library(RPostgreSQL)
library(glue)
library(postGIStools)
library(reshape2)
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(stringr)
library(vegan)
library(config)
#library(geojsonio)

## For problem solving
#setwd("C:/Users/kmc00/OneDrive - CEFAS/working")
#write.csv(object,'filename.csv', row.names=F)
#__________________________________________________________________________________________
#### CODE TO SOLVE ERROR: Warning: Error in s2_geography_from_wkb: Evaluation error: Found 1 feature with invalid spherical geometry. [116] Loop 0 is not valid: Edge 8 is degenerate (duplicate vertex). ####
## Solution here: https://github.com/r-spatial/sf/issues/1762

sf_use_s2(FALSE)
#__________________________________________________________________________________________
#### CODE TO SOLVE ERROR: Missing dbQuoteLiteral methods for pool' ####

##https://github.com/rstudio/pool/issues/96
#' @importMethodsFrom DBI dbQuoteLiteral
#' @importFrom pool poolCheckout poolReturn
#' @export
setMethod("dbQuoteLiteral", c("Pool", "ANY"),
          function(conn, x, ...) {
            # As of 2020-05-07, this is necessary due to an incompatiblity
            # between packages `pool` (v 0.1.4.3) and `glue` (v >= 1.3.2).
            # In v1.3.2, `glue` started using `dbQuoteLiteral`, which is
            # currently not offered by `pool`, so creating it here.
            connection <- pool::poolCheckout(conn)
            on.exit(pool::poolReturn(connection))
            DBI::dbQuoteLiteral(connection, x, ...)
          }
)
#__________________________________________________________________________________________
#### CREATE A CONNECTION TO OneBenthic LIVE ####
Sys.setenv(R_CONFIG_ACTIVE = "one_benthic")

dw <- config::get()

pool <- dbPool(drv = dbDriver(dw$driver),
               dbname = dw$database,
               host = dw$server,
               port =  dw$port,
               user = dw$uid,
               password = dw$pwd)
#__________________________________________________________________________________________
#### GET POINT SAMPLE METADATA FOR INITIAL SELECTION CHOICES ####

## SQL query to OneBenthic
data <- dbGetQuery(pool, 
                   "SELECT
s.samplecode,
su.surveyname,
s.samplelong,
s.samplelat,
su.datapubliclyavailable


FROM
samples.sample as s
inner join associations.surveysample as susa on susa.sample_samplecode=s.samplecode
inner join associations.survey as su on su.surveyname = susa.survey_surveyname
WHERE su.datapubliclyavailable = TRUE
;")
#WHERE st.stationgroup = 'RSMP'

## Drop down choices for surveys
surveys <- sort(unique(data$surveyname))

## Data for mapping
names(data)
points <- unique(data[,c(1:4)])
colnames(points)[1] <- "Samplecode"
colnames(points)[2] <- "Survey"
colnames(points)[3] <- "Longitude"
colnames(points)[4] <- "Latitude"

#__________________________________________________________________________________________
#### SURVEY ARRAYS QUERY (REQUIRED FOR SUB-REGION CHOICES)
arrays <- dbGetQuery(pool, 
                     "SELECT DISTINCT
stationsubgroup1

FROM
associations.station
                   where stationgroup = 'RSMP' ;")
#__________________________________________________________________________________________
## SPATIAL DATA ####

## Queries
ref <- st_read(pool, query = "SELECT box as area,geom FROM spatial.ref_box_all;")
siz<- st_read(pool, query = "SELECT distinct area_numbe as area,geom FROM ap_marine_aggregate.extraction_areas_siz 
where droped = false;")
piz <- st_read(pool, query = "SELECT distinct area_numbe as area,geom FROM ap_marine_aggregate.extraction_areas where droped = false;")
randd <- st_read(pool, query = "SELECT area,geom,sector,treatment FROM spatial.r_and_d_polygons;")
dummy <- st_read(pool, query = "SELECT area,geom,sector,treatment FROM spatial.dummy_treatment_boxes;")# location on land
noratl <- st_read(pool, query = "SELECT area,geom,sector,treatment FROM spatial.north_atlantic;")# North Atlantic box

## Set CRS
st_crs(piz)<- 4326
st_crs(siz)<- 4326
st_crs(ref)<- 4326
st_crs(randd)<- 4326
st_crs(dummy)<- 4326
st_crs(noratl)<- 4326
#__________________________________________________________________________________________

#### GET SPATIAL LAYERS VIA API (AGG, OWF, WAVE, TIDE) ####

## 29/04/2020 Stopped using api as addresses kept changing

#https://opendata.arcgis.com/datasets/4da955de094e475d8d902ee446e38d58_0.geojson

## Bring in data via API
#agg <- readLines("https://opendata.arcgis.com/datasets/ced5788f014546b0b571e8d29b021166_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#owf <- readLines("https://opendata.arcgis.com/datasets/4da955de094e475d8d902ee446e38d58_0.geojson")%>% paste(collapse = "\n") %>% geojson_sf()
#owf_cab<- readLines("https://opendata.arcgis.com/datasets/c42f90c49f0d44f0997eaa52d692be3d_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#wave <- readLines("https://opendata.arcgis.com/datasets/d9f9dbc0e29b410c87ba544f6082a0d0_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#wave_cab <- readLines("https://opendata.arcgis.com/datasets/00ab10c847fb42a9bef8f0f4644c41ac_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#tidal <- readLines("https://opendata.arcgis.com/datasets/db94124b152641a992d4e8dfa14d59f2_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#tidal_cab <- readLines("https://opendata.arcgis.com/datasets/3e5203ce7daa4ae08690699925668f46_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#R4_chara <- readLines("https://opendata.arcgis.com/datasets/c0e61d8972e4438ab1c39304b7f28608_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#R4_bid <- readLines("https://opendata.arcgis.com/datasets/54dce8a263324a85b36523e31fff20cc_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()
#oga <- readLines("https://opendata.arcgis.com/datasets/3c950a2c8186438899f99ced733dd947_0.geojson") %>% paste(collapse = "\n") %>% geojson_sf()

## Check classof spatial objects
#class(agg) #"sf"         "data.frame"
#class(owf)#"sf"         "data.frame"
#class(owf_cab)#"sf"         "data.frame"
#class(wave)#"sf"         "data.frame"
#class(wave_cab)#"sf"         "data.frame"
#class(tidal)#"sf"         "data.frame"
#class(tidal_cab)#"sf"         "data.frame"
#class(R4_chara)#"sf"         "data.frame"
#class(R4_bid)#"sf"         "data.frame"

## Check CRS of spatial objects
#st_crs(agg) #EPSG: 4326 
#st_crs(owf) #EPSG: 4326 
#st_crs(owf_cab) #EPSG: 4326 
#st_crs(wave) #EPSG: 4326 
#st_crs(wave_cab) #EPSG: 4326 
#st_crs(tidal) #EPSG: 4326 
#st_crs(tidal_cab) #EPSG: 4326 
#st_crs(R4_chara) #EPSG: 4326 
#st_crs(R4_bid) #EPSG: 4326 

#__________________________________________________________________________________________
#### GET SPATIAL LAYERS FROM OneBenthic - WHERE NO API AVAILABLE (MPAS, O&G, DISP) ####

## Bring in spatial data layers
mcz <-  st_read(pool, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'MCZ - Secretary of State';")
sac <-  st_read(pool, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'SAC'or site_statu = 'cSAC';")
ncmpa <-  st_read(pool, query = "SELECT * FROM spatial.c20190905_offshorempas_wgs84 WHERE site_statu = 'NCMPA';")
disp  <-  st_read(pool, query = "SELECT * FROM spatial.disposalSiteJan2020;")
agg <- st_read(pool, query = "SELECT * FROM ap_marine_aggregate.extraction_areas;")
oga <- st_read(pool, query = "SELECT * FROM spatial.oga_licences_wgs84;")
owf <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_site_agreements_england_wales__ni__the_crown_esta;")
owf_cab <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_cable_agreements_england_wales__ni_the_crown_esta;")
wave <- st_read(pool, query = "SELECT * FROM spatial.offshore_wave_site_agreements_england_wales__ni_the_crown_estat;")
wave_cab <- st_read(pool, query = "SELECT * FROM spatial.offshore_wave_cable_agreements_england_wales__ni_the_crown_esta;")
tidal <- st_read(pool, query = "SELECT * FROM spatial.offshore_tidal_stream_site_agreements_england_wales__ni_the_cro;")
tidal_cab <- st_read(pool, query = "SELECT * FROM spatial.offshore_tidal_stream_cable_agreements_england_wales__ni_the_cr;")
R4_chara <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_leasing_round_4_characterisation_areas_england_wa;")
R4_bid <- st_read(pool, query = "SELECT * FROM spatial.offshore_wind_leasing_round_4_bidding_areas_england_wales_and_n;")

## Create duplicate layers as these will me manipulated later
## Reduce sac to 2 cols (region, area, geom) - same as piz, siz, ref etc
sac2 <- sac[,c(3,15)]
colnames(sac2) <- c("area","geom")

mcz2 <- mcz[,c(3,15)]
colnames(mcz2) <- c("area","geom")

owf2 <- owf[,c(3,10)]
colnames(owf2) <- c("area","geom")

## Check class of objects
#class(mcz)#[1] "sf"         "data.frame"
#class(sac)#[1] "sf"         "data.frame"
#class(ncmpa)#[1] "sf"         "data.frame"
#class(oga)#[1] "sf"         "data.frame"
#class(disp)#[1] "sf"         "data.frame"
#class(agg) #[1] "sf"         "data.frame"
#class(owf)#[1] "sf"         "data.frame"
#class(owf_cab)#[1] "sf"         "data.frame"
#class(wave)#[1] "sf"         "data.frame"
#class(wave_cab)#[1] "sf"         "data.frame"
#class(tidal)#[1] "sf"         "data.frame"
#class(tidal_cab)#[1] "sf"         "data.frame"
#class(R4_chara)#[1] "sf"         "data.frame"
#class(R4_bid)#[1] "sf"         "data.frame"

## Check CRS
#st_crs(mcz)#Coordinate Reference System: NA
#st_crs(sac)#Coordinate Reference System: NA
#st_crs(ncmpa)#Coordinate Reference System: NA
#st_crs(oga)#Coordinate Reference System: NA
#st_crs(disp)#Coordinate Reference System: NA
#st_crs(agg) # 4326
#st_crs(owf)#Coordinate Reference System: NA
#st_crs(owf_cab)#Coordinate Reference System: NA
#st_crs(wave)#Coordinate Reference System: NA
#st_crs(wave_cab)#Coordinate Reference System: NA
#st_crs(tidal)#Coordinate Reference System: NA
#st_crs(tidal_cab)#Coordinate Reference System: NA
#st_crs(R4_chara)#Coordinate Reference System: NA
#st_crs(R4_bid)#Coordinate Reference System: NA

## Set CRS where necessary
st_crs(mcz) <- 4326
st_crs(mcz2) <- 4326
st_crs(sac) <- 4326
st_crs(sac2) <- 4326
st_crs(ncmpa) <- 4326
st_crs(oga) <- 4326
st_crs(disp) <- 4326
st_crs(owf) <- 4326
st_crs(owf2) <- 4326
st_crs(owf_cab) <- 4326
st_crs(wave) <- 4326
st_crs(wave_cab) <- 4326
st_crs(tidal) <- 4326
st_crs(tidal_cab) <- 4326
st_crs(R4_chara) <- 4326
st_crs(R4_bid) <- 4326
#__________________________________________________________________________________________
#### RBIND SPATIAL DATA INTO A SINGLE DF ####

## First add a column for 'treatment'
piz$sector <- "AGG"
siz$sector <- "AGG"
ref$sector <- "AGG"
mcz2$sector <- "MCZ"
sac2$sector <- "SAC"
owf2$sector <- "OWF"
piz$treatment <- "PIZ"
siz$treatment <- "SIZ"
ref$treatment <- "REF"
mcz2$treatment <- "PIZ"
sac2$treatment <- "PIZ"
owf2$treatment <- "PIZ"

## Join
spatial.data <- rbind.fill(piz,siz,ref,mcz2,sac2,randd,dummy,owf2,noratl)

## Covert back to sf object
spatial.data <- st_as_sf(x = spatial.data)
#__________________________________________________________________________________________
#### MAP LAYERS SOURCE INFO ####

layer <- data.frame("Layer"=c("owf","owf_cab","R4_chara","R4_bid","agg","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa","randd","samples"),
                    
                    "Detail"=c("Offshore Wind Site Agreements (England, Wales & NI) - The Crown Estate",
                               "Offshore Wind Cable Agreements (England, Wales & NI), The Crown Estate",
                               
                               "Offshore Wind Leasing Round 4 Characterisation Areas (England, Wales and NI) - The Crown Estate",
                               "Offshore Wind Leasing Round 4 Bidding Areas (England, Wales and NI) - The Crown Estate",
                               "Offshore Minerals Aggregates Site Agreements (England, Wales & NI), The Crown Estate",
                               "UK Disposal Site Layer, Cefas",
                               "Offshore Wave Site Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Wave Cable Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Tidal Stream Site Agreements (England, Wales & NI), The Crown Estate",
                               "Offshore Tidal Stream Cable Agreements (England, Wales & NI), The Crown Estate",
                               "OGA Licences WGS84, Oil and Gas Authority","Marine Conservation Zones (MCZ)","Special Area of Conservation","Nature Conservation Marine Protected Areas (Scotland)",
                               "Polygons for R & D sites",
                               "Location of all grab/core samples from the OneBenthic database"),
                    "Link"=c("https://opendata.arcgis.com/datasets/b58c1254c04642db80bfd4d8f34a1079_0.geojson",
                             "https://opendata.arcgis.com/datasets/c42f90c49f0d44f0997eaa52d692be3d_0.geojson",
                             "https://opendata.arcgis.com/datasets/c0e61d8972e4438ab1c39304b7f28608_0.geojson",
                             "https://opendata.arcgis.com/datasets/54dce8a263324a85b36523e31fff20cc_0.geojson",
                             "https://opendata.arcgis.com/datasets/d734d753d04649e2a7e1c64b820a5df9_0.geojson",
                             "http://data.cefas.co.uk/#/View/407",
                             "https://opendata.arcgis.com/datasets/d9f9dbc0e29b410c87ba544f6082a0d0_0.geojson",
                             "https://opendata.arcgis.com/datasets/00ab10c847fb42a9bef8f0f4644c41ac_0.geojson",
                             "https://opendata.arcgis.com/datasets/db94124b152641a992d4e8dfa14d59f2_0.geojson",
                             "https://opendata.arcgis.com/datasets/3e5203ce7daa4ae08690699925668f46_0.geojson",
                             "https://opendata.arcgis.com/datasets/3c950a2c8186438899f99ced733dd947_0.geojson",
                             "https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp",
                             "https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp",
                             "https://hub.jncc.gov.uk/assets/ade43f34-54d6-4084-b66a-64f0b4a5ef27/c20190905_OffshoreMPAs_WGS84.shp",
                             "",
                             "https://openscience.cefas.co.uk/obdash/"))


layer$Link<- paste0("<a href='",layer$Link,"'>",layer$Link,"</a>")

#__________________________________________________________________________________________
#### USER INTERFACE ####

ui <- fluidPage(
  #### TITLE PANEL ####
  titlePanel(title=div(img(src="onebenthic.gif",tags$b(" OneBenthic"),"Sediment Change Tool",height = 70, width = 170),style='background-color:#B4C7E7;padding-right: 50px')),
  #__________________________________________________________________________________________  
  fluidRow(
    #### SELECTIONS ####
    column(2,
           selectInput(inputId="programmeInput",selected = NULL, multiple = F,h4(br(),"Choose sector",style="color:#808080"),choices =c("SAC","AGG","MCZ","R&D","OWF","NONE")),
           selectInput(inputId="baselineInput", multiple = T,h4(br(),"Select Baseline survey(s)",style="color:#808080"),choices =surveys),
           selectInput(inputId="monitoringInput", multiple = T,h4("Select Monitoring survey(s)",style="color:#808080"),choices =surveys),
           ## Only show array choice where sector is 'AGG'
           conditionalPanel(condition = "input.programmeInput == 'AGG'",
                            selectInput(inputId="arrayInput", multiple = F,h4(br()," RSMP array",style="color:#808080"),choices =arrays),
                            h4(br(),"Use paired samples only",style="color:#808080"),checkboxInput(inputId = "paired",
                                                                                                   label = "",
                                                                                                   value = FALSE)
           ),br()#selectInput
    ),#close conditional panel
    #__________________________________________________________________________________________
    #### MAP ####
    column(4,
           leafletOutput("map",width = "100%", height=850),style='border-left: 1px solid grey'),
    #__________________________________________________________________________________________
    #### TABS ####
    column(6,style='border-left: 1px solid grey',
           tabsetPanel(
             #__________________________________________________________________________________________
             #### DATA TAB ####
             tabPanel(tags$b("Data"),
                      tabsetPanel(
                        tabPanel("Baseline",br(),div(DT::dataTableOutput("widebas"),style = 'font-size:85%')),
                        tabPanel("Monitoring",br(),div(DT::dataTableOutput("widemon"),style = 'font-size:85%')),
                        tabPanel("All",br(),div(DT::dataTableOutput("wideall"),style = 'font-size:85%')),
                        tabPanel("All+Treatment",br(),div(DT::dataTableOutput("refst"),style = 'font-size:85%'))
                      )# close tabsetPanel
             ),#close tabPanel "Data"
             #__________________________________________________________________________________________
             #### RESULTS TAB ####
             tabPanel(tags$b("Results"),
                      tabsetPanel(
                        #__________________________________________________________________________________________
                        #### RESULTS TAB: SITE ####
                        tabPanel("Site",
                                 tabsetPanel(
                                   tabPanel(em("Line Plots"),plotOutput(outputId = "site.line", height = 740)),
                                   tabPanel(em("Means"),br(),div(DT::dataTableOutput("sedsumsite"),style = 'font-size:85%')),
                                   tabPanel(em("nMDS 2D"),plotOutput(outputId = "nmds.site.piz", height = 740)),
                                   tabPanel(em("nMDS 3D"),plotlyOutput(outputId = "nmds3dsite", height = 740)),
                                   tabPanel(em("Anosim"),br(),div(DT::dataTableOutput("anosim.site"),style = 'font-size:85%'),h4(br(),style="color:#808080"),actionButton("update","Show results in map")),
                                   tabPanel(em("Simper"),br(),div(DT::dataTableOutput("simpertab.site"),style = 'font-size:85%')),
                                   tabPanel(em("Change"),plotOutput(outputId = "change.site", height = 740))
                                 )#close tabsetPanel
                        ),#close tabPanel "Site"
                        #__________________________________________________________________________________________
                        #### RESULTS TAB: SUB REGION ####
                        tabPanel("Sub-region",
                                 tabsetPanel(
                                   tabPanel(em("Line Plots"),value= 'subregionTab',plotOutput(outputId = "subreg.line", height = 740)),
                                   tabPanel(em("Means"),value= 'subregionTab',br(),div(DT::dataTableOutput("sedsumsubreg"),style = 'font-size:85%')),
                                   tabPanel(em("nMDS 2D"),value= 'subregionTab',plotOutput(outputId = "nmds.subreg", height = 740)),
                                   
                                   tabPanel(em("Anosim"),value= 'subregionTab',br(),div(DT::dataTableOutput("anosim.subreg"),style = 'font-size:85%')),
                                   tabPanel(em("Simper"),value= 'subregionTab',br(),div(DT::dataTableOutput("simpertab.subreg"),style = 'font-size:85%')),
                                   tabPanel(em("Change"),value= 'subregionTab',plotOutput(outputId = "change.subreg", height = 740)),id ="tabselected2"
                                 )#close tabsetPanel
                        ),#close tabPanel "Sub-region"
                        #__________________________________________________________________________________________
                        #### RESULTS TAB: REGION ####
                        tabPanel("Region",
                                 tabsetPanel(
                                   tabPanel(em("Line Plots"),value = 'regionTab',br(),plotOutput(outputId = "reg.line", height = 350)),
                                   tabPanel(em("Means"),br(),value = 'regionTab',div(DT::dataTableOutput("sedsumreg"),style = 'font-size:85%')),
                                   tabPanel(em("nMDS 2D"),value = 'regionTab',br(),plotOutput(outputId = "nmds", height = 350)),#750
                                   tabPanel(em("Anosim"),value = 'regionTab',br(),div(DT::dataTableOutput("anosim"),style = 'font-size:85%')),
                                   tabPanel(em("Simper"),value = 'regionTab',br(),div(DT::dataTableOutput("simpertab.reg"),style = 'font-size:85%')),
                                   tabPanel(em("Change"),value = 'regionTab',br(),plotOutput(outputId = "change.reg", height = 740)), id ="tabselected"
                                 )#close tabsetPanel
                        )#close tabPanel "Region"
                        #__________________________________________________________________________________________
                      )#close tabsetPanel
             ),#close tabPanel "Results"
             #__________________________________________________________________________________________
             #### ABOUT TAB ####
             tabPanel(tags$b("About"),
                      tabsetPanel(
                        #__________________________________________________________________________________________
                        ### INSTRUCTIONS TAB ####
                        tabPanel("Instructions",
                                 h4("App purpose"),"This app is designed to look for evidence of statistically significant changes in sediment composition between baseline and monitoring surveys.The app can be used for monitoring of marine licences (e.g. marine aggregate, offshore wind, renewables), MPAs or R&D sites. Note that the",tags$a(href="https://openscience.cefas.co.uk/matool_mhtest/", "OneBenthic M-Test Tool")," can help determine whether sediment changes are likely to be ecologically significant.",br(),
                                 h4("Data"),"The app uses sediment particle size data classified according to the Wentworth scale (i.e. percentages of silt/clay - SC, fine sand - fS, medium sand - mS, coarse sand - cS, fine gravel - fG, medium gravel - mG, coarse gravel - cG). Data used in the app come from the ",tags$a(href="https://openscience.cefas.co.uk/obdash/", "OneBenthic database."),"The app also uses spatial data polygons (see 'Map Layers' tab) to automatically assign samples to treatment groups (e.g. reference, licensed areas and secondary impact zones).",br(),h4("How it works"),"Select a sector and choose baseline and monitoring surveys for comparison. For the marine aggregates sector (AGG), users must also select an RSMP survey array and choose whether to use all or only paired sample data (i.e. stations with both a baseline and a monitoring sample). Selected samples are shown in the map. To find surveys for comparison, turn on sample locations (see map 'samples' checkbox) and use the 'Draw a rectangle' tool to highlight an area of interest. Results will appear in the 'Search' tab. Selected data and assigned treatment groups can be viewed under the 'Data' tab.","Data are analysed in various ways, with results available, where relevant, at different spatial scales (see 'Results' tab).",br(),br(),
                                 "The",tags$b(" Line Plots"),"tab shows the sediment composition of samples by treatment group. For clarity, individual samples are not shown in regional line plots.",br(),br(),
                                 "The",tags$b("Means")," tab includes a table for mean sediment composition by treatment group.",br(),br(),
                                 "The", tags$b("MDS")," tab shows a series of non-metric multidimensional scaling ordination (nMDS) plots based on Euclidean distance (untransformed data). Each dot represents a sample, with positions reflecting similarity/dis-similarity in terms of sediment composition. The stress value provides a measure of how well the 2-d plot represents the multidimensional data (stress values of <0.1 are considered good).",br(),br(),
                                 "The", tags$b("ANOSIM")," test looks for evidence of statistically significant differences between the two groups of samples (i.e. baseline vs monitoring). Test outputs include R and p-values. The R value indicates the size of the difference between the two groups, with 0 indicating no difference and 1 a large difference. A p-value of <0.05 indicates that results are statistically significant. Both R and p-values should be considered when interpreting test results. Note that where there are large numbers of samples it may be possible to find statistically significant, yet very small differences between the groups. It is therefore important to consider the effect size (R value). For this reason we generally consider differences to only be of interest where p<0.05 and R>0.1.","Anosim test outputs include a column for 'interpretation', following criteria in ",tags$a(href="https://www.researchgate.net/post/Which-R-value-is-considered-to-show-a-strong-difference-between-the-groups-in-ANOSIM/54ff36e4d2fd64c47b8b45a7/citation/download", "Goss-Souza (2015).")," Press button to see results displayed in the map. ANOSIM tests are performed using the R",tags$a(href="
https://CRAN.R-project.org/package=vegan", "vegan"),"package. Where ANOSIM finds a meaningful difference (i.e. p<0.05, R>0.1), relevant data are carried forward fora SIMPER test to identify which sediment fractions are responsible for the differences.",br(),br(),
                                 "The",tags$b("SIMPER")," test is carried out using the 'simper' function (vegan) based on Euclidean distances.","Finally, under the",tags$b("Change")," tab, a simple bar chart shows how mean sediment fractions has changed. These plots should be interpreted together with the SIMPER results. Note that analyses only run after clicking on each tab - please be patient.","You can change the R value cut-off for SIMPER analysis here:",numericInput("num", h4("",style="color:#808080"), value = 0.1, min = 0, max = 1, step=0.01) ,
                                 h4("Contact"),"This is a beta version of the app. Users should satisfy themselves that comparisons are valid and carefully check the data and results. No liability is accepted by the app developer. For help/advice using the app (or to provide feedback), please get in touch (keith.cooper@cefas.co.uk).",style = 'font-size:90%'),
                        #__________________________________________________________________________________________
                        #### MAP LAYERS TAB ###
                        tabPanel("Map Layers",br(),DT::dataTableOutput("activitytable"),style = 'font-size:85%'),
                        #__________________________________________________________________________________________
                        #### FUNDERS TAB ####
                        tabPanel("Funders",br(),tags$b("OneBenthic"),"apps are free to use but not to run. If you found the app useful then please consider joining existing funders to support the initiative. Thankyou!",br(),(img(src="logos.png",height = 375, width = 750)),style = 'font-size:90%')
                      )
             ),
             #__________________________________________________________________________________________
             #### SEARCH TAB ####
             tabPanel(tags$b("Search"),br(),div(DT::dataTableOutput("bbdata"),style = 'font-size:85%')))
    )#close tabsetPanel
    #__________________________________________________________________________________________   
    #### SUPRESS IRRELEVANT ERROR MESSAGE ON SAMPLE TAB ####
    ,tags$style(type="text/css",
                
                ".shiny-output-error { visibility: hidden; }",
                
                ".shiny-output-error:before { visibility: hidden; }")
    #__________________________________________________________________________________________
  )#column(6
)#fluidRow

#__________________________________________________________________________________________
#### SERVER FUNCTION ####

server <- function(input, output, session) {
  
  #__________________________________________________________________________________________
  #### REMOVE TAB (RESULTS/REGION) BASED ON SECTOR CHOICE ####
  observe({
    #req(input$programmeInput)
    if (input$programmeInput =="SAC"|input$programmeInput =="MCZ"|input$programmeInput =="R&D"|input$programmeInput =="OWF"|input$programmeInput =="NONE"){
      hideTab(inputId = "tabselected", target = 'regionTab')
      
    }
    else if(input$programmeInput =="AGG" ){
      
      showTab(inputId = "tabselected", target = 'regionTab')
      
    }
  })
  #__________________________________________________________________________________________
  #### REMOVE TAB (RESULTS/SUB-REGION) BASED ON SECTOR CHOICE ####
  observe({
    #req(input$programmeInput)
    if (input$programmeInput =="SAC"|input$programmeInput =="MCZ"|input$programmeInput =="R&D"|input$programmeInput =="OWF"|input$programmeInput =="NONE"){
      hideTab(inputId = "tabselected2", target = 'subregionTab')
    }
    #else if(input$programmeInput =="AGG"& input$arrayInput =="South Coast RSMP"){
    else if(input$programmeInput =="AGG"& input$arrayInput =="South Coast RSMP"|input$programmeInput =="AGG"& input$arrayInput =="Humber RSMP"){  
      showTab(inputId = "tabselected2", target = 'subregionTab')
    }
  })
  #__________________________________________________________________________________________ 
  #### SELECT BY SAMPLE: DEFINE AREA OF INTEREST USING BOUNDING BOX ####
  
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    print(st_bbox(poly))
    
  })
  
  ## Bounding box
  bbminlat <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[2]]})# min lat
  bbminlon <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[1]][[1]]})# min long
  bbmaxlat <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[2]][[2]]})# max lat
  bbmaxlon <- reactive({input$map_draw_new_feature$geometry$coordinates[[1]][[3]][[1]]})# max long
  
  
  #__________________________________________________________________________________________ 
  #### REACTIVE OBJECT: SAMPLE METADATA FOR SELECTED SAMPLE(S) - SAMPLES WITHIN BOUNDING BOX ####
  
  
  bbcoord <- reactive({
    
    bbcoord2 <- sqlInterpolate(ANSI(),
                               "SELECT
s.samplecode,
sst.station_stationcode,
ss.survey_surveyname,
s.samplelong,
s.samplelat,
s.gear_gearcode,
s.date,
su.datapubliclyavailable

FROM
samples.sample as s
inner join associations.surveysample as ss on ss.sample_samplecode=s.samplecode
inner join associations.survey as su on su.surveyname = ss.survey_surveyname
left join associations.samplestation as sst on s.samplecode=sst.sample_samplecode

WHERE 
s.samplelat > ?minlat
AND s.samplelat < ?maxlat
AND s.samplelong > ?minlong
AND s.samplelong < ?maxlong
ORDER by ss.survey_surveyname, s.samplecode;",
                               minlat = bbminlat(),
                               maxlat = bbmaxlat(),
                               minlong = bbminlon(),
                               maxlong = bbmaxlon())
    
    bbcoord3 <- as.data.frame(dbGetQuery(pool, bbcoord2))
    
    return(bbcoord3)
    
  })
  #__________________________________________________________________________________________ 
  #### OUTPUT  METADATA TABLE FOR SELECTED SAMPLE(S) - WITHIN BOUNDING BOX ####
  
  output$bbdata <- DT::renderDataTable({
    bbdat2 <- unique(bbcoord()[,1:8])
    DT::datatable(bbdat2,colnames=c("Sample Code","Station Code","Survey Name","Long","Lat","Gear","Date","Data available"),class = "display nowrap",options = list(pageLength = 19))
  })
  #__________________________________________________________________________________________
  #### SELECT SPATIAL LAYERS ####
  
  ## PIZ  
  test.piz <- reactive({
    
    if (input$programmeInput == 'AGG'){
      piz <- spatial.data[which(spatial.data$sector=='AGG' & spatial.data$treatment=='PIZ'),]
      piz <- piz%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'MCZ'){
      piz <- spatial.data[which(spatial.data$sector=='MCZ' & spatial.data$treatment=='PIZ'),]
      piz <- piz%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'SAC'){
      piz <- spatial.data[which(spatial.data$sector=='SAC' & spatial.data$treatment=='PIZ'),]
      piz <- piz%>% select(- c(treatment, sector))
    }else if(input$programmeInput == 'R&D'){
      piz <- spatial.data[which(spatial.data$sector=='R&D' & spatial.data$treatment=='PIZ'),] 
      piz <- piz%>% select(- c(treatment, sector))
    }else if(input$programmeInput == 'OWF'){
      piz <- spatial.data[which(spatial.data$sector=='OWF' & spatial.data$treatment=='PIZ'),] 
      piz <- piz%>% select(- c(treatment, sector))
    }else if(input$programmeInput == 'NONE'){
      piz <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),] 
      piz <- piz%>% select(- c(treatment, sector))
    }
    return(piz)
  })
  
  ## SIZ
  test.siz <- reactive({
    if (input$programmeInput == 'AGG'){
      siz <- spatial.data[which(spatial.data$sector=='AGG' & spatial.data$treatment=='SIZ'),]
      siz <- siz%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'MCZ'){
      siz <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),]
      siz <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'SAC'){
      siz <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),]
      siz <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'R&D'){
      siz <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),]
      siz <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'OWF'){
      siz <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),]
      siz <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'NONE'){
      siz <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),] 
      siz <- ref%>% select(- c(treatment, sector))
    }
    return(siz)
  })
  
  ## REF 
  test.ref <- reactive({
    if (input$programmeInput == 'AGG'){
      ref <- spatial.data[which(spatial.data$sector=='AGG' & spatial.data$treatment=='REF'),]
      ref <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'MCZ'){
      ref <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),]
      ref <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'SAC'){
      ref <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),]
      ref <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'R&D'){
      ref <- spatial.data[which(spatial.data$sector=='R&D' & spatial.data$treatment=='REF'),] 
      ref <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'OWF'){
      ref <- spatial.data[which(spatial.data$sector=='dummy' & spatial.data$treatment=='dummy'),] 
      ref <- ref%>% select(- c(treatment, sector))
    }else if (input$programmeInput == 'NONE'){
      ref <- spatial.data[which(spatial.data$sector=='NONE' & spatial.data$treatment=='REF'),] 
      ref <- ref%>% select(- c(treatment, sector))
    }
    return(ref)
  })
  #__________________________________________________________________________________________
  #### TABLE FOR ACTIVITY LAYERS ####
  
  output$activitytable <- DT::renderDataTable(
    DT::datatable(layer, options = list(pageLength = 9),escape=FALSE)
  )
  #__________________________________________________________________________________________  
  #### MAP ####
  
  output$map <- renderLeaflet({
    
    ## Basic map
    leaflet() %>%
      
      #addProviderTiles(providers$Esri.OceanBasemap,options = providerTileOptions(noWrap = TRUE))%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,options = providerTileOptions(noWrap = TRUE))%>%
      addMouseCoordinates()%>%addLegend(
        position = "topright",
        colors = c("blue","#00CCCC"),# NB have to use hex cols
        labels = c("Baseline","Monitoring"),        
        opacity = 5,
        title = "Samples"
      )%>%
      addPolygons(data=owf,color = "#444444", weight = 1, smoothFactor = 0.5,group = "owf",popup = paste0("<b>Name: </b>", owf$name_prop, "<br>","<b>Status: </b>", owf$inf_status))%>%
      addPolygons(data=owf_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "owf_cab",popup = paste0("<b>Name: </b>", owf_cab$name_prop, "<br>","<b>Status: </b>", owf_cab$infra_stat))%>%
      addPolygons(data=R4_chara,color = "#444444", weight = 1, smoothFactor = 0.5,group = "R4_chara",popup = paste0("<b>Name: </b>", R4_chara$name))%>%
      addPolygons(data=R4_bid,color = "#444444", weight = 1, smoothFactor = 0.5,group = "R4_bid",popup = paste0("<b>Name: </b>", R4_bid$name, "<br>","<b>Status: </b>", R4_bid$bidding_ar))%>%
      addPolygons(data=siz,color = "#444444", fillOpacity = 0, weight = 1, smoothFactor = 0.5,group = "agg (SIZ)",popup = paste0("<b>SIZ Area: </b>", siz$area))%>%
      addPolygons(data=ref,color = "#444444", fillOpacity = 0,weight = 1, smoothFactor = 0.5,group = "ref",popup = paste0("<b>Name: </b>", ref$area))%>%
      addPolygons(data=piz,color = "#444444", fillOpacity = 0,weight = 2, smoothFactor = 0.5,group = "agg (PIZ)",popup = paste0("<b>PIZ Area: </b>", piz$area))%>%
      addPolygons(data=disp,color = "#444444", weight = 1, smoothFactor = 0.5,group = "disp",popup = paste0("<b>Name: </b>", disp$name_, "<br>","<b>Number: </b>", disp$site_))%>%
      addPolygons(data=wave,color = "#444444", weight = 1, smoothFactor = 0.5,group = "wave",popup = paste0("<b>Name: </b>", wave$name_prop, "<br>","<b>Status: </b>", wave$inf_status))%>%
      addPolygons(data=wave_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "wave_cab",popup = paste0("<b>Name: </b>", wave_cab$name_prop, "<br>","<b>Status: </b>", wave_cab$infra_stat))%>%
      addPolygons(data=tidal,color = "#444444", weight = 1, smoothFactor = 0.5,group = "tidal",popup = paste0("<b>Name: </b>", tidal$name_prop, "<br>","<b>Status: </b>", tidal$inf_statUS))%>%
      addPolygons(data=tidal_cab,color = "#444444", weight = 1, smoothFactor = 0.5,group = "tidal_cab",popup = paste0("<b>Name: </b>", tidal_cab$name_prop, "<br>","<b>Status: </b>", tidal_cab$infra_stat))%>%
      addPolygons(data=mcz,color = "#444444", weight = 1, smoothFactor = 0.5,group = "mcz",popup = paste0("<b>Name: </b>", mcz$site_name))%>%
      addPolygons(data=sac,color = "#444444", weight = 1, smoothFactor = 0.5,group = "sac",popup = paste0("<b>Name: </b>", sac$site_name))%>%
      addPolygons(data=ncmpa,color = "#444444", weight = 1, smoothFactor = 0.5,group = "ncmpa",popup = paste0("<b>Name: </b>", ncmpa$site_name))%>%
      addPolygons(data=randd,color = "#444444", weight = 5, smoothFactor = 0.5,group = "randd")%>%
      addPolygons(data=oga,color = "#444444", weight = 1, smoothFactor = 0.5,group = "oga",popup = paste0("<b>Number: </b>", oga$licref, "<br>","<b>Organisation: </b>", oga$licorggrp))%>%
      addCircleMarkers(data = points,~Longitude,~Latitude,group = "samples",popup = ~paste("<b>Sample Code: </b>",Samplecode, "<br>","<b>Survey: </b>", Survey),radius =0.9,color = "grey",stroke = FALSE, fillOpacity = 0.5)%>%
      addLayersControl(
        overlayGroups = c("owf","owf_cab","R4_chara","R4_bid","agg (SIZ)","agg (PIZ)","ref","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa","randd","samples"),options = layersControlOptions(collapsed = FALSE))%>%hideGroup(c("owf","owf_cab","R4_chara","R4_bid","agg (SIZ)","agg (PIZ)","ref","disp","wave","wave_cab","tidal","tidal_cab","oga","mcz","sac","ncmpa","randd","samples"))%>%
      addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,circleMarkerOptions = F, polygonOptions = F, singleFeature=TRUE)%>%
      setView(-3,54.6,zoom=5.5)
  })
  #__________________________________________________________________________________________  
  #### PLOT BASELINE SURVEY ####
  
  #https://stackoverflow.com/questions/46979328/how-to-make-shiny-leaflet-map-reac-to-change-in-input-value-r
  
  # Watch for selection of baseline survey 
  observeEvent(input$baselineInput, {  
    
    # Modify existing map
    leafletProxy("map") %>%
      
      # Remove any previous selections 
      clearGroup("myMarkers1") %>%
      
      # Add markers
      addCircleMarkers(data = data[data$surveyname %in% input$baselineInput, ],
                       ~samplelong,
                       ~samplelat,
                       group = "myMarkers1",
                       popup = ~paste("<b>Station Code: </b>",samplecode),
                       radius =3.5,
                       color = "blue",
                       stroke = FALSE, fillOpacity = 1
      )
  })
  #__________________________________________________________________________________________  
  #### PLOT MONITORING SURVEY ####
  
  # Watch for selection of monitoring survey    
  observeEvent(input$monitoringInput, {  
    
    # Modify existing map
    leafletProxy("map") %>%
      
      # Remove any previous selections 
      clearGroup("myMarkers2") %>%
      
      # Add markers
      addCircleMarkers(data = data[data$surveyname %in% input$monitoringInput, ],
                       ~samplelong,
                       ~samplelat,
                       group = "myMarkers2",
                       popup = ~paste("<b>Station Code: </b>",samplecode),
                       radius =2,
                       color = "#00CCCC",
                       stroke = FALSE, fillOpacity = 1
      )
  })
  #__________________________________________________________________________________________ 
  #### RETRIEVE DATA FOR SELECTED SURVEYS ####
  
  coord <- reactive({
    
    req(input$baselineInput)
    req(input$monitoringInput)
    
    coord2 <- glue::glue_sql(
      "select

s.year,
st.stationcode,
su.surveyname,
s.samplecode,
s.samplelong,
s.samplelat,
w.wwacr,
SUM(sv.percentage) 

FROM 
samples.sample as s
inner join sediment_data.sedvarsample as sv on s.samplecode=sv.sample_samplecode
left join associations.samplestation as sst on s.samplecode=sst.sample_samplecode
left join associations.station as st on sst.station_stationcode=st.stationcode
inner join associations.surveysample as susa on susa.sample_samplecode=s.samplecode
inner join associations.survey as su on su.surveyname = susa.survey_surveyname
inner join sediment_data.sedvar as sedv on sedv.sievesize=sv.sedvar_sievesize
inner join sediment_data.wentworth as w on w.wwid=sedv.wentworth_id
where
su.surveyname IN ({baselinename*}) or
su.surveyname IN ({monitoring*})

GROUP BY
s.year,
st.stationcode,
su.surveyname,
s.samplecode,
s.samplelong,
s.samplelat,
w.wwacr

order by s.year desc, st.stationcode asc;",
      
      baselinename = input$baselineInput,
      monitoring = input$monitoringInput,
      .con=pool)
    
    coord3 <- as.data.frame(dbGetQuery(pool, coord2))
    coord4 <- coord3[,]
    
    ## Remove rows with cobbles
    coord4 = filter(coord4, !(wwacr %in% "Co"))
    return(coord4)
  })
  #__________________________________________________________________________________________ 
  #### BASELINE DATA SUBSET ####
  selsurbas <- reactive({
    # 1year,
    #2 stationcode,
    #3 surveyname,
    #4 samplecode,
    #5 samplelong,
    #6 samplelat,
    #7 wwacr,
    #8 SUM(sv.percentage) 
    #a <- subset( coord()[,c(4,5,6,7,11,12)],surveyname %in% input$baselineInput)
    a <- subset( coord()[,c(4,2,5,6,3,7,8)],surveyname %in% input$baselineInput)#
    return(a)
  })
  #__________________________________________________________________________________________  
  ## MONITORING DATA SUBSET ####
  selsurmon <- reactive({
    b <- subset( coord()[,c(4,2,5,6,3,7,8)],surveyname %in% input$monitoringInput)#samplecode,stationcode,samplelong,samplelat,surveyname,wwacr,SUM(sv.percentage) 
    return(b)
  })
  #__________________________________________________________________________________________ 
  #### BASELINE DATA: WIDE FORMAT #### 
  
  long <- reactive({
    long2 <- selsurbas()[,c(1:7)]#samplecode,stationcode,surveyname,stationlong,stationlat,wwacr,sum
    long3 <- reshape2::dcast(long2, samplecode+stationcode+samplelong+ samplelat+surveyname ~ wwacr, value.var="sum")
    long3$time <- "baseline"
    
    ## Change NA to zero (to allow dor rowsum calculation)
    long3[, 6:12][is.na(long3[, 6:12])] <- 0
    return(long3)
  })
  #_________________________________________________________________________________________  
  #### MONITORING DATA: WIDE FORMAT #### 
  
  longmon <- reactive({
    longmon2 <- selsurmon()[,c(1:7)]#stationcode,surveyname,stationlong,stationlat,wwacr,sum
    longmon3 <- reshape2::dcast(longmon2, samplecode+stationcode+samplelong+ samplelat+surveyname ~ wwacr, value.var="sum")
    
    longmon3$time <- "monitoring"
    
    ##Change NA to zero (to allow dor rowsum calculation)
    longmon3[, 6:12][is.na(longmon3[, 6:12])] <- 0
    
    return(longmon3)
  })
  #_________________________________________________________________________________________ 
  #### BASELINE DATA (WIDE): TABLE ####
  
  output$widebas <- DT::renderDataTable({
    DT::datatable(long()[,c(1:5,12,9,11,7,8,10,6)], colnames=c("Sample Code","Station Code","Long","Lat","Survey Name","SC","fS","mS","cS","fG","mG","cG"),class = "display nowrap",options = list(pageLength = 19,language = list(zeroRecords = "Data not publicly available")))%>%formatRound(columns=c("cG", "mG","fG","cS","mS","fS","SC"), digits=1) 
  })
  #__________________________________________________________________________________________ 
  #### MONITORING DATA (WIDE): TABLE ####
  
  output$widemon <- DT::renderDataTable({
    DT::datatable(longmon()[,c(1:5,12,9,11,7,8,10,6)],colnames=c("Sample Code","Station Code","Long","Lat","Survey Name","SC","fS","mS","cS","fG","mG","cG"),class = "display nowrap", options = list(pageLength = 19))%>%formatRound(columns=c("cG", "mG","fG","cS","mS","fS","SC"), digits=1) #removing [,1:11] will get rid of error message?????
  })
  #__________________________________________________________________________________________
  #### ALL DATA: PAIRED SAMPLES #### 
  
  longall <- reactive({
    
    ## Select only baseline samples where there is a corresponding monitoring sample
    long.paired <- long()
    
    ## Change baseline survey name to 'Baseline'
    #long.paired$surveyname <-"Baseline"
    
    ### Select only monitoring samples where there is a corresponding baseline sample
    longmon.paired <- longmon()
    
    ## Change monitoring survey name to 'Monitoring'
    #longmon.paired$surveyname <-"Monitoring"
    
    long.paired$match <-  long.paired$stationcode %in% longmon.paired$stationcode
    longmon.paired$match <-  longmon.paired$stationcode %in% long.paired$stationcode
    
    ## Now join the baseline df to the monitoring df
    testab=rbind(long.paired,longmon.paired)
    longall2 <- testab
    
    return(longall2)
  })
  
  longalltest <- reactive({
    
    df2<- longall() %>% 
      filter(if (input$paired == TRUE) match == TRUE else !is.na(match))
    
    df3 <- df2[,1:13]
    return(df3)
    
  })
  #_________________________________________________________________________________________  
  #### ALL DATA: TABLE ####
  
  output$wideall <- DT::renderDataTable({
    DT::datatable(longalltest()[,c(1:5,12,9,11,7,8,10,6)], colnames=c("Sample Code","Station Code","Long","Lat","Survey Name","SC","fS","mS","cS","fG","mG","cG"),class = "display nowrap",options = list(pageLength = 19))%>%formatRound(columns=c('cG', 'mG','fG','cS','mS','fS','SC'), digits=1)
  })
  #__________________________________________________________________________________________
  #### MAP: SHOW PAIRED SAMPLE SITES ####
  
  ## Watch for
  observeEvent(input$paired , {
    leafletProxy("map") %>%
      
      ## Remove any previous selections 
      clearGroup("myMarkers3")%>%
      clearGroup("myMarkers4")%>%
      
      ## Remove any previous selections 
      clearGroup("myMarkers1")%>%
      clearGroup("myMarkers2")%>%
      
      #addCircleMarkers(data = longall()[longall()$surveyname %in% input$baselineInput, ],
      #addCircleMarkers(data = longalltest()[longalltest()$surveyname =="Baseline", ],
      addCircleMarkers(data = longalltest()[longalltest()$surveyname %in% input$baselineInput, ],                 
                       ~samplelong,
                       ~samplelat,
                       group = "myMarkers3",
                       popup = ~paste("<b>Sample Code: </b>",samplecode),
                       radius =3.5,
                       color = "blue",
                       stroke = FALSE, fillOpacity = 1)%>%
      #addCircleMarkers(data = longall()[longall()$surveyname %in% input$monitoringInput, ],
      #addCircleMarkers(data = longalltest()[longalltest()$surveyname == "Monitoring", ], 
      addCircleMarkers(data = longalltest()[longalltest()$surveyname %in% input$monitoringInput, ],                
                       ~samplelong,
                       ~samplelat,
                       group = "myMarkers4",
                       popup = ~paste("<b>Sample Code: </b>",samplecode),
                       radius =2,
                       color = "#00CCCC",
                       stroke = FALSE, fillOpacity = 1)
  })
  #__________________________________________________________________________________________
  #### ASSIGN SAMPLES TO TREATMENT GROUPS ####
  
  stations <- reactive({
    
    ## Change point data to a sf object
    st_points <- longalltest() %>%
      mutate_at(vars(samplelong, samplelat), as.numeric) %>%   # coordinates must be numeric
      st_as_sf(
        coords = c("samplelong", "samplelat"),
        agr = "constant",
        crs = 4326,        # nad83 / new york long island projection
        stringsAsFactors = FALSE,
        remove = F)
    #__________________________________________________________________________________________ 
    ## REF
    ## Split off stations in Ref
    stations_in_ref <- st_join(st_points, test.ref(), join =st_within)
    
    ## Count number of rows in stations_in_ref
    count <- nrow(stations_in_ref)
    
    ## Count number of samples in a ref site
    count.na<- sum(is.na(stations_in_ref$area))
    
    ## if no ref samples then create an empty df for stations_in_ref2, otherwise subset where area is not NA
    if(count==count.na){
      stations_in_ref2 <-stations_in_ref[0,]
      
    } else{
      stations_in_ref2 <- stations_in_ref[!is.na(stations_in_ref$area),]#stations associated with a ref box
      stations_in_ref2$treatment <- "REF"
      stations_in_ref2$treatment2 <- paste(stations_in_ref2$area, "-", stations_in_ref2$treatment)
    }
    #__________________________________________________________________________________________  
    ## PIZ
    ## These are the stations remaining after those in Ref removed
    stations_after_ref_removed <- stations_in_ref[is.na(stations_in_ref$area),]
    
    
    ## Now take out the stations in PIZs. Cols 1:11 are those for st_points
    #stations_in_piz <- st_join(stations_after_ref_removed, piz, join =st_within)
    stations_in_piz <- st_join(stations_after_ref_removed, test.piz(), join =st_within)
    
    stations_in_piz <-  stations_in_piz[,c(1:13,15,16)]
    
    colnames(stations_in_piz)[14] <- "area"
    
    ## Count number of rows in stations_in_ref
    countpiz <- nrow(stations_in_piz)
    
    ## Count number of samples in a ref site
    countpiz.na<- sum(is.na(stations_in_piz$area))
    
    ## Where no data create an empty object
    if(countpiz==countpiz.na){
      stations_in_piz2 <- stations_in_piz[0,]
    } else{
      
      stations_in_piz2 <- stations_in_piz[!is.na(stations_in_piz$area),]
      stations_in_piz2$treatment <- "PIZ"
      stations_in_piz2$treatment2 <- paste(stations_in_piz2$area, "-",stations_in_piz2$treatment )
      
    }
    #__________________________________________________________________________________________       
    ## SIZ
    stations_siz_and_context <- stations_in_piz[is.na(stations_in_piz$area),]
    
    ## Now take out the stations in SIZs. Cols 1:6 are those for st_points
    stations_in_siz <- st_join(stations_siz_and_context, test.siz(), join =st_within)
    
    stations_in_siz <-  stations_in_siz[,c(1:13,15,16)]
    #colnames(stations_in_siz)[14] <- "region"
    colnames(stations_in_siz)[14] <- "area"
    
    ## Count number of rows in stations_in_ref
    countsiz <- nrow(stations_in_siz)
    
    ## Count number of samples in a ref site
    countsiz.na<- sum(is.na(stations_in_siz$area))
    
    ## Where no data create an empty object
    if(countsiz==countsiz.na){
      stations_in_siz2 <- stations_in_siz[0,]
    } else{
      stations_in_siz2 <- stations_in_siz[!is.na(stations_in_siz$area),]
      stations_in_siz2$treatment <- "SIZ"
      stations_in_siz2$treatment2 <- paste(stations_in_siz2$area, "-", stations_in_siz2$treatment)
    }
    #__________________________________________________________________________________________         
    ## CONTEXT
    ## Where no data create an empty object
    if(countsiz==countsiz.na){
      stations_in_context <- stations_in_siz[0,]
    } else{
      stations_in_context <- stations_in_siz[is.na(stations_in_siz$area),]
      stations_in_context$area <- "CONTEXT"
      stations_in_context$treatment <- "REF"
      stations_in_context$treatment2 <- paste(stations_in_context$area, "-",stations_in_context$treatment )
    }
    #__________________________________________________________________________________________        
    ## Now joint outputs together
    stations <- rbind(stations_in_piz2,stations_in_ref2,stations_in_siz2,stations_in_context)
    
    ## Drop geom column
    st_geometry(stations) <- NULL
    
    stations$treatment <- factor(stations$treatment, levels=c("PIZ","SIZ","REF"))
    
    return(stations)
  })
  #__________________________________________________________________________________________       
  ### TABLE OF SAMPLES AND THEIR TREATMENT CATEGORIES ####        
  
  output$refst <- DT::renderDataTable({
    DT::datatable(stations()[,c(1:5,12,9,11,7,8,10,6,13,14,15,16)], colnames=c("Sample Code","Station Code","Long","Lat","Survey Name","SC","fS","mS","cS","fG","mG","cG","Time","Area","Treatment","Treatment2"),class = "display nowrap",options = list(pageLength = 20))%>%formatRound(columns=c('cG', 'mG','fG','cS','mS','fS','SC'), digits=1)
  })
  #__________________________________________________________________________________________         
  #### REGION: PREPARE DATA #####
  
  ## Generate reactive object for PIZ, SIZ AND REF
  stations.reg <- reactive({
    stations.reg <- stations()[which(stations()$treatment=='PIZ'|stations()$treatment=='SIZ'|stations()$treatment=='REF'),]
    
    stations.reg$treatment2 <- stations.reg$treatment
    
    ## Create new col for treatment3 (concat of site/treatment/time)
    stations.reg$treatment3 <- paste(stations.reg$treatment2, "-", stations.reg$time)
    
    return(stations.reg[,2:ncol(stations.reg)])#exc the samplecode
    #stationcode	samplelong	samplelat	surveyname	cG	cS	fG	fS	mG	mS	SC	time	area	treatment	treatment2	treatment3
    
  })
  #__________________________________________________________________________________________
  #### SITE: PREPARE DATA #####
  
  ## Generate reactive object for site data
  stations.site <- reactive({
    stations.site <- stations()
    
    ## Create new col for treatment3 (concat of site/treatment/time)
    stations.site$treatment3 <- paste(stations.site$treatment2, "$", stations.site$time)
    
    return(stations.site[,2:ncol(stations.site)])#exc the samplecode
  })
  #__________________________________________________________________________________________
  #### SUB-REGION: PREPARE DATA #####
  
  ## Generate reactive object for sub regional data
  stations.subreg <- reactive({
    
    stations6 <- stations()
    stations6 <- stations6[which(stations6$treatment=='PIZ'|stations6$treatment=='SIZ'|stations6$treatment=='REF'),]
    
    if ( input$arrayInput=='South Coast RSMP') {
      stations6$area <- ifelse(stations6$samplelong > -2.14 & stations6$samplelong < -1.288, 'West IOW',
                               ifelse(stations6$samplelong > -1.288 & stations6$samplelong < -0.779, 'East IOW',
                                      ifelse(stations6$samplelong > -0.779 & stations6$samplelong < 0.244, 'Owers','')))
      
      stations6$treatment2 <- paste(stations6$area, "-", stations6$treatment)
      
      ## Delete rows where col 'area' is blank
      stations6 <- stations6[-which(stations6$area == ""), ]
    } else if ( input$arrayInput=='Humber RSMP') {
      stations6$area <- ifelse(stations6$samplelong < 0.78419 , 'Humber inner',
                               ifelse(stations6$samplelong > 0.78419, 'Humber outer'))
      
      stations6$treatment2 <- paste(stations6$area, "-", stations6$treatment)
      
      ## Delete rows where col 'area' is blank
      stations6 <- stations6[-which(stations6$area == ""), ]
      
    }
    
    ## Create new col for treatment3 (concat of site/treatment/time)
    stations6$treatment3 <- paste(stations6$treatment2, "$", stations6$time)
    
    return(stations6[,2:ncol(stations6)])#exc the samplecode
  })
  #__________________________________________________________________________________________
  #### REGION: NMDS 2D ####
  
  output$nmds <- renderPlot({
    
    library(vegan)
    
    ## Start with data in object 'stations4' from STEP 10.
    data <- stations.reg()
    data$time <- as.factor(data$time)
    
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Seperate out env data (Wentworth - for overlay) for ordination. Make sure values are numeric
    #env = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
   ## Perform the NMDS ordination 
    set.seed(123)
    ord <- metaMDS(data2,distance="eu",k = 2)
    
    ## Stress
    stress <- format(round(ord$stress, 3), nsmall = 3)  # Apply format function
    
    ## Now we run the envfit function with our environmental data frame, env.
    #en = envfit(ord, env, permutations = 999, na.rm = TRUE)
    
    ## Extract the sample coordinates in the NMDS ordination space
    data.scores = as.data.frame(scores(ord))
    
    ##Bind results of ordination to data (so you have access to factors etc)
    data.scores2 <- cbind(data,data.scores)
    
    ## Produce plot
    #en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
    
    p <-  ggplot(data = data.scores2, aes(x = NMDS1, y = NMDS2, col=time)) + 
      geom_point(data = data.scores2, size = 1.8, alpha =0.6) +
      scale_colour_manual(values = c("blue","#00CCCC"))+
      annotate(geom="text", x=0, y=1, label=paste("Stress=",stress))+
      facet_wrap(~treatment2)+
      theme(text = element_text(size=11),legend.title = element_blank(), 
            legend.text=element_text(size=11))
    print(p)
    
    
  })
  #__________________________________________________________________________________________
  #### SUBREGION: NMDS 2D ####
  
  output$nmds.subreg <- renderPlot({
    
    library(vegan)
    
    ## Start with data in object 'stations4' from STEP 10.
    data <- stations.subreg()
    data$time <- as.factor(data$time)
    
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Seperate out env data (Wentworth - for overlay) for ordination. Make sure values are numeric
    #env = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Perform the NMDS ordination 
    set.seed(123)
    ord <- metaMDS(data2,distance="eu",k = 2)
    
    ## Stress
    stress <- format(round(ord$stress, 3), nsmall = 3)  # Apply format function
    
    ## Now we run the envfit function with our environmental data frame, env.
    #en = envfit(ord, env, permutations = 999, na.rm = TRUE)
    
    ## Extract the sample coordinates in the NMDS ordination space
    data.scores = as.data.frame(scores(ord))
    
    ##Bind results of ordination to data (so you have access to factors etc)
    data.scores2 <- cbind(data,data.scores)
    
    ## Make a df for your subset with coordinates and time
    #data.scores.site = as.data.frame(data.scores3[,c(17:19,12)])
    
    ## Produce plot
    #en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
    
    p <-  ggplot(data = data.scores2, aes(x = NMDS1, y = NMDS2, col=time)) + 
      geom_point(data = data.scores2, size = 1.8, alpha =0.6) +
      scale_colour_manual(values = c("blue","#00CCCC"))+
      annotate(geom="text", x=0.5, y=1, label=paste("Stress=",stress))+
      facet_wrap(~treatment2)+
      theme(text = element_text(size=11),legend.title = element_blank(), 
            legend.text=element_text(size=11))
    print(p)
  })
  #__________________________________________________________________________________________
  #### SITE: NMDS 2D ####
  
  output$nmds.site.piz <- renderPlot({
    
    library(vegan)
    
    ## Start with data in object 'stations4' from STEP 10.
    data <- stations.site()
    data$time <- as.factor(data$time)
    
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Seperate out env data (Wentworth - for overlay) for ordination. Make sure values are numeric
    #env = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Perform the NMDS ordination 
    set.seed(123)
    ord <- metaMDS(data2,distance="eu",k = 2)
    
    ## Stress
    stress <- format(round(ord$stress, 3), nsmall = 3)  # Apply format function
    
    ## Now we run the envfit function with our environmental data frame, env.
    #en = envfit(ord, env, permutations = 999, na.rm = TRUE)
    
    ## Extract the sample coordinates in the NMDS ordination space
    data.scores = as.data.frame(scores(ord))
    
    ##Bind results of ordination to data (so you have access to factors etc)
    data.scores2 <- cbind(data,data.scores)
    
    ## Make a df for your subset with coordinates and time
    #data.scores.site = as.data.frame(data.scores3[,c(17:19,12)])
    
    ## Produce plot
    #en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
    
    p <-  ggplot(data = data.scores2, aes(x = NMDS1, y = NMDS2, col=time)) + 
      geom_point(data = data.scores2, size = 1.8, alpha =0.6) +
      scale_colour_manual(values = c("blue","#00CCCC"))+
      #annotate(geom="text", x=0.5, y=1, label=paste("Stress=",stress))+
      facet_wrap(~treatment2)+
      theme(text = element_text(size=11),legend.title = element_blank(), 
            legend.text=element_text(size=11))
    print(p)
  })
  
  #__________________________________________________________________________________________
  #### SITE: NMDS 3D ####
  
  output$nmds3dsite <- renderPlotly({
    library(vegan)
    
    ## Start with site data 
    data <- stations.site()
    data$time <- as.factor(data$time)
    data$treatment3 <- factor(data$treatment3)
    data$area <- as.factor(data$area)
    
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Seperate out env data (Wentworth - for overlay) for ordination. Make sure values are numeric
    #env = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## Perform the NMDS ordination 
    set.seed(123)
    ord <- metaMDS(data2,distance="eu",k = 3)
    
    ## Now we run the envfit function with our environmental data frame, env.
    #en = envfit(ord, env, permutations = 999, na.rm = TRUE)
    
    ## Extract the sample coordinates in the NMDS ordination space
    data.scores = as.data.frame(scores(ord))
    
    ##Bind results of ordination to data (so you have access to factors etc)
    data.scores2 <- cbind(data,data.scores)
    
    ## Subset data for PIZ
    # data.piz <- data.scores2[which(data.scores2$treatment2=='PIZ'),]#today
    data.piz <- data.scores2
    
    ## Number of levels in treatment3
    treat3nlevs <- nlevels(data.piz$treatment3)
    
    ## Colour pattern replicated by no. of treatment3 levels divided by 2
    cols <- rep(c('blue','#00CCCC'), (treat3nlevs/2))
    
    ## Plot
    fig.piz <- plot_ly(data.piz, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~treatment3,colors = cols,text = ~paste('Station Code: ', stationcode),hoverinfo = "text")
    fig.piz <- fig.piz %>% add_markers()
    fig.piz <- fig.piz %>% layout(scene = list(xaxis = list(title = 'NMDS1'),
                                               yaxis = list(title = 'NMDS2'),
                                               zaxis = list(title = 'NMDS3')))
    print(fig.piz)
  })
  
  #__________________________________________________________________________________________
  #### REGION: ANOSIM ####
  
  anosim.reg <- reactive({
    
    ## Find out which treatments have both baseline and monitoring samples
    library(dplyr)
    data <- stations.reg()
    data2 <- data%>%
      group_by(treatment2,time) %>%
      tally() %>%
      group_by(time)
    
    ## Tally by treatment 
    data3 <- data.frame(table(data2$treatment2))
    
    ## ID treatment groups where Freq is not 2 (i.e. missing eith baseline or monitoring)
    data4 <- data3[which(data3$Freq!=2),]
    
    ## Remove rows where there isn't baseline and monitoring
    `%notin%` <- Negate(`%in%`)#Negate %in%
    data5 <-  data %>% filter(treatment2 %notin% data4$Var1)
    data <- data5
    
    ## Pick off site variable
    site = as.character(data$treatment2)
    
    freqs = data.frame(table(site))
    
    ## Remove sites with only one before and after sample
    freqs.big = freqs[which(freqs$Freq>=2.5),]
    
    ## Number of sites that qualify
    nsites <- nrow(freqs.big) 
    
    ## Put the site names into a vector
    arse = as.data.frame(freqs.big)
    site.names = as.character(arse[,1])
    
    ## Create vectors to store R and p values from anosims
    R = rep(0, nsites)
    p = rep(0, nsites)
    
    for (j in 1:nsites) {
      
      ## pick off jth site
      use = site == site.names[j]
      
      ## Get the particle size matrix and the grouping vector
      site.use = data[use,c(12,5:11)]
      time.use = site.use$time
      matrix.use = site.use[,2:8]
      anos = anosim(matrix.use, grouping = time.use, distance = "eu")
      R[j] = anos$statistic
      p[j] = anos$signif
    }
    
    anosim.res <- data.frame(site.names,R,p)
    
    ## Add column for 'difference'
    anosim.res$interpretation <-anosim.res$R
    
    ## Now add interpretation
    anosim.res$interpretation[anosim.res$interpretation>0.75] <- "highly different"
    anosim.res$interpretation[anosim.res$interpretation>0.5 & anosim.res$interpretation<0.75] <- "different"
    anosim.res$interpretation[anosim.res$interpretation>0.25 & anosim.res$interpretation<0.5] <- "different with some overlap"
    anosim.res$interpretation[anosim.res$interpretation>0.1 & anosim.res$interpretation<0.25] <- "similar with some differences (or high overlap)"
    anosim.res$interpretation[anosim.res$interpretation<0.1] <- "similar"
    anosim.res$interpretation[anosim.res$p >0.05] <- "no evidence of change (p>0.05)" 
    
    return(anosim.res)
  })
  #__________________________________________________________________________________________      
  #### SUBREGION: ANOSIM ####
  
  anosim.subreg <- reactive({
    
    ## Find out which treatments have both baseline and monitoring samples
    library(dplyr)
    data <- stations.subreg()
    data2 <- data%>%
      group_by(treatment2,time) %>%
      tally() %>%
      group_by(time)
    
    ## Tally by treatment 
    data3 <- data.frame(table(data2$treatment2))
    
    ## ID treatment groups where Freq is not 2 (i.e. missing eith baseline or monitoring)
    data4 <- data3[which(data3$Freq!=2),]
    
    ## Remove rows where there isn't baseline and monitoring
    `%notin%` <- Negate(`%in%`)#Negate %in%
    data5 <-  data %>% filter(treatment2 %notin% data4$Var1)
    data <- data5
    
    ## Pick off site variable
    site = as.character(data$treatment2)
    freqs = data.frame(table(site))

    ## Remove sites with only one before and after sample
    freqs.big = freqs[which(freqs$Freq>=2.5),]
    
    ## There are 35 sites that qualify
    nsites <- nrow(freqs.big)
    
    ## Put the site names into a vector
    arse = as.data.frame(freqs.big)
    site.names = as.character(arse[,1])
    
    ## Create vectors to store R and p values from anosims
    R = rep(0, nsites)
    p = rep(0, nsites)
    
    for (j in 1:nsites) {
      
      ## pick off jth site
      use = site == site.names[j]
      
      ## Get the particle size matrix and the grouping vector
      site.use = data[use,c(12,5:11)]
      time.use = site.use$time
      matrix.use = site.use[,2:8]
      anos = anosim(matrix.use, grouping = time.use, distance = "eu")
      R[j] = anos$statistic
      p[j] = anos$signif
    }
    
    anosim.res <- data.frame(site.names,R,p)
    
    ## Add column for 'difference'
    anosim.res$interpretation <-anosim.res$R
    
    ## Now add interpretation
    anosim.res$interpretation[anosim.res$interpretation>0.75] <- "highly different"
    anosim.res$interpretation[anosim.res$interpretation>0.5 & anosim.res$interpretation<0.75] <- "different"
    anosim.res$interpretation[anosim.res$interpretation>0.25 & anosim.res$interpretation<0.5] <- "different with some overlap"
    anosim.res$interpretation[anosim.res$interpretation>0.1 & anosim.res$interpretation<0.25] <- "similar with some differences (or high overlap)"
    anosim.res$interpretation[anosim.res$interpretation<0.1] <- "similar"
    anosim.res$interpretation[anosim.res$p >0.05] <- "no evidence of change (p>0.05)" 
    
    return(anosim.res)
  })
  #__________________________________________________________________________________________ 
  #### SITE: ANOSIM ####
  
  anosim.site <- reactive({
    
    ## Find out which treatments have both baseline and monitoring samples
    library(dplyr)
    data <- stations.site()
    data2 <- data%>%
      group_by(treatment2,time) %>%
      tally() %>%
      group_by(time)
    
    ## Tally by treatment 
    data3 <- data.frame(table(data2$treatment2))
    
    
    ## ID treatment groups where Freq is not 2 (i.e. missing eith baseline or monitoring)
    data4 <- data3[which(data3$Freq!=2),]
    
    ## Remove rows where there isn't baseline and monitoring
    `%notin%` <- Negate(`%in%`)#Negate %in%
    data5 <-  data %>% filter(treatment2 %notin% data4$Var1)
    data <- data5
    
    ## Pick off site variable
    site = as.character(data$treatment2)
    freqs = data.frame(table(site))
    
    ## Remove sites with only one before and after sample
    freqs.big = freqs[which(freqs$Freq>=2.5),]
    
    ## There are 35 sites that qualify
    nsites <- nrow(freqs.big)
    
    ## Put the site names into a vector
    arse = as.data.frame(freqs.big)
    site.names = as.character(arse[,1])
    
    ## Create vectors to store R and p values from anosims
    R = rep(0, nsites)
    p = rep(0, nsites)
    
    for (j in 1:nsites) {
      
      ## pick off jth site
      use = site == site.names[j]
      
      ## Get the particle size matrix and the grouping vector
      site.use = data[use,c(12,5:11)]
      time.use = site.use$time
      matrix.use = site.use[,2:8]
      anos = anosim(matrix.use, grouping = time.use, distance = "eu")
      R[j] = anos$statistic
      p[j] = anos$signif
    }
    
    anosim.site <- data.frame(site.names,R,p)
    
    ## Add column for 'difference'
    anosim.site$interpretation <-anosim.site$R
    
    ## Now add interpretation
    anosim.site$interpretation[anosim.site$interpretation>0.75] <- "highly different"
    anosim.site$interpretation[anosim.site$interpretation>0.5 & anosim.site$interpretation<0.75] <- "different"
    anosim.site$interpretation[anosim.site$interpretation>0.25 & anosim.site$interpretation<0.5] <- "different with some overlap"
    anosim.site$interpretation[anosim.site$interpretation>0.1 & anosim.site$interpretation<0.25] <- "similar with some differences (or high overlap)"
    anosim.site$interpretation[anosim.site$interpretation<0.1] <- "similar"
    anosim.site$interpretation[anosim.site$p >0.05] <- "no evidence of change (p>0.05)" 
    
    return(anosim.site)
  })
  #__________________________________________________________________________________________
  #### REGION: ANOSIM TABLE ####
  
  output$anosim <- DT::renderDataTable({
    DT::datatable(anosim.reg(), colnames=c("Site","R","p","Interpretation"),options = list(pageLength = 8))%>%formatRound(columns=c('R', 'p'), digits=3)%>%formatStyle('interpretation',backgroundColor=styleEqual(c("highly different","different","different with some overlap","similar with some differences (or high overlap)","similar","no evidence of change (p>0.05)"),c('#cc3232','#db7b2b','#e7b416','#99c140','#2dc937','#2dc937')))
  })
  #__________________________________________________________________________________________
  #### SUBREGION: ANOSIM TABLE ####
  
  output$anosim.subreg <- DT::renderDataTable({
    DT::datatable(anosim.subreg(), colnames=c("Site","R","p","Interpretation"),options = list(pageLength = 20))%>%formatRound(columns=c('R', 'p'), digits=3)%>%formatStyle('interpretation',backgroundColor=styleEqual(c("highly different","different","different with some overlap","similar with some differences (or high overlap)","similar","no evidence of change (p>0.05)"),c('#cc3232','#db7b2b','#e7b416','#99c140','#2dc937','#2dc937')))
  })
  #__________________________________________________________________________________________
  #### SITE: ANOSIM TABLE ####
  
  output$anosim.site <- DT::renderDataTable({
    
    DT::datatable(anosim.site(), colnames=c("Site","R","p","Interpretation"),options = list(pageLength = 15))%>%formatRound(columns=c('R', 'p'), digits=3)%>%formatStyle('interpretation',backgroundColor=styleEqual(c("highly different","different","different with some overlap","similar with some differences (or high overlap)","similar","no evidence of change (p>0.05)"),c('#cc3232','#db7b2b','#e7b416','#99c140','#2dc937','#2dc937')))
  })
  #__________________________________________________________________________________________
  #### REGION: SIMPER ####
  
  simper.reg <- reactive({
    
    ## Start with data in object 'stations4' from STEP 10.
    data <- stations.reg()
    data$time <- as.factor(data$time)
    
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## SIMPER function (euclidean distance)
    #https://stackoverflow.com/questions/35773571/r-vegan-simper-analysis-modify-distance-matrix
    
    library(vegan)
    
    ## R function for getPermuteMatrix
    `getPermuteMatrix` <-
      function(perm, N,  strata = NULL)
      {
        ## 'perm' is either a single number, a how() structure or a
        ## permutation matrix
        if (length(perm) == 1) {
          perm <- how(nperm = perm)
        }
        ## apply 'strata', but only if possible: ignore silently other cases
        if (!missing(strata) && !is.null(strata)) {
          if (inherits(perm, "how") && is.null(getBlocks(perm)))
            setBlocks(perm) <- strata
        }
        ## now 'perm' is either a how() or a matrix
        if (inherits(perm, "how"))
          perm <- shuffleSet(N, control = perm)
        else { # matrix: check that it *strictly* integer
          if(!is.integer(perm) && !all(perm == round(perm)))
            stop("permutation matrix must be strictly integers: use round()")
        }
        ## now 'perm' is a matrix (or always was). If it is a plain
        ## matrix, set minimal attributes for printing. This is a dirty
        ## kluge: should be handled more cleanly.
        if (is.null(attr(perm, "control")))
          attr(perm, "control") <-
            structure(list(within=list(type="supplied matrix"),
                           nperm = nrow(perm)), class = "how")
        perm
      }
    
    
    ## R function for simper with euclidean distance
    
    euclidean_simper <- function (comm, group, permutations = 0, trace = FALSE, parallel = getOption("mc.cores"), 
                                  ...) {
      EPS <- sqrt(.Machine$double.eps)
      if (any(rowSums(comm, na.rm = TRUE) == 0)) 
        warning("you have empty rows: results may be meaningless")
      pfun <- function(x, comm, comp, i, contrp) {
        groupp <- group[perm[x, ]]
        ga <- comm[groupp == comp[i, 1], , drop = FALSE]
        gb <- comm[groupp == comp[i, 2], , drop = FALSE]
        n.a <- nrow(ga)
        n.b <- nrow(gb)
        for (j in seq_len(n.b)) {
          for (k in seq_len(n.a)) {
            mdp<-(ga[k,,drop=FALSE] - gb[j,, drop=FALSE])^2
            contrp[(j-1)*n.a+k, ] <- mdp
          }
        }
        colMeans(contrp)
      }
      comm <- as.matrix(comm)
      comp <- t(combn(unique(as.character(group)), 2))
      outlist <- NULL
      P <- ncol(comm)
      nobs <- nrow(comm)
      perm <- getPermuteMatrix(permutations, nobs, ...)
      if (ncol(perm) != nobs) 
        stop(gettextf("'permutations' have %d columns, but data have %d rows", 
                      ncol(perm), nobs))
      nperm <- nrow(perm)
      if (nperm > 0) 
        perm.contr <- matrix(nrow = P, ncol = nperm)
      if (is.null(parallel)) 
        parallel <- 1
      hasClus <- inherits(parallel, "cluster")
      isParal <- hasClus || parallel > 1
      isMulticore <- .Platform$OS.type == "unix" && !hasClus
      if (isParal && !isMulticore && !hasClus) {
        parallel <- makeCluster(parallel)
      }
      for (i in seq_len(nrow(comp))) {
        group.a <- comm[group == comp[i, 1], , drop = FALSE]
        group.b <- comm[group == comp[i, 2], , drop = FALSE]
        n.a <- nrow(group.a)
        n.b <- nrow(group.b)
        contr <- matrix(ncol = P, nrow = n.a * n.b)
        for (j in seq_len(n.b)) {
          for (k in seq_len(n.a)) {
            md <-(group.a[k,,drop=FALSE] - group.b[j,,drop=FALSE])^2
            contr[(j-1)*n.a+k, ]<-md
          }
        }
        average <- colMeans(contr)
        if (nperm > 0) {
          if (trace) 
            cat("Permuting", paste(comp[i, 1], comp[i, 2], 
                                   sep = "_"), "\n")
          contrp <- matrix(ncol = P, nrow = n.a * n.b)
          if (isParal) {
            if (isMulticore) {
              perm.contr <- mclapply(seq_len(nperm), function(d) pfun(d, 
                                                                      comm, comp, i, contrp), mc.cores = parallel)
              perm.contr <- do.call(cbind, perm.contr)
            }
            else {
              perm.contr <- parSapply(parallel, seq_len(nperm), 
                                      function(d) pfun(d, comm, comp, i, contrp))
            }
          }
          else {
            perm.contr <- sapply(1:nperm, function(d) pfun(d, 
                                                           comm, comp, i, contrp))
          }
          p <- (rowSums(apply(perm.contr, 2, function(x) x >= 
                                average - EPS)) + 1)/(nperm + 1)
        }
        else {
          p <- NULL
        }
        overall <- sum(average)
        sdi <- apply(contr, 2, sd)
        ratio <- average/sdi
        ava <- colMeans(group.a)
        avb <- colMeans(group.b)
        ord <- order(average, decreasing = TRUE)
        cusum <- cumsum(average[ord]/overall)
        out <- list(species = colnames(comm), average = average, 
                    overall = overall, sd = sdi, ratio = ratio, ava = ava, 
                    avb = avb, ord = ord, cusum = cusum, p = p)
        outlist[[paste(comp[i, 1], "_", comp[i, 2], sep = "")]] <- out
      }
      if (isParal && !isMulticore && !hasClus) 
        stopCluster(parallel)
      attr(outlist, "permutations") <- nperm
      attr(outlist, "control") <- attr(perm, "control")
      class(outlist) <- "simper"
      outlist
    }
    
    ## Perform SIMPER analysis using modified function
    sim <- (sim <- with(data,euclidean_simper(data2, treatment3)))
    
    simsum <- summary(sim)
    top3<-lapply(simsum, `[`,1:7,)#get top 3 contributors
    cuss<-lapply(top3, `[`,4:6)#get last column
    
    rows<-lapply(top3, rownames)#get names from list
    #rows2<-lapply(cuss, cumsum)#get values from list
    rows2 <- cuss
    
    rowsdf<-do.call(rbind, lapply(rows, data.frame, stringsAsFactors=FALSE))#names into df
    
    cusumdf<-do.call(rbind, lapply(rows2, data.frame, stringsAsFactors=FALSE))#values into df
    
    simperdf<-cbind(rowsdf,cusumdf) #combine into one df
    
    colnames(simperdf)<-c('name','ava','avb','cusum') #change colnames
    simperdf$diff <- (simperdf$avb-simperdf$ava)#05/11
    library(data.table)
    simperdf <-setDT(simperdf, keep.rownames = TRUE)[]#convert rownames to a column
    
    simperdf<-separate(data = simperdf, col = rn, into = c("left", "right"), sep = "\\.")#seperate contrast names
    simperdf2<-separate(data = simperdf, col = left, into = c("a", "b"), sep = "\\_")#seperate contrast names
    simperdf3<-separate(data = simperdf2, col = a, into = c("a1", "a2"), sep = "\\-")
    simperdf4<-separate(data = simperdf3, col = b, into = c("b1", "b2"), sep = "\\-")
    
    
    simperdf4$match <- ifelse(simperdf4$a1 == simperdf4$b1, TRUE, FALSE)
    
    simperdf5 <- simperdf4[which(simperdf4$match==TRUE),]
    
    ## Select columns of interest ()
    simperdf6 <-simperdf5[,c(1,6,7,8,10,9)]#a1, name, ava, avb, diff, cusum
    
    ## Update column names
    colnames(simperdf6) <-c("Site","Sed","Baseline","Monitoring","Diff","CumSum") 
    simperdf6$Site<-str_trim(simperdf6$Site)
    #simperdf6$Contribution<- (1-simperdf6$cumsum)*100
    
    ## Subset anosim.reg() where p<0.05 (i.e. the statistically significant results)
    anosim.reg2 <- anosim.reg()
    #anosim.reg3 <- anosim.reg2[which(anosim.reg2$p < 0.05& anosim.reg2$R >0.1),]
    anosim.reg3 <- anosim.reg2[which(anosim.reg2$p < 0.05 & anosim.reg2$R > input$num),]
    anosim.reg3$site.names<-str_trim(anosim.reg3$site.names)
   
    
    library(dplyr)
    
    ##Now subset SIMPER results for only the sites where ANOSIM p<0.05
    simperdf7 <- simperdf6 %>% filter(Site %in% anosim.reg3$site.names)
    
    return(simperdf7)
  })
   #__________________________________________________________________________________________
  #### SUBREGION: SIMPER ####
  
  simper.subreg <- reactive({
    
    ## Start with data in object 'stations4' from STEP 10.
    data <- stations.subreg()
    data$time <- as.factor(data$time)
    
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
    
    ## SIMPER function (euclidean distance)
    #https://stackoverflow.com/questions/35773571/r-vegan-simper-analysis-modify-distance-matrix
    
    library(vegan)
    
    ## R function for getPermuteMatrix
    `getPermuteMatrix` <-
      function(perm, N,  strata = NULL)
      {
        ## 'perm' is either a single number, a how() structure or a
        ## permutation matrix
        if (length(perm) == 1) {
          perm <- how(nperm = perm)
        }
        ## apply 'strata', but only if possible: ignore silently other cases
        if (!missing(strata) && !is.null(strata)) {
          if (inherits(perm, "how") && is.null(getBlocks(perm)))
            setBlocks(perm) <- strata
        }
        ## now 'perm' is either a how() or a matrix
        if (inherits(perm, "how"))
          perm <- shuffleSet(N, control = perm)
        else { # matrix: check that it *strictly* integer
          if(!is.integer(perm) && !all(perm == round(perm)))
            stop("permutation matrix must be strictly integers: use round()")
        }
        ## now 'perm' is a matrix (or always was). If it is a plain
        ## matrix, set minimal attributes for printing. This is a dirty
        ## kluge: should be handled more cleanly.
        if (is.null(attr(perm, "control")))
          attr(perm, "control") <-
            structure(list(within=list(type="supplied matrix"),
                           nperm = nrow(perm)), class = "how")
        perm
      }
    
    
    ## R function for simper with euclidean distance
    
    euclidean_simper <- function (comm, group, permutations = 0, trace = FALSE, parallel = getOption("mc.cores"), 
                                  ...) {
      EPS <- sqrt(.Machine$double.eps)
      if (any(rowSums(comm, na.rm = TRUE) == 0)) 
        warning("you have empty rows: results may be meaningless")
      pfun <- function(x, comm, comp, i, contrp) {
        groupp <- group[perm[x, ]]
        ga <- comm[groupp == comp[i, 1], , drop = FALSE]
        gb <- comm[groupp == comp[i, 2], , drop = FALSE]
        n.a <- nrow(ga)
        n.b <- nrow(gb)
        for (j in seq_len(n.b)) {
          for (k in seq_len(n.a)) {
            mdp<-(ga[k,,drop=FALSE] - gb[j,, drop=FALSE])^2
            contrp[(j-1)*n.a+k, ] <- mdp
          }
        }
        colMeans(contrp)
      }
      comm <- as.matrix(comm)
      comp <- t(combn(unique(as.character(group)), 2))
      outlist <- NULL
      P <- ncol(comm)
      nobs <- nrow(comm)
      perm <- getPermuteMatrix(permutations, nobs, ...)
      if (ncol(perm) != nobs) 
        stop(gettextf("'permutations' have %d columns, but data have %d rows", 
                      ncol(perm), nobs))
      nperm <- nrow(perm)
      if (nperm > 0) 
        perm.contr <- matrix(nrow = P, ncol = nperm)
      if (is.null(parallel)) 
        parallel <- 1
      hasClus <- inherits(parallel, "cluster")
      isParal <- hasClus || parallel > 1
      isMulticore <- .Platform$OS.type == "unix" && !hasClus
      if (isParal && !isMulticore && !hasClus) {
        parallel <- makeCluster(parallel)
      }
      for (i in seq_len(nrow(comp))) {
        group.a <- comm[group == comp[i, 1], , drop = FALSE]
        group.b <- comm[group == comp[i, 2], , drop = FALSE]
        n.a <- nrow(group.a)
        n.b <- nrow(group.b)
        contr <- matrix(ncol = P, nrow = n.a * n.b)
        for (j in seq_len(n.b)) {
          for (k in seq_len(n.a)) {
            md <-(group.a[k,,drop=FALSE] - group.b[j,,drop=FALSE])^2
            contr[(j-1)*n.a+k, ]<-md
          }
        }
        average <- colMeans(contr)
        if (nperm > 0) {
          if (trace) 
            cat("Permuting", paste(comp[i, 1], comp[i, 2], 
                                   sep = "_"), "\n")
          contrp <- matrix(ncol = P, nrow = n.a * n.b)
          if (isParal) {
            if (isMulticore) {
              perm.contr <- mclapply(seq_len(nperm), function(d) pfun(d, 
                                                                      comm, comp, i, contrp), mc.cores = parallel)
              perm.contr <- do.call(cbind, perm.contr)
            }
            else {
              perm.contr <- parSapply(parallel, seq_len(nperm), 
                                      function(d) pfun(d, comm, comp, i, contrp))
            }
          }
          else {
            perm.contr <- sapply(1:nperm, function(d) pfun(d, 
                                                           comm, comp, i, contrp))
          }
          p <- (rowSums(apply(perm.contr, 2, function(x) x >= 
                                average - EPS)) + 1)/(nperm + 1)
        }
        else {
          p <- NULL
        }
        overall <- sum(average)
        sdi <- apply(contr, 2, sd)
        ratio <- average/sdi
        ava <- colMeans(group.a)
        avb <- colMeans(group.b)
        ord <- order(average, decreasing = TRUE)
        cusum <- cumsum(average[ord]/overall)
        out <- list(species = colnames(comm), average = average, 
                    overall = overall, sd = sdi, ratio = ratio, ava = ava, 
                    avb = avb, ord = ord, cusum = cusum, p = p)
        outlist[[paste(comp[i, 1], "_", comp[i, 2], sep = "")]] <- out
      }
      if (isParal && !isMulticore && !hasClus) 
        stopCluster(parallel)
      attr(outlist, "permutations") <- nperm
      attr(outlist, "control") <- attr(perm, "control")
      class(outlist) <- "simper"
      outlist
    }
    
    ## Perform SIMPER analysis using modified function
    sim <- (sim <- with(data,euclidean_simper(data2, treatment3)))
    
    simsum <- summary(sim)
    top3<-lapply(simsum, `[`,1:7,)#get top 3 contributors
    cuss<-lapply(top3, `[`,4:6)#get last column
    
    rows<-lapply(top3, rownames)#get names from list
    #rows2<-lapply(cuss, cumsum)#get values from list
    rows2 <- cuss
    
    rowsdf<-do.call(rbind, lapply(rows, data.frame, stringsAsFactors=FALSE))#names into df
    
    cusumdf<-do.call(rbind, lapply(rows2, data.frame, stringsAsFactors=FALSE))#values into df
    
    simperdf<-cbind(rowsdf,cusumdf) #combine into one df
    
    colnames(simperdf)<-c('name','ava','avb','cusum') #change colnames
    simperdf$diff <- (simperdf$avb-simperdf$ava)#05/11
    library(data.table)
    simperdf <-setDT(simperdf, keep.rownames = TRUE)[]#convert rownames to a column
    
    simperdf<-separate(data = simperdf, col = rn, into = c("left", "right"), sep = "\\.")#seperate contrasts names
    simperdf2<-separate(data = simperdf, col = left, into = c("a", "b"), sep = "\\_")#seperate contrasts names
    simperdf3<-separate(data = simperdf2, col = a, into = c("a1", "a2"), sep = "\\$")#\\-
    simperdf4<-separate(data = simperdf3, col = b, into = c("b1", "b2"), sep = "\\$")#\\-
    
    
    simperdf4$match <- ifelse(simperdf4$a1 == simperdf4$b1, TRUE, FALSE)
    
    simperdf5 <- simperdf4[which(simperdf4$match==TRUE),]
    #simperdf6 <-simperdf5[,c(1,6,7,8,10,9)]#a1, name, ava, avb, diff, cusum
    simperdf6 <-simperdf5[,c(1,6,7,8,10,9)]#a1, name, ava, avb, diff, cusum
    
    ## Update column names
    #colnames(simperdf6) <-c("Site","Sed","Baseline","Monitoring","Diff","cumsum")
    colnames(simperdf6) <-c("Site","Sed","Baseline","Monitoring","Diff","CumSum")
    simperdf6$Site<-str_trim(simperdf6$Site)
    #simperdf6$Contribution<- (1-simperdf6$cumsum)*100
    
    
    ## Subset anosim.reg() where p<0.05 (i.e. the statistically significant results)
    anosim.reg2 <- anosim.subreg()
    #anosim.reg3 <- anosim.reg2[which(anosim.reg2$p < 0.05 & anosim.reg2$R >0.1),]
    anosim.reg3 <- anosim.reg2[which(anosim.reg2$p < 0.05 & anosim.reg2$R > input$num),]
    anosim.reg3$site.names<-str_trim(anosim.reg3$site.names)
    
    library(dplyr)
    
    ##Now subset SIMPER results for only the sites where ANOSIM p<0.05
    simperdf7 <- simperdf6 %>% filter(Site %in% anosim.reg3$site.names)
    
    
    
    return(simperdf7)
  })
  #__________________________________________________________________________________________
  #### SITE: SIMPER ####
  
  simper.site <- reactive({
    
    #### region ####
    ## Start with data in object 'stations4' from STEP 10.
    data <- stations.site()
     
    data$time <- as.factor(data$time)
    ## Seperate out sieve data for ordination. Make sure values are numeric
    data2 = as.data.frame(sapply(data[,c(11,8,10,6,7,9,5)], as.numeric))
   
    
    ## SIMPER function (euclidean distance)
    #https://stackoverflow.com/questions/35773571/r-vegan-simper-analysis-modify-distance-matrix
    
    library(vegan)
    
    ## R function for getPermuteMatrix
    `getPermuteMatrix` <-
      function(perm, N,  strata = NULL)
      {
        ## 'perm' is either a single number, a how() structure or a
        ## permutation matrix
        if (length(perm) == 1) {
          perm <- how(nperm = perm)
        }
        ## apply 'strata', but only if possible: ignore silently other cases
        if (!missing(strata) && !is.null(strata)) {
          if (inherits(perm, "how") && is.null(getBlocks(perm)))
            setBlocks(perm) <- strata
        }
        ## now 'perm' is either a how() or a matrix
        if (inherits(perm, "how"))
          perm <- shuffleSet(N, control = perm)
        else { # matrix: check that it *strictly* integer
          if(!is.integer(perm) && !all(perm == round(perm)))
            stop("permutation matrix must be strictly integers: use round()")
        }
        ## now 'perm' is a matrix (or always was). If it is a plain
        ## matrix, set minimal attributes for printing. This is a dirty
        ## kluge: should be handled more cleanly.
        if (is.null(attr(perm, "control")))
          attr(perm, "control") <-
            structure(list(within=list(type="supplied matrix"),
                           nperm = nrow(perm)), class = "how")
        perm
      }
    
    
    ## R function for simper with euclidean distance
    
    euclidean_simper <- function (comm, group, permutations = 0, trace = FALSE, parallel = getOption("mc.cores"), 
                                  ...) {
      EPS <- sqrt(.Machine$double.eps)
      if (any(rowSums(comm, na.rm = TRUE) == 0)) 
        warning("you have empty rows: results may be meaningless")
      pfun <- function(x, comm, comp, i, contrp) {
        groupp <- group[perm[x, ]]
        ga <- comm[groupp == comp[i, 1], , drop = FALSE]
        gb <- comm[groupp == comp[i, 2], , drop = FALSE]
        n.a <- nrow(ga)
        n.b <- nrow(gb)
        for (j in seq_len(n.b)) {
          for (k in seq_len(n.a)) {
            mdp<-(ga[k,,drop=FALSE] - gb[j,, drop=FALSE])^2
            contrp[(j-1)*n.a+k, ] <- mdp
          }
        }
        colMeans(contrp)
      }
      comm <- as.matrix(comm)
      comp <- t(combn(unique(as.character(group)), 2))
      outlist <- NULL
      P <- ncol(comm)
      nobs <- nrow(comm)
      perm <- getPermuteMatrix(permutations, nobs, ...)
      if (ncol(perm) != nobs) 
        stop(gettextf("'permutations' have %d columns, but data have %d rows", 
                      ncol(perm), nobs))
      nperm <- nrow(perm)
      if (nperm > 0) 
        perm.contr <- matrix(nrow = P, ncol = nperm)
      if (is.null(parallel)) 
        parallel <- 1
      hasClus <- inherits(parallel, "cluster")
      isParal <- hasClus || parallel > 1
      isMulticore <- .Platform$OS.type == "unix" && !hasClus
      if (isParal && !isMulticore && !hasClus) {
        parallel <- makeCluster(parallel)
      }
      for (i in seq_len(nrow(comp))) {
        group.a <- comm[group == comp[i, 1], , drop = FALSE]
        group.b <- comm[group == comp[i, 2], , drop = FALSE]
        n.a <- nrow(group.a)
        n.b <- nrow(group.b)
        contr <- matrix(ncol = P, nrow = n.a * n.b)
        for (j in seq_len(n.b)) {
          for (k in seq_len(n.a)) {
            md <-(group.a[k,,drop=FALSE] - group.b[j,,drop=FALSE])^2
            contr[(j-1)*n.a+k, ]<-md
          }
        }
        average <- colMeans(contr)
        if (nperm > 0) {
          if (trace) 
            cat("Permuting", paste(comp[i, 1], comp[i, 2], 
                                   sep = "_"), "\n")
          contrp <- matrix(ncol = P, nrow = n.a * n.b)
          if (isParal) {
            if (isMulticore) {
              perm.contr <- mclapply(seq_len(nperm), function(d) pfun(d, 
                                                                      comm, comp, i, contrp), mc.cores = parallel)
              perm.contr <- do.call(cbind, perm.contr)
            }
            else {
              perm.contr <- parSapply(parallel, seq_len(nperm), 
                                      function(d) pfun(d, comm, comp, i, contrp))
            }
          }
          else {
            perm.contr <- sapply(1:nperm, function(d) pfun(d, 
                                                           comm, comp, i, contrp))
          }
          p <- (rowSums(apply(perm.contr, 2, function(x) x >= 
                                average - EPS)) + 1)/(nperm + 1)
        }
        else {
          p <- NULL
        }
        overall <- sum(average)
        sdi <- apply(contr, 2, sd)
        ratio <- average/sdi
        ava <- colMeans(group.a)
        avb <- colMeans(group.b)
        ord <- order(average, decreasing = TRUE)
        cusum <- cumsum(average[ord]/overall)
        out <- list(species = colnames(comm), average = average, 
                    overall = overall, sd = sdi, ratio = ratio, ava = ava, 
                    avb = avb, ord = ord, cusum = cusum, p = p)
        outlist[[paste(comp[i, 1], "_", comp[i, 2], sep = "")]] <- out
      }
      if (isParal && !isMulticore && !hasClus) 
        stopCluster(parallel)
      attr(outlist, "permutations") <- nperm
      attr(outlist, "control") <- attr(perm, "control")
      class(outlist) <- "simper"
      outlist
    }
    
    ## Perform SIMPER analysis using modified function
    sim <- (sim <- with(data,euclidean_simper(data2, treatment3)))
    
    simsum <- summary(sim)
    top3<-lapply(simsum, `[`,1:7,)#get top 3 contributors
    cuss<-lapply(top3, `[`,4:6)#get last column
    
    rows<-lapply(top3, rownames)#get names from list
    #rows2<-lapply(cuss, cumsum)#get values from list
    rows2 <- cuss
    
    rowsdf<-do.call(rbind, lapply(rows, data.frame, stringsAsFactors=FALSE))#names into df
    
    cusumdf<-do.call(rbind, lapply(rows2, data.frame, stringsAsFactors=FALSE))#values into df
    
    simperdf<-cbind(rowsdf,cusumdf) #combine into one df
    colnames(simperdf)<-c('name','ava','avb','cusum') #change colnames
    simperdf$diff <- (simperdf$avb-simperdf$ava)#05/11
    library(data.table)
    simperdf <-setDT(simperdf, keep.rownames = TRUE)[]#convert rownames to a column
    
    simperdf<-separate(data = simperdf, col = rn, into = c("left", "right"), sep = "\\.")#seperate contrasts names
    simperdf2<-separate(data = simperdf, col = left, into = c("a", "b"), sep = "\\_")#seperate contrasts names
    simperdf3<-separate(data = simperdf2, col = a, into = c("a1", "a2"), sep = "\\$")#\\-
    simperdf4<-separate(data = simperdf3, col = b, into = c("b1", "b2"), sep = "\\$")#\\-

    simperdf4$match <- ifelse(simperdf4$a1 == simperdf4$b1, TRUE, FALSE)
    
    simperdf5 <- simperdf4[which(simperdf4$match==TRUE),]

    #simperdf6 <-simperdf5[,c(1,6,7,8,10,9)]#a1, name, ava, avb, diff, cusum
    simperdf6 <-simperdf5[,c(1,6,7,8,10,9)]#a1, name, ava, avb, diff, cusum
    
    ## Update column names
    #colnames(simperdf6) <-c("Site","Sed","Baseline","Monitoring","Diff","cumsum")
    colnames(simperdf6) <-c("Site","Sed","Baseline","Monitoring","Diff","CumSum")
    simperdf6$Site<-str_trim(simperdf6$Site)
    #simperdf6$Contribution<- (1-simperdf6$cumsum)*100
    
    
    ## Subset anosim.reg() where p<0.05 (i.e. the statistically significant results)
    anosim.reg2 <- anosim.site()
    #anosim.reg3 <- anosim.reg2[which(anosim.reg2$p < 0.05 & anosim.reg2$R >0.1),]
    anosim.reg3 <- anosim.reg2[which(anosim.reg2$p < 0.05 & anosim.reg2$R > input$num),]
    anosim.reg3$site.names<-str_trim(anosim.reg3$site.names)

    library(dplyr)
    
    ##Now subset SIMPER results for only the sites where ANOSIM p<0.05
  
    simperdf7 <- simperdf6 %>% filter(Site %in% anosim.reg3$site.names)
    
    return(simperdf7)
  })
  #__________________________________________________________________________________________
  #### REGION: SIMPER TABLE ####
  
  output$simpertab.reg <- DT::renderDataTable({
    DT::datatable(simper.reg(), options = list(pageLength = 20,searching = FALSE,paging = FALSE,
                                               language = list(
                                                 zeroRecords = "Not relevant given Anosim found no R values > than set value.")))%>%formatRound(columns=c('Baseline', 'Monitoring','Diff','CumSum'), digits=1)
  })
  
  #__________________________________________________________________________________________
  #### SUBREGION: SIMPER TABLE ####
  
  output$simpertab.subreg <- DT::renderDataTable({
    DT::datatable(simper.subreg(), options = list(pageLength = 20,searching = FALSE,paging = FALSE,
                                                  language = list(
                                                    zeroRecords = "Not relevant given Anosim found no R values > than set value.")))%>%formatRound(columns=c('Baseline', 'Monitoring','Diff','CumSum'), digits=1)
  })
  #__________________________________________________________________________________________
  #### SITE: SIMPER TABLE ####
  
  output$simpertab.site <- DT::renderDataTable({
    #DT::datatable(simper.site(), options = list(pageLength = 20))%>%formatRound(columns=c('Baseline', 'Monitoring','Diff','cumsum','Contribution'), digits=1)
    DT::datatable(simper.site(), options = list(pageLength = 20,searching = FALSE,paging = FALSE,
                                                language = list(
                                                  zeroRecords = "Not relevant given Anosim found no R values > than set value.")))%>%formatRound(columns=c('Baseline', 'Monitoring','Diff','CumSum'), digits=1)
  })
  
  #__________________________________________________________________________________________
  #### REGION: MEANS DATA #### 
  sed.sum.reg <- reactive({ 
    sumdata=stations.reg()[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
      group_by(surveyname,treatment) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    ## Order by treatment and area
    sumdata2 <- sumdata[order(sumdata$treatment ),]
    return(sumdata2)
  })
  #__________________________________________________________________________________________
  #### SUBREGION: MEANS DATA #### 
  sed.sum.subreg <- reactive({ 
    sumdata=stations.subreg()[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
      
      group_by(surveyname,treatment2) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    ## Order by treatment and area
    sumdata2 <- sumdata[order(sumdata$treatment2 ),]
    return(sumdata2)
  })
  #__________________________________________________________________________________________
  #### SITE: MEANS DATA #### 
  
  sed.sum.site <- reactive({ 
    sumdata=stations.site()[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
      group_by(surveyname,treatment2) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    ## Order by treatment and area
    sumdata2 <- sumdata[order(sumdata$treatment2 ),]
    
    return(sumdata2)
  })
  #__________________________________________________________________________________________
  #### REGION: MEANS DATA TABLE ####      
  
  output$sedsumreg <- DT::renderDataTable({
    DT::datatable(sed.sum.reg(),colnames=c("Survey","Treatment2","Count","SC","fS","mS","cS","fG","mG","cG"), options = list(pageLength = 12))%>%formatRound(columns=c('SC','fS','mS','cS','fG','mG','cG'), digits=1)
  })
  
  #__________________________________________________________________________________________
  #### SUBEGION: MEANS DATA TABLE ####      
  
  output$sedsumsubreg <- DT::renderDataTable({
    DT::datatable(sed.sum.subreg(), colnames=c("Survey","Treatment2","Count","SC","fS","mS","cS","fG","mG","cG"),options = list(pageLength = 12))%>%formatRound(columns=c('SC','fS','mS','cS','fG','mG','cG'), digits=1)
  })
  
  #__________________________________________________________________________________________ 
  #### SITE: MEANS DATA TABLE ####     
  
  output$sedsumsite <- DT::renderDataTable({
    DT::datatable(sed.sum.site(),colnames=c("Survey","Treatment2","Count","SC","fS","mS","cS","fG","mG","cG"), options = list(pageLength = 12))%>%formatRound(columns=c('SC','fS','mS','cS','fG','mG','cG'), digits=1)
  })
  
  #__________________________________________________________________________________________
  #### REGION: MEANS DIFF #### 
  
  sed.sum.reg.diff <- reactive({ 
    
    data.diff <- stations.reg()
    data.diff$surveyname <- factor(data.diff$surveyname, levels=c(input$baselineInput,input$monitoringInput))
    #data.diff$surveyname <- factor(stations.reg()$surveyname, levels=c(input$baselineInput,input$monitoringInput))
    #data.diff$surveyname <- factor(data.diff$surveyname, levels=c("Baseline","Monitoring"))
    
    #data.diff2=stations.reg()[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
    data.diff2=data.diff[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
      group_by(surveyname,treatment) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    data.diff3 <- data.diff2 %>%
      group_by(treatment) %>%
      arrange(surveyname, .by_group = TRUE) %>%
      mutate(SC_diff = SC - lag(SC, default = first(SC)),
             fS_diff = fS - lag(fS, default = first(fS)),
             mS_diff = mS - lag(mS, default = first(mS)),
             cS_diff = cS - lag(cS, default = first(cS)),
             fG_diff = fG - lag(fG, default = first(fG)),
             mG_diff = mG - lag(mG, default = first(mG)),
             cG_diff = cG - lag(cG, default = first(cG))
      )
    
    data.diff4 <- data.diff3[,c(1,2,3,11:17)]#surveyname,treatment,count,SC_diff:cG_diff
    #data.diff5 <- data.diff4[which(data.diff4$surveyname == input$monitoringInput),] #
    #
    data.diff5 <- data.diff4[which(data.diff4$surveyname == input$monitoringInput),]
    return(data.diff5)
  })
  #__________________________________________________________________________________________ 
  #### SUBREGION: MEANS DIFF #### 
  
  sed.sum.subreg.diff <- reactive({ 
    
    data.diff <- stations.subreg()
    data.diff$surveyname <- factor(data.diff$surveyname, levels=c(input$baselineInput,input$monitoringInput))
    #data.diff$surveyname <- factor(stations.reg()$surveyname, levels=c(input$baselineInput,input$monitoringInput))
    #data.diff$surveyname <- factor(data.diff$surveyname, levels=c("Baseline","Monitoring"))
    
    #data.diff2=stations.reg()[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
    data.diff2=data.diff[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
      group_by(surveyname,treatment2) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    data.diff3 <- data.diff2 %>%
      group_by(treatment2) %>%
      arrange(surveyname, .by_group = TRUE) %>%
      mutate(SC_diff = SC - lag(SC, default = first(SC)),
             fS_diff = fS - lag(fS, default = first(fS)),
             mS_diff = mS - lag(mS, default = first(mS)),
             cS_diff = cS - lag(cS, default = first(cS)),
             fG_diff = fG - lag(fG, default = first(fG)),
             mG_diff = mG - lag(mG, default = first(mG)),
             cG_diff = cG - lag(cG, default = first(cG))
      )
    
    data.diff4 <- data.diff3[,c(1,2,3,11:17)]#surveyname,treatment,count,SC_diff:cG_diff
    data.diff5 <- data.diff4[which(data.diff4$surveyname == input$monitoringInput),] #
    return(data.diff5)
  })
  #__________________________________________________________________________________________ 
  #### SITE: MEANS DIFF ####
  sed.sum.site.diff <- reactive({ 
    
    data.diff <- stations.site()
    data.diff$surveyname <- factor(data.diff$surveyname, levels=c(input$baselineInput,input$monitoringInput))
    #data.diff$surveyname <- factor(data.diff$surveyname, levels=c("Baseline","Monitoring"))
    
    #data.diff2=stations.reg()[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
    data.diff2=data.diff[c(4,15,16,14,as.numeric(5:11))]%>%#surveyname, treatment,treatment2,area,SC,fs,ms,cs,fg,mg,cg
      group_by(surveyname,treatment2) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    data.diff3 <- data.diff2 %>%
      group_by(treatment2) %>%
      arrange(surveyname, .by_group = TRUE) %>%
      mutate(SC_diff = SC - lag(SC, default = first(SC)),
             fS_diff = fS - lag(fS, default = first(fS)),
             mS_diff = mS - lag(mS, default = first(mS)),
             cS_diff = cS - lag(cS, default = first(cS)),
             fG_diff = fG - lag(fG, default = first(fG)),
             mG_diff = mG - lag(mG, default = first(mG)),
             cG_diff = cG - lag(cG, default = first(cG))
      )
    
    data.diff4 <- data.diff3[,c(1,2,3,11:17)]#surveyname,treatment,count,SC_diff:cG_diff
    data.diff5 <- data.diff4[which(data.diff4$surveyname == input$monitoringInput),] #
    #setwd("C:/Users/kmc00/OneDrive - CEFAS/working")
    #write.csv(data.diff5,'data.diff5v3029.csv', row.names=F) 
    return(data.diff5)
  })
  #__________________________________________________________________________________________  
  #### REGION: MEANS DIFF TABLE #### 
  
  output$sedsumregdiff <- DT::renderDataTable({
    DT::datatable(sed.sum.reg.diff(), options = list(pageLength = 3))%>%formatRound(columns=c('SC_diff','fS_diff','mS_diff','cS_diff','fG_diff','mG_diff','cG_diff'), digits=1)
  })
  #__________________________________________________________________________________________
  #### SUBREGION: MEANS DIFF TABLE #### 
  
  output$sedsumsubregdiff <- DT::renderDataTable({
    DT::datatable(sed.sum.subreg.diff(), options = list(pageLength = 3))%>%formatRound(columns=c('SC_diff','fS_diff','mS_diff','cS_diff','fG_diff','mG_diff','cG_diff'), digits=1)
  })
  #__________________________________________________________________________________________
  #### SITE: MEANS DIFF TABLE ####     
  output$sedsumsitediff <- DT::renderDataTable({
    DT::datatable(sed.sum.site.diff(), options = list(pageLength = 3))%>%formatRound(columns=c('SC_diff','fS_diff','mS_diff','cS_diff','fG_diff','mG_diff','cG_diff'), digits=1)
  })
  #__________________________________________________________________________________________
  #### REGION: CHANGE PLOTS ####
  output$change.reg <- renderPlot({
    
    data <- sed.sum.reg.diff()
    colnames(data) <- c("surveyname", "treatment", "count",   "SC",  "fS","mS",   "cS",    "fG",    "mG",      "cG")
    library(tidyr)
    data_long <- gather(data, sed, percentage, SC:cG, factor_key=TRUE)
    data_long
    
    ## Select sites to plot
    siganosim <- anosim.reg()[which(anosim.reg()$interpretation!='similar' & anosim.reg()$interpretation!='no evidence of change (p>0.05)'),]
    notsimilar <- siganosim$site.names
    data_long2 <- data_long[data_long$treatment %in% notsimilar, ]
    
    ## Change error message where no data to plot
    validate(
      need(data_long2$treatment != "", "No data to plot (treatments either 'similar' or 'no evidence of change')"))
    
    p<-ggplot(data=data_long2, aes(x=sed, y=percentage,fill=percentage<0)) +
      geom_bar(stat="identity")+
      scale_fill_manual(guide = FALSE,
                        values = c("#4682B4", "#4682B4"))+
      labs(x="Sediment",y="Percentage")+
      facet_wrap(~treatment)
    print(p)
  })
  #__________________________________________________________________________________________
  #### SUBREGION: CHANGE PLOTS ####
  output$change.subreg <- renderPlot({
    
    data <- sed.sum.subreg.diff()
    colnames(data) <- c("surveyname", "treatment", "count",   "SC",  "fS","mS",   "cS",    "fG",    "mG",      "cG")
    library(tidyr)
    data_long <- gather(data, sed, percentage, SC:cG, factor_key=TRUE)
    data_long
    
    ## Select sites to plot
    siganosim <- anosim.subreg()[which(anosim.subreg()$interpretation!='similar' & anosim.subreg()$interpretation!='no evidence of change (p>0.05)'),]
    notsimilar <- siganosim$site.names
    data_long2 <- data_long[data_long$treatment %in% notsimilar, ]
    
    ## Change error message where no data to plot
    validate(
      need(data_long2$treatment != "", "No data to plot (tretaments either 'similar' or 'no evidence of change')"))
    
    p<-ggplot(data=data_long2, aes(x=sed, y=percentage,fill=percentage<0)) +
      geom_bar(stat="identity")+
      scale_fill_manual(guide = FALSE,
                        values = c("#4682B4", "#4682B4"))+
      labs(x="Sediment",y="Percentage")+
      facet_wrap(~treatment)
    print(p)
  })
  #__________________________________________________________________________________________
  #### SITE: CHANGE PLOTS ####
  
  output$change.site <- renderPlot({
    
    data <- sed.sum.site.diff()
    colnames(data) <- c("surveyname", "treatment", "count",   "SC",  "fS","mS",   "cS",    "fG",    "mG",      "cG")
    library(tidyr)
    data_long <- gather(data, sed, percentage, SC:cG, factor_key=TRUE)
    data_long
    
    ## Select sites to plot
    siganosim <- anosim.site()[which(anosim.site()$interpretation!='similar' & anosim.site()$interpretation!='no evidence of change (p>0.05)'),]
    notsimilar <- siganosim$site.names
    data_long2 <- data_long[data_long$treatment %in% notsimilar, ]
    
    ## Change error message where no data to plot
    validate(
      need(data_long2$treatment != "", "No data to plot (tretaments either 'similar' or 'no evidence of change')"))
    
    p<-ggplot(data=data_long2, aes(x=sed, y=percentage,fill=percentage<0)) +
      geom_bar(stat="identity")+
      scale_fill_manual(guide = FALSE,
                        values = c("#4682B4", "#4682B4"))+
      labs(x="Sediment",y="Percentage")+
      facet_wrap(~treatment)
    print(p)
  })
  #__________________________________________________________________________________________
  #### REGION: LINE PLOTS ####
  
  output$reg.line <- renderPlot({
    adata <- stations.reg()[,c(15,12,11,8,10,6,7,9,5,1)]#treatment2, time, SC,fS,mS,cS,fG,mG,cG,stationcode 
    adata$ID <- seq.int(nrow(adata))
    
    ## Data by Area
    library(dplyr)
    sumdata=adata[c(1,2,3,as.numeric(4:11))]%>%
      
      group_by(treatment2,time) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    
    ## Get into long format
    library(tidyr)
    data_long_mean <- gather(sumdata, sed, percentage, SC:cG, factor_key=TRUE)
    
    ## Plots
    p <- ggplot()+
      # geom_line(data=data_long2, aes(x=sed, y=percentage, col=time,group=ID),linetype = "dashed",size = 0.5) +# samples
      geom_line(data=data_long_mean,aes(x=sed, y=percentage,col=time, group=time),size = 3)+ #means
      scale_colour_manual(values = c("blue","#00CCCC"))+
      labs(x="Sediment",y="Percentage")+
      facet_wrap(~treatment2)+
      theme(text = element_text(size=11),legend.title = element_blank(), 
            legend.text=element_text(size=11))#a 
    print(p)
  })  
  #__________________________________________________________________________________________
  #### SUBREGION: LINE PLOTS ####
  
  output$subreg.line <- renderPlot({
    
    adata <- stations.subreg()[,c(15,12,11,8,10,6,7,9,5,1)]#treatment2, time, SC,fS,mS,cS,fG,mG,cG,stationcode
    adata$ID <- seq.int(nrow(adata))
    
    
    ## Get summary data into long format
    library(tidyr)
    data_long <- gather(adata, sed, percentage, SC:cG, factor_key=TRUE)
    
    ## Data by Area
    library(dplyr)
    sumdata=adata[c(1,2,3,as.numeric(4:11))]%>%
      group_by(treatment2,time) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    sumdata
    
    ## Get into long format
    library(tidyr)
    data_long_mean <- gather(sumdata, sed, percentage, SC:cG, factor_key=TRUE)
    
    ## Plots
    p <- ggplot()+
      geom_line(data=data_long, aes(x=sed, y=percentage, col=time,group=ID),linetype = "dashed",size = 0.5) +# samples
      geom_line(data=data_long_mean,aes(x=sed, y=percentage,col=time, group=time),size = 3)+ #means
      scale_colour_manual(values = c("blue","#00CCCC"))+
      labs(x="Sediment",y="Percentage")+
      facet_wrap(~treatment2)+
      theme(text = element_text(size=11),legend.title = element_blank(), 
            legend.text=element_text(size=11))#a 
    print(p)
  }) 
  #__________________________________________________________________________________________     
  #### SITE: LINE PLOTS ####
  
  output$site.line <- renderPlot({
    adata <- stations()[,c(16,13,12,9,11,7,8,10,6,1)]#treatment2, time, SC,fS,mS,cS,fG,mG,cG,stationcode
    adata$ID <- seq.int(nrow(adata))
    
    ## Get summary data into long format
    library(tidyr)
    data_long <- gather(adata, sed, percentage, SC:cG, factor_key=TRUE)
    
    ## Data by Area
    library(dplyr)
    sumdata=adata[c(1,2,3,as.numeric(4:11))]%>%
      group_by(treatment2,time) %>%
      summarise(
        count = n(),
        SC = mean(SC, na.rm = TRUE),  
        fS = mean(fS, na.rm = TRUE),
        mS = mean(mS, na.rm = TRUE),
        cS = mean(cS, na.rm = TRUE),
        fG = mean(fG, na.rm = TRUE),
        mG = mean(mG, na.rm = TRUE),
        cG = mean(cG, na.rm = TRUE))
    sumdata
    
    ## Get into long format
    data_long_mean <- gather(sumdata, sed, percentage, SC:cG, factor_key=TRUE)
    
    ## Plots
    p <- ggplot()+
      geom_line(data=data_long, aes(x=sed, y=percentage, col=time,group=ID),linetype = "dashed",size = 0.5) +# samples
      geom_line(data=data_long_mean,aes(x=sed, y=percentage,col=time, group=time),size = 2)+ #means
      scale_colour_manual(values = c("blue","#00CCCC"))+
      labs(x="Sediment",y="Percentage")+
      facet_wrap(~treatment2)+
      theme(text = element_text(size=11),legend.title = element_blank(), 
            legend.text=element_text(size=11))#a
    
    print(p)
  })  
  #__________________________________________________________________________________________
  #### PIZ NO EVIDENCE OF CHANGE (p>0.05) ####
  
  piz.no.evid <- reactive({
    
    ## Pick off sites where not sig (i.e.p>0.05)
    nochange <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p>0.05)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(nochange$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4piz <- testns4[which(testns4[,2]=="PIZ")]# PIZs where no change
    
    ## No change polygons
    pizwithnochange <- subset(test.piz(), area %in% testns4piz)
    
    return(pizwithnochange)
  })
  #__________________________________________________________________________________________
  #### SIZ NO EVIDENCE OF CHANGE (p>0.05) ####
  
  siz.no.evid <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    nochange <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p> 0.05)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(nochange$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ssiz <- testns4[which(testns4[,2]=="SIZ")]# SIZs where no change
    
    ## No change polygons
    sizwithnochange <- subset(test.siz(), area %in% testns4ssiz)
    
    return(sizwithnochange)
  })
  #__________________________________________________________________________________________
  #### REF NO EVIDENCE OF CHANGE (p>0.05) ####
  
  ref.no.evid <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    nochange <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p> 0.05)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(nochange$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ref <- testns4[which(testns4[,2]=="REF")]# REFs where no change
    
    ## No change polygons
    refwithnochange <- subset(test.ref(), area %in% testns4ref)
    
    return(refwithnochange)
  })
  #__________________________________________________________________________________________
  #### PIZ SIMILAR (p<0.05, R<0.1) ####
  
  piz.sim <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    nochange <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p<0.05)%>%filter(R<0.1)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(nochange$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4piz <- testns4[which(testns4[,2]=="PIZ")]# PIZs where no change
    
    ## No change polygons
    pizwithnochange <- subset(test.piz(), area %in% testns4piz)
    
    return(pizwithnochange)
  })
  #__________________________________________________________________________________________
  #### SIZ SIM (p<0.05, R<0.1) ####
  
  siz.sim <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    nochange <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)%>%filter(R<0.1)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(nochange$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    #testns4piz <- testns4[which(testns4[,2]=="PIZ")]# PIZs where no change
    testns4ssiz <- testns4[which(testns4[,2]=="SIZ")]# SIZs where no change
    #testns4ref <- testns4[which(testns4[,2]=="REF")]# REFs where no change
    
    ## No change polygons
    #pizwithnochange <- subset(piz, area %in% testns4piz)
    sizwithnochange <- subset(test.siz(), area %in% testns4ssiz)
    #refwithnochange <- subset(ref, area %in% testns4ref)
    return(sizwithnochange)
  })
  #__________________________________________________________________________________________
  #### REF SIM (p<0.05, R<0.1) ####
  
  ref.sim <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    nochange <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)%>%filter(R<0.1)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(nochange$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ref <- testns4[which(testns4[,2]=="REF")]# REFs where no change
    
    ## No change polygons
    refwithnochange <- subset(test.ref(), area %in% testns4ref)
    
    return(refwithnochange)
  })
  
  #__________________________________________________________________________________________
  #### PIZ SIM SOME DIFF (p<0.05, R>0.1<0.25) ####
  
  
  piz.sim.some.diff <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.1)%>%filter(R<0.25)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4piz <- testns4[which(testns4[,2]=="PIZ")]# PIZs where no change
    
    ## No change polygons
    pizwithnochange <- subset(test.piz(), area %in% testns4piz)
    
    return(pizwithnochange)
  })
  #__________________________________________________________________________________________
  #### SIZ SIM SOME DIFF (p<0.05, R>0.1<0.25) ####
  
  
  siz.sim.some.diff <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.1)%>%filter(R<0.25)
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ssiz <- testns4[which(testns4[,2]=="SIZ")]# SIZs where no change
    
    ## No change polygons
    sizwithnochange <- subset(test.siz(), area %in% testns4ssiz)
    
    return(sizwithnochange)
  })
  #__________________________________________________________________________________________
  #### REF SIM SOME DIFF (p<0.05, R>0.1<0.25) ####
  
  
  ref.sim.some.diff <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.1)%>%filter(R<0.25)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    
    testns4ref <- testns4[which(testns4[,2]=="REF")]# REFs where no change
    
    ## No change polygons
    refwithnochange <- subset(test.ref(), area %in% testns4ref)
    
    return(refwithnochange)
  })
  #__________________________________________________________________________________________
  #### PIZ DIFF SOME OVERLAP (p<0.05, R>0.25<0.5) ####
  
  
  piz.diff.some.overlap <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.25)%>%filter(R<0.5)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4piz <- testns4[which(testns4[,2]=="PIZ")]# PIZs where no change
    
    ## No change polygons
    pizwithnochange <- subset(test.piz(), area %in% testns4piz)
    
    return(pizwithnochange)
  })
  #__________________________________________________________________________________________
  #### SIZ DIFF SOME OVERLAP (p<0.05, R>0.25<0.5) ####
  
  
  siz.diff.some.overlap <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.25)%>%filter(R<0.5)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ssiz <- testns4[which(testns4[,2]=="SIZ")]# SIZs where no change
    
    ## No change polygons
    sizwithnochange <- subset(test.siz(), area %in% testns4ssiz)
    
    return(sizwithnochange)
  })
  #__________________________________________________________________________________________
  #### REF DIFF SOME OVERLAP (p<0.05, R>0.25<0.5) ####
  
  
  ref.diff.some.overlap <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.25)%>%filter(R<0.5)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ref <- testns4[which(testns4[,2]=="REF")]# REFs where no change
    
    ## No change polygons
    refwithnochange <- subset(test.ref(), area %in% testns4ref)
    
    return(refwithnochange)
  })
  #__________________________________________________________________________________________
  #### PIZ DIFF (p<0.05, R>0.5<0.75) ####
  
  
  piz.diff <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.5)%>%filter(R<0.75)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4piz <- testns4[which(testns4[,2]=="PIZ")]# PIZs where no change
    
    ## No change polygons
    pizwithnochange <- subset(test.piz(), area %in% testns4piz)
    
    return(pizwithnochange)
  })
  #__________________________________________________________________________________________
  #### SIZ DIFF (p<0.05, R>0.5<0.75) ####
  
  
  siz.diff <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.5)%>%filter(R<0.75)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ssiz <- testns4[which(testns4[,2]=="SIZ")]# SIZs where no change
    
    ## No change polygons
    sizwithnochange <- subset(test.siz(), area %in% testns4ssiz)
    
    return(sizwithnochange)
  })
  #__________________________________________________________________________________________
  #### REF DIFF (p<0.05, R>0.5<0.75) ####
  
  
  ref.diff <- reactive({
    ## Pick off sites where not sig (i.e.p>0.05)
    change <- anosim.site() %>% 
      select(site.names,R,p) %>% 
      filter(p< 0.05)
    
    change2 <- change%>%
      select(site.names,R,p) %>% 
      filter(R>0.5)%>%filter(R<0.75)
    
    
    ## Split 'site.names' col to isolate the site numbers
    library(stringr)
    out2 <- strsplit(as.character(change2$site.names),' - ')
    testns4 <- do.call(rbind,out2)
    testns4ref <- testns4[which(testns4[,2]=="REF")]# REFs where no change
    
    ## No change polygons
    refwithnochange <- subset(test.ref(), area %in% testns4ref)
    
    return(refwithnochange)
  })
  #__________________________________________________________________________________________
  #### MAP: SHOW ANOSIM SITE RESULTS ####
  
  ## Watch for
  observeEvent(input$update , {
    leafletProxy("map") %>%
      
      # Remove any previous selections 
      clearGroup("myMarkers3")%>%
      clearGroup("myMarkers4")%>%
      
      # SIZ
      addPolygons(data=siz.no.evid(),color = "#2dc937", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", siz.no.evid()$area))%>%
      addPolygons(data=siz.sim(),color = "#2dc937", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", siz.sim()$area))%>%
      addPolygons(data=siz.sim.some.diff(),color = "#99c140", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", siz.sim.some.diff()$area))%>%
      addPolygons(data=siz.diff.some.overlap(),color = "#e7b416", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", siz.diff.some.overlap()$area))%>%addPolygons(data=siz.diff(),color = "#db7b2b", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", siz.diff()$area))%>%
      
      # PIZ
      addPolygons(data=piz.no.evid(),color = "#2dc937", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", piz.no.evid()$area))%>%
      addPolygons(data=piz.sim(),color = "#2dc937", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", piz.sim()$area))%>%
      addPolygons(data=piz.sim.some.diff(),color = "#99c140", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", piz.sim.some.diff()$area))%>%
      addPolygons(data=piz.diff.some.overlap(),color = "#e7b416", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", piz.diff.some.overlap()$area))%>%addPolygons(data=piz.diff(),color = "#db7b2b", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", piz.diff()$area))%>%
      
      ## REF
      addPolygons(data=ref.no.evid(),color = "#2dc937", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", ref.no.evid()$area))%>%
      addPolygons(data=ref.sim(),color = "#2dc937", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", ref.sim()$area))%>%
      addPolygons(data=ref.sim.some.diff(),color = "#99c140", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", ref.sim.some.diff()$area))%>%
      addPolygons(data=ref.diff.some.overlap(),color = "#e7b416", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", ref.diff.some.overlap()$area))%>%addPolygons(data=ref.diff(),color = "#db7b2b", weight = 1, smoothFactor = 0.5,fillOpacity =0.8,popup = paste0("<b>Name: </b>", ref.diff()$area))%>%
      
      #addCircleMarkers
      addCircleMarkers(data = longalltest()[longalltest()$surveyname %in% input$baselineInput, ],
                       #addCircleMarkers(data = longalltest()[longalltest()$surveyname =="Baseline", ],
                       ~samplelong,
                       ~samplelat,
                       #group = "myMarkers3",
                       popup = ~paste("<b>Station Code: </b>",stationcode),
                       radius =2.5,#3.5
                       color = "blue",
                       stroke = FALSE, fillOpacity = 0.1)%>%
      addCircleMarkers(data = longalltest()[longalltest()$surveyname %in% input$monitoringInput, ],
                       #addCircleMarkers(data = longalltest()[longalltest()$surveyname == "Monitoring", ],                 
                       ~samplelong,
                       ~samplelat,
                       #group = "myMarkers4",
                       popup = ~paste("<b>Station Code: </b>",stationcode),
                       radius =1,#2
                       color = "#00CCCC",
                       stroke = FALSE, fillOpacity = 0.1)
    
  })
  #red'#cc3232','#db7b2b','#e7b416','#99c140','#2dc937','#2dc937'green
  #__________________________________________________________________________________________
} 

shinyApp(ui = ui, server = server)