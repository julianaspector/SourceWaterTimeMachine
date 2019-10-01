library(lubridate)
library(leaflet)
library(dplyr)
library(shiny)
library(timeDate)
library(baydeltautils)
library(data.table)

# bring in SacWAM results

sacWAM_base <- read.csv("sac_wam_postprocessor_model_1_2.csv", skip=11)

# convert index to machine readable date
sacWAM_base$Index <- ymd(sacWAM_base$Index)

# bring in station locations

stationLoc <- read.csv("Station_Coordinates.csv")

# bring in water year types table

wyTypes <- read.csv("water_year_types.csv")

# select relevant columns for SacWAM data and merge station location with station data

FPT <- sacWAM_base %>% select(c(Index,Flow=Sacramento.River.209...SWRCB.Sac.AT.Freeport)) 
FPT$CDEC.Station <- "FPT"
FPT <- merge(FPT, stationLoc, by="CDEC.Station")

YBY <- sacWAM_base %>% select(c(Index, Flow=Yolo.Bypass..22...Reach))
YBY$CDEC.Station <- "YBY"
YBY <- merge(YBY, stationLoc, by="CDEC.Station")

PUT <- sacWAM_base %>% select(c(Index,Flow=Putah.Creek..21...SWRCB.Putah.Creek)) 
PUT$CDEC.Station <- "PUT"
PUT <- merge(PUT, stationLoc, by="CDEC.Station")

CSN <- sacWAM_base %>% select(c(Index,Flow=Cosumnes.River...5...FNF.Cosumnes.at.Michigan.Bar..gauge.)) 
CSN$CDEC.Station <- "CSN"
CSN <- merge(CSN, stationLoc, by="CDEC.Station")

DLC <- sacWAM_base %>% select(c(Index, Flow=Delta.Cross.Channel.fr.Sacramento.River.RM.030...0...Headflow)) 
DLC$CDEC.Station <- "DLC"
DLC <- merge(DLC, stationLoc, by="CDEC.Station")

GSS <- sacWAM_base %>% select(c(Index, Flow=Georgiana.Slough.fr.Sacramento.River.RM.029...0...Headflow)) 
GSS$CDEC.Station <- "GSS"
GSS <- merge(GSS, stationLoc, by="CDEC.Station")

WBR <- sacWAM_base %>% select(c(Index,Flow=Mokelumne.River..39...REG.Mokelumne.blw.Woodbridge)) 
WBR$CDEC.Station <- "WBR"
WBR <- merge(WBR, stationLoc, by="CDEC.Station")

QMISC <- sacWAM_base %>% select(c(Index,Flow=Calaveras.River..29...SWRCB.Calaveras.River)) 
QMISC$CDEC.Station <- "QMISC"
QMISC <- merge(QMISC, stationLoc, by="CDEC.Station")

IDB <- sacWAM_base %>% select(c(Index, Flow=Old.River.Pipeline...0...Headflow)) 
IDB$CDEC.Station <- "IDB"
IDB <- merge(IDB, stationLoc, by="CDEC.Station")                                      

INB <- sacWAM_base %>% select(c(Index,Flow=Rock.Slough.Intake...0...Headflow)) 
INB$CDEC.Station <- "INB"
INB <- merge(INB, stationLoc, by="CDEC.Station") 

WCI <- sacWAM_base %>% select(c(Index, Flow=Clifton.Court.Forebay)) 
WCI$CDEC.Station <- "WCI"
WCI <- merge(WCI, stationLoc, by="CDEC.Station")
WCI <- WCI %>% mutate_at(c("Flow"), as.factor)

SJR <- sacWAM_base %>% select(c(Index, Flow=San.Joaquin.River...1...Inflow.at.Vernalis.Inflow)) 
SJR$CDEC.Station <- "SJR"
SJR <- merge(SJR, stationLoc, by="CDEC.Station")

BKS <- sacWAM_base %>% select(c(Index, Flow=North.Bay.Aqueduct...0...Headflow))
BKS$CDEC.Station <- "BKS"
BKS <- merge(BKS, stationLoc, by="CDEC.Station")
BKS <- BKS %>% mutate_at(c("Flow"), as.factor)

TRP <- sacWAM_base %>% select(c(Index, Flow=Delta.SOD.Export))
TRP$CDEC.Station <- "TRP"
TRP <- merge(TRP, stationLoc, by="CDEC.Station")
TRP <- TRP %>% mutate_at(c("Flow"), as.factor)

# separate data into imports and exports

data <- rbind(FPT, YBY, PUT, CSN, DLC, GSS, WBR, QMISC, SJR, IDB, INB, WCI)
imports <- rbind(FPT, YBY, PUT, CSN, DLC, GSS, WBR, QMISC, SJR)
exports <- rbind(IDB, INB, WCI, BKS, TRP)


imports <- imports %>% mutate_at (c("Flow"), as.numeric)
imports <- imports %>% mutate_at (c("Name"), as.character)

exports <- exports %>% mutate_at (c("Flow"), as.numeric)
exports <- exports %>% mutate_at (c("Name"), as.character)

data <- data %>% mutate_at (c("Flow"), as.numeric)

imports$Units <- "TAF"
exports$Units <- "TAF"

imports$Label <- "Delta Imports"
exports$Label <- "Delta Exports"


imports$WY <- waterYear(imports$Index)
exports$WY <- waterYear(exports$Index)
data$WY <- waterYear(data$Index)

# combine imports and exports for accessing later
imports_exports <- rbind(imports, exports)

data <- merge(data, wyTypes, by="WY")
imports <- merge(imports, wyTypes, by="WY")
exports <- merge(exports, wyTypes, by="WY")
imports_exports <- merge(imports_exports, wyTypes, by="WY")

# determin min and max dates for each slider bar
data_dt <- data.table::data.table(data)

data_maxDate <- data_dt[,max(Index.x), by=WYT]
data_maxDate <- setorder(data_maxDate, WYT)
data_minDate <- data_dt[,min(Index.x), by=WYT]
data_minDate <- setorder(data_minDate, WYT)

# set wyTypes to be integers for indices and numeric for WY
wyTypes <- wyTypes %>% select(c("WY", "WYT"))


# names of sliders

xAxisGroup <- c("Water Year Type 1", "Water Year Type 2", "Water Year Type 3", "Water Year Type 4", "Water Year Type 5")


# create match table
WY_match <- data.table(WYT=c(1, 2, 3, 4, 5), Description=c("Wet", "Above Normal", "Below Normal", "Dry", "Critical"))

wyTypes <- merge(WY_match, wyTypes, by="WYT")

# rearrange columns
wyTypes <- wyTypes %>%
  select(-WYT, -Description, everything())

# sort by year

wyTypes <- wyTypes[order(WY),]

# divide water year types into two groups for tables
wyTypes_one <- wyTypes[1:47,]
wyTypes_two <- wyTypes[48:94,]

ui <- fluidPage(
  tabsetPanel(
    tabPanel(title="Maps",
             titlePanel("Sacramento/San Joaquin Delta Water Imports and Exports"),
             sidebarLayout(
               sidebarPanel(
                 uiOutput("sliders")),
               mainPanel(
                 fluidRow(
                   column(4, leafletOutput("map_one")),
                   column(4, leafletOutput("map_two")),
                   column(4, leafletOutput("map_three"))),
                 fluidRow(
                   column(6, leafletOutput("map_four")),
                   column(6, leafletOutput("map_five")))
               )
             )
    ),
    tabPanel(title="Water Year Information",
             fluidRow(
               column(4, tableOutput("table_one")),
               column(4, tableOutput("table_two")),
               column(4, tableOutput("table_three")),
               column(4, tableOutput("table_four"))
             )
    )
  )
)


server<-function(input,output,session){
  
  #Render the sliders
  output$sliders <- renderUI({
    # First, create a list of sliders each with a different name
    sliders <- lapply(1:length(xAxisGroup), function(i) {
      inputName <- xAxisGroup[i]
      sliderInput(inputName, inputName, min=as.Date(data_minDate$V1[i]), max=as.Date(data_maxDate$V1[i]), value=as.Date(data_minDate$V1[i]), timeFormat="%b %Y")
    })
    # Create a tagList of sliders (this is important)
    do.call(tagList, sliders)
  })
  
  # subset the data based on user choice
  dataInput_one <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 1")) & imports_exports$WYT== 1,]
  })
  dataInput_two <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 2")) & imports_exports$WYT== 2,]
  })
  dataInput_three <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 3")) & imports_exports$WYT== 3,]
  })
  dataInput_four <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 4")) & imports_exports$WYT== 4,]
  })
  dataInput_five <- reactive({
    imports_exports[imports_exports$Index.x==as.character(timeLastDayInMonth(input$"Water Year Type 5")) & imports_exports$WYT== 5,]
  })
  
  # render basemap and legend for each water year type (static)
  
  output$map_one <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat))%>%
      addLegend(data=dataInput_one(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 1")
  })
  
  output$map_two <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_two(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 2")
  })
  
  output$map_three <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_three(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 3")
  })
  output$map_four <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_four(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 4")
  })
  output$map_five <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(Long),~min(Lat), ~max(Long), ~max(Lat)) %>%
      addLegend(data=dataInput_five(),"topright", colors=c("#67a9cf", "#ef8a62"), labels=unique((imports_exports$Label)), title="Water Year Type 5")
  })
  
  # color palette for maps
  pal <- colorFactor(palette=c("#ef8a62", "#67a9cf"), levels=sort(imports_exports$Label))
  
  # interactive data for maps
  observe({
    map<-leafletProxy("map_one")%>%
      clearShapes() %>%
      addCircles(data=dataInput_one(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                       "<br>", "Flow:", as.character(Flow), "<br>", "Units:", as.character(Units)))
  })
  
  observe({
    map<-leafletProxy("map_two")%>%
      clearShapes() %>%
      addCircles(data=dataInput_two(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                       "<br>", "Flow:", as.character(Flow),
                                                                                       "<br>", "Units:", as.character(Units)))
  })
  observe({
    map<-leafletProxy("map_three")%>%
      clearShapes() %>%
      addCircles(data=dataInput_three(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                         "<br>", "Flow:", as.character(Flow),
                                                                                         "<br>", "Units:", as.character(Units)))  
  })
  observe({
    map<-leafletProxy("map_four")%>%
      clearShapes() %>%
      addCircles(data=dataInput_four(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                        "<br>", "Flow:", as.character(Flow),
                                                                                        "<br>", "Units:", as.character(Units)))
  })
  observe({
    map<-leafletProxy("map_five")%>%
      clearShapes() %>%
      addCircles(data=dataInput_five(), radius=~Flow*2, color=~pal(Label), popup=~paste("Station Name:",as.character(Name),
                                                                                        "<br>", "Flow:", as.character(Flow),
                                                                                        "<br>", "Units:", as.character(Units)))
    
  })
  
  output$table_one <- renderTable({
    head(wyTypes_one, n = 23)}, bordered=TRUE, digits=0)
  
  output$table_two <- renderTable({
    tail(wyTypes_one, n = -23)}, bordered=TRUE, digits=0)
  
  output$table_three <- renderTable({
    head(wyTypes_two, n = 23)}, bordered=TRUE, digits=0)
  
  output$table_four <- renderTable({
    tail(wyTypes_two, n = -23)}, bordered=TRUE, digits=0)
  
}


shinyApp(ui, server)