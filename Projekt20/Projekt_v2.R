library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
datatab <- read.csv("dane5.csv", sep=',')
data <- read.csv("dane4.csv",sep=";")
map <- readOGR("jednostki_administracyjne/Województwa.shp")
data$kod_woj<-as.factor(data$kod_woj)
levels(data$kod_woj)<-c("02","04","06","08","10","12","14","16","18","20","22","24","26","28","30","32")
# ui object
ui <- fluidPage(
  titlePanel("Statystyka Kryminalna "),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variableselected",
        label = "Przestępstwa:",
        choices = c("Wykryte", "Stwierdzone")
      ),
      selectInput(
        inputId = "yearselected",
        label = "Wybierz rok",
        choices = 1999:2017
      ),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa i Graf",leafletOutput(outputId = "map"),dygraphOutput(outputId = "timetrend")),
        tabPanel("Tablica",DTOutput(outputId = "table"))
  )
)
))

# server()
server <- function(input, output) {
  output$table <- renderDT(datatab)
  
  output$timetrend <- renderDygraph({
    dataxts <- NULL
    counties <- unique(data$Jednostka_podzialu_administracyjnego)
    for (l in 1:length(counties)) {
      datacounty <- data[data$Jednostka_podzialu_administracyjnego == counties[l], ]
      dd <- xts(
        datacounty[, input$variableselected],
        as.Date(paste0(datacounty$Rok, "-01-01"))
      )
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- counties
    
    dygraph(dataxts) %>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.2) -> d1
    
    d1$x$css <- "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "
    d1
  })
  
  output$map <- renderLeaflet({
    
    # Add data to map
    
    datafiltered <- data[which(data$Rok == input$yearselected), ]
    ordercounties <- match(map@data$JPT_KOD_JE, datafiltered$kod_woj)
    map@data <- datafiltered[ordercounties, ]
    
    # Create variableplot
    # ADD this to create variableplot
    map$variableplot <- as.numeric(
      map@data[, input$variableselected])
    
    # Create leaflet
    
    pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7)
    
    
    labels <- sprintf("%s: %g", map$Jednostka_podzialu_administracyjnego, map$variableplot) %>%
      lapply(htmltools::HTML)
    
    
    l <- leaflet(map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)


