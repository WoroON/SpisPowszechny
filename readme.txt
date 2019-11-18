# 1. Pierwszy kod mapy: 
# Która wyświetla mapę polski, jednak bez podziału administracyjnego na województwa:
# Nie wstawiam do głównego kodu aplikacji,ponieważ nie jest to jeszcze to czego potrzebujemy.
library(shiny)
library(leaflet)

# wygląd aplikacji (najprostszy)
ui <- fluidPage(
    
    # mapa
    leafletOutput("mymap", height = 400),
    
    # pole na tekst - tu pokażemy klikane współrzędne
    verbatimTextOutput("coordinates")
)


# mechanika aplikacji
server <- function(input, output) {
    
    # początkowe ustawienie mapy
    output$mymap <- renderLeaflet(
        leaflet() %>%
            addTiles() %>%
            # środek mapki to domyślny marker
            addMarkers(lng = 19.15, lat = 52.19) %>%
            # mapa pokazuje całą Polskę na początek
            setView(lng = 19.15, lat = 52.19, zoom = 6)
    )
    
    # czekamy na kliknięcie w mapę
    observeEvent(input$mymap_click, {
        # współrzędne kliknięcia
        click_lat <- input$mymap_click$lat
        click_long <- input$mymap_click$lng
        
        # na mapie...
        leafletProxy("mymap") %>%
            # czyścimy markery
            clearMarkers() %>%
            # ustawiamy marker w miejscu gdzie kliknięto
            addMarkers(lng = click_long, lat = click_lat)
        
        # wyświetlamy współrzędne
        output$coordinates <- renderPrint({
            cat(paste0("Long: ", click_long, "\nLat:  ", click_lat))
        })
    })
}

shinyApp(ui = ui, server = server)
