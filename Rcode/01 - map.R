library(shiny)
library(leaflet)
library(countrycode)
library(png)

ui <- fluidPage(
  titlePanel("Click for country names and flags"),
  leafletOutput("worldMap", height = "600px"),
  uiOutput("countryInfo")
)

server <- function(input, output, session) {
  #Rendering the world map and setting the initial perspective
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
                  layerId = ~iso_a2,
                  fillColor = "lightblue", color = "white", weight = 1) %>%
      setView(lng = 0, lat = 20, zoom = 2)  # 设置初始视野
  })
  
  #Responding to map click events
  observeEvent(input$worldMap_shape_click, {
    country_code <- input$worldMap_shape_click$id
    if (!is.null(country_code)) {
      country_name <- countrycode(country_code, "iso2c", "country.name")
      
      #Download and display flags
      flag_url <- paste0("https://flagcdn.com/w320/", tolower(country_code), ".png")
      
      output$countryInfo <- renderUI({
        tagList(
          h3(country_name),
          img(src = flag_url, height = "150px", alt = country_name)
        )
      })
    }
  })
}

shinyApp(ui = ui, server = server)
