

# Define server logic required to draw a histogram
server <- function(input, output) {

    #functie leaflet de start
    output$map <- renderLeaflet({
        leaflet_fun()
    })
}


