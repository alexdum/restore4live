ui_home <- tagList(
    tags$div(
        class = "container-fluid home-page",
        style = "padding: 2rem;",
        tags$div(
            class = "text-center mb-5",
            tags$h1("Danube basin", class = "display-3"),
            tags$p("Explore climate and hydrological data for the Danube basin.", class = "lead")
        ),
        layout_column_wrap(
            width = 1 / 3,
            heights_equal = "row",
            card(
                class = "home-card action-link",
                id = "link_climate_scenario",
                card_header(
                    class = "bg-primary text-white text-center",
                    icon("chart-line", "fa-3x mb-3"),
                    tags$h4("Climate scenario")
                ),
                card_body(
                    "Visualize and analyze climate scenarios data."
                ),
                style = "cursor: pointer;"
            ),
            card(
                class = "home-card action-link",
                id = "link_hydrological_scenario",
                card_header(
                    class = "bg-info text-white text-center",
                    icon("water", "fa-3x mb-3"),
                    tags$h4("Hydrological scenario")
                ),
                card_body(
                    "Explore hydrological projections and scenarios."
                ),
                style = "cursor: pointer;"
            ),
            card(
                class = "home-card action-link",
                id = "link_climate_observational",
                card_header(
                    class = "bg-success text-white text-center",
                    icon("temperature-high", "fa-3x mb-3"),
                    tags$h4("Climate observational data")
                ),
                card_body(
                    "Access historical climate observational data."
                ),
                style = "cursor: pointer;"
            ),
            card(
                class = "home-card action-link",
                id = "link_remote_sensing",
                card_header(
                    class = "bg-warning text-white text-center",
                    icon("satellite", "fa-3x mb-3"),
                    tags$h4("Remote Sensing")
                ),
                card_body(
                    "View remote sensing data and NDVI maps."
                ),
                style = "cursor: pointer;"
            ),
            card(
                class = "home-card action-link",
                id = "link_aoi",
                card_header(
                    class = "bg-secondary text-white text-center",
                    icon("map-marked-alt", "fa-3x mb-3"),
                    tags$h4("AoI")
                ),
                card_body(
                    "Define and explore the Area of Interest."
                ),
                style = "cursor: pointer;"
            ),
            card(
                class = "home-card action-link",
                id = "link_about",
                card_header(
                    class = "bg-dark text-white text-center",
                    icon("info-circle", "fa-3x mb-3"),
                    tags$h4("About")
                ),
                card_body(
                    "Learn more about this project and the data sources."
                ),
                style = "cursor: pointer;"
            )
        )
    ),
    # Add script to handle div clicks as input for Shiny
    tags$script(HTML("
    $(document).on('click', '.home-card', function() {
      var id = $(this).attr('id');
      Shiny.setInputValue(id, Math.random());
    });
  "))
)
