
ui <- 
  page_fillable(
    layout_sidebar(
      sidebar = sidebar(
      ),
      
      # Show results
      
      card(
        full_screen = T,
        leafletOutput("map")
      )
      
      
      # )
    )
  )