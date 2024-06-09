

output$about <- renderUI({
  tags$iframe(src = "html/about.html", height = "100%", width = "100%")
})