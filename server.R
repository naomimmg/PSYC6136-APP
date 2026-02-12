server <- function(input, output, session) {
  
  select_dat <- reactive({
    data_sets[[input$select_sample_dat]]
  })
  
  output$table_preview <- renderUI({
    vcdExtra::color_table(select_dat())
  })
  
  preview_levels <- reactive(
    select_dat() |>  detect_levels(is_table = TRUE)
  )
  
  output$level_preview <- renderPrint({
    cat(paste(
      lapply(names(preview_levels()), function(nm) {
        paste0(
          nm, ": ", paste(preview_levels()[[nm]], collapse = ", ")
        )
      }),
      collapse = "\n\n"
    ))
  })
  
  output$mosaic_plot <- renderPlot({
    vcd::mosaic(select_dat(), 
                shading = TRUE,
                split_vertical = "V" %in% input$split,
                main = paste("Mosaic Plot of", input$select_sample_dat))
  })
}
