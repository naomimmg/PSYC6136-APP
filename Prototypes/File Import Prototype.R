##### File import prototype #####
# Install current version of vcdExtra
# for color_table() function
# devtools::install_github("https://github.com/friendly/vcdExtra")

# Load packages
library(vcd)
library(vcdExtra)
library(DT)
library(shiny)

# DEVNOTE: Need to fit model as glm, since MASS::loglm() fails to
# extract the data found within the shiny environment
# (No one knows why;
# see https://stackoverflow.com/questions/31397663/shiny-object-from-reactive-expression-not-found-when-used-in-loglm)
# library(MASS)

library(tidyverse)

# Load requisite functions
source("Functions.R")
# Import datasets
source("data.R")

ui <- fluidPage(
    fileInput("upload", NULL, accept = c(".csv", ".tsv")),
    # Preview initially downloaded data
    DT::dataTableOutput("preview"),
    
    # Create custom ui for selecting variables later on
    uiOutput("var_select_inputs"),
    
    # Preview data with categorical vars selected
    DT::dataTableOutput("preview_cat"),
    
    # Preview table
    tableOutput("tab_preview"),
    
    # Create mosaic with table data
    # (to see if works with data)
    plotOutput("mosaic_plot")
    
)

server <- function(input, output, session) {
    
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        
        switch(
            ext,
            csv = vroom::vroom(input$upload$datapath, delim = ","),
            tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
            validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    
    output$preview <- DT::renderDataTable({
        req(data())
        data()
    })
    
    # Create reactive object to save names in data to pull from
    all_choices <- reactive({
        req(data())
        names(data())
    })
    
    observe({
        req(data())
        
        vars_choices <- setdiff(all_choices(), input$select_freq)
        freq_choices <- setdiff(all_choices(), input$select_variables)
        
        updateSelectInput(
            session,
            "select_variables",
            choices = vars_choices,
            selected = intersect(input$select_variables, vars_choices)
        )
        
        updateSelectInput(
            session,
            "select_freq",
            choices = freq_choices,
            selected = intersect(input$select_freq, freq_choices)
        )
    })
    
    output$var_select_inputs <- renderUI({
        req(data())
        
        tagList(
            selectInput(
                "select_freq",
                label = "Select the variable that corresponds to frequency in the data set",
                choices = all_choices(),
                multiple = FALSE
            ),
            
            selectInput(
                "select_variables",
                label = "Select all of the CATEGORICAL variables in your imported data",
                choices = all_choices(),
                multiple = TRUE
            )
        )
    })
    
    cat_dat <- reactive({
        req(data(), input$select_freq, input$select_variables)
        
        data() |>
            dplyr::select(all_of(c(input$select_freq, input$select_variables)))
    })
    
    data_tab <- reactive({
        req(data(), input$select_freq, input$select_variables)
        
        # Transform data frame to table with xtabs
        # 1) create formula
        form <- as.formula(paste(input$select_freq, "~", paste(input$select_variables, collapse = " + ")))
        
        # 2) apply formula to xtabs
        xtabs(form, data = data())
    })
    
    output$preview_cat <- DT::renderDataTable({
        req(cat_dat())
        cat_dat()
    })
    
    output$tab_preview <- renderTable({
        req(data_tab())
        data_tab()
    })
    
    output$mosaic_plot <- renderPlot({
        req(data_tab())
        
        vcd::mosaic(data_tab(), gp = vcd::shading_Friendly())
    })
}

shinyApp(ui, server)

