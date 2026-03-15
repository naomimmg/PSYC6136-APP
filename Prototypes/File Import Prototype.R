##### File import prototype #####
# Install current version of vcdExtra
# for color_table() function
# devtools::install_github("https://github.com/friendly/vcdExtra")

# Load packages
library(vcd)
library(vcdExtra)
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
    # Number of rows to preview
    numericInput("n", "Rows", value = 5, min = 1, step = 1),
    # Preview data
    tableOutput("preview"),
    
    # Preview data with ONLY categorical variables (+ freq) selected
    tableOutput("preview_cat"),
    
    # 
    
)

server <- function(input, output, session) {
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    
    output$preview <- renderTable({
        head(data(), input$n)
    })
    
    # Reactively save data set version with 
    # only categorical variables
    cat_dat <- reactive({
        req(input$upload)
        
        # If one of the variables is NOT frequency,
        # then it means data is case format, 
        # so need to convert to long format
        select_cat(data())
    })
    
    # Print out data with only the categorical variables
    output$preview_cat <- renderTable({
        head(cat_dat(), input$n)
    })
}

shinyApp(ui, server)

