# Load required libraries
library(vcd)
library(vcdExtra)
library(shiny)
library(tidyverse)
library(shinyjs)
library(colourpicker)

# Source required files
source("Functions.R")
source("data.R")

# Explicitly add path to find image to ensure RStudio is reading it
addResourcePath("www", "www")

ui <- fluidPage(
    shinyjs::useShinyjs(), 
    tags$head(
        tags$style(HTML("
            .nav-tabs li:not(.active) a { color: #999 !important; background-color: #f8f9fa !important; cursor: not-allowed !important; }
            .nav-tabs li:not(.active) { pointer-events: none !important; }
            .nav-tabs li.active a { color: #000 !important; font-weight: bold; background-color: #fff !important; }
            .resizable-plot { 
            resize: both; 
            overflow: auto; 
            border: 1px solid #ddd; 
            background: white;
            width: 100%;
            height: 600px; 
            min-height: 300px;
            padding-bottom: 15px; 
            }

            .resizable-plot .shiny-plot-output, 
            .resizable-plot .shiny-plot-output img {
            height: 100% !important;
            width: 100% !important;
            }
        "))
    ),
    
    titlePanel(
        windowTitle = "Shiny Mosaics",
        tagList(
            div(
                style = "display: flex; align-items: center; padding-top: 10px; margin-bottom: 20px;",
                img(src = "www/logo.png", height = "100px", style = "margin-right: 20px;"),
                div(
                    h1("Shiny Mosaics", style = "font-weight: bold; margin: 0;"),
                    h6("Version 1.0.0"),
                    HTML('<div style="font-size:12px; color:#555; text-align:left; margin-top:10px;">
    Created by Gabriel Crone & Naomi Martinez Gutierrez.<br>
    All code is available on <a href="https://github.com/naomimmg/PSYC6136-APP" target="_blank"> Github. </a>  <br>
    Inspired by Michael Friendly\'s 
    <a href="http://euclid.psych.yorku.ca/cgi/mosaics" target="_blank"><i>mosaics</i></a> visualization tool.<br>
</div>')
                )
            )
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            style = "background-color: #f8f9fa;",
            h3("Data Input"),
            radioButtons("data_option", "Data Source:",
                         choices = c("Use sample data", "Upload data"),
                         selected = "Use sample data"),
            
            conditionalPanel(
                condition = "input.data_option == 'Use sample data'",
                selectInput("select_sample_dat", "Choose Sample Data:", 
                            choices = c("Select..." = "", names(data_sets)))
            ),
            
            conditionalPanel(
                condition = "input.data_option == 'Upload data'",
                
                tags$small(
                    HTML('Data must be <b>.csv/.tsv</b> and structured in '),
                    actionLink(
                        inputId = "freq_help",
                        label = HTML("<b>frequency format</b>.")
                    )
                ),
                fileInput("upload", "Upload CSV/TSV", accept = c(".csv", ".tsv")),
                uiOutput("var_select_inputs")
            ),
            
            h4("Step Guide"),
            tags$small("1. Choose/Upload Data"), br(),
            tags$small("2. Verify Levels & Select Factors"), br(),
            tags$small("3. Build Statistical Model & Customize Plot"), br(),
            tags$small(
                h4("Navigation"),
                tags$div(
                    tags$span(
                        class = "btn btn-primary btn-xs",
                        style = "pointer-events: none; cursor: default;",
                        "Next"
                    ),
                    " = go forward"
                ),
                
                tags$div(
                    tags$span(
                        class = "btn btn-default btn-xs",
                        style = "pointer-events: none; cursor: default;",
                        "Back"
                    ),
                    " = go back"
                )
            )
        ),
        
        mainPanel(
            width = 9,
            tabsetPanel(
                id = "tabs",
                tabPanel("1. Data Selection", value = "tab1", br(),
                         shinyjs::hidden(
                             div(id = "tab1_contents",
                                 h4("Table Preview"),
                                 uiOutput("table_preview"),
                                 hr(),
                                 div(style = "float: left;", 
                                     actionButton("next1", "Next: Factor Selection", class = "btn-primary"))
                             )
                         )
                ),
                
                tabPanel("2. Factor Selection", value = "tab2", br(),
                         h4("Factor Selection & Levels"),
                         verbatimTextOutput("level_preview"),
                         selectInput("select_factors", 
                                     label = "Factors to include in display (default is all):",
                                     choices = NULL, multiple = TRUE),
                         hr(),
                         actionButton("back2", "Back", class = "btn-default"),
                         div(style = "float: right;", 
                             actionButton("next2", "Next: Model & Plot", class = "btn-primary"))
                ),
                
                tabPanel("3. Model & Plot", value = "tab3", br(),
                         fluidRow(
                             column(4,
                                    wellPanel(
                                        h4("Model Specification"),
                                        radioButtons("customize_formula_options", "Model Formula Customization:",
                                                     choices = c("No customization", "Select model type", "Write custom formula")),
                                        
                                        conditionalPanel(
                                            condition = "input.customize_formula_options == 'Write custom formula'",
                                            textOutput("preview_vars"),
                                            textInput("custom_formula", "Enter formula:", placeholder = "Freq ~ A + B + C")
                                        ),
                                        
                                        conditionalPanel(
                                            condition = "input.customize_formula_options == 'Select model type'",
                                            uiOutput("select_formula_ui"),
                                            textOutput("formula_preview")
                                        ),
                                        
                                        selectInput("residual_type", "Residual Type:", choices = NULL),
                                        hr(),
                                        h4("Mosaic Plot Options"),
                                        selectInput("shading_type", "Shading Style:",
                                                    choices  = c("Friendly", "Friendly2", "sieve", "custom")),
                                        conditionalPanel(
                                            condition = "input.shading_type == 'custom'",
                                            colourpicker::colourInput("color_1", "+ve Residuals", value = "#0072B2"),
                                            colourpicker::colourInput("color_2", "-ve Residuals", value = "#F70D20")
                                        ),
                                        checkboxInput("show_residuals", "Show residual labels?", TRUE),
                                        checkboxInput("show_formula", "Display formula title?", TRUE),
                                        checkboxInput("split", "Change X and Y axis?", FALSE),
                                        
                                        conditionalPanel(
                                            condition = "input.customize_formula_options != 'No customization'",
                                            checkboxInput("show_g_square", "Display G² statistic?", FALSE)
                                        ),
                                        hr(),
                                        actionButton("back3", "Back", class = "btn-default")
                                    )
                             ),
                             column(8,
                                    div(class = "resizable-plot",
                                        style = "position: relative;",
                                        div(style = "position: absolute; right: 10px; top: 5px; z-index: 1000;",
                                            downloadButton("downloadPNG", "PNG", class = "btn-xs"),
                                            downloadButton("downloadSVG", "SVG", class = "btn-xs")
                                        ),
                                        plotOutput("mosaic_plot", height = "auto")
                                    )
                             )
                         )
                )
            ),
            shinyjs::hidden(
                div(id = "welcome_panel",
                    style = "text-align: center; margin-top: 100px; color: #7f8c8d;",
                    icon("table", class = "fa-4x"),
                    h3("Welcome! Please select (or upload) a dataframe on the sidebar to begin."))
            )
        )
    )
)
