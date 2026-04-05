# Explicitly add path to find image to ensure RStudio is reading it
addResourcePath("www", "www")

ui <- fluidPage(
    useShinyjs(), 
    tags$head(
        tags$style(HTML("
            .nav-tabs li:not(.active) a { color: #999 !important; background-color: #f8f9fa !important; cursor: not-allowed !important; }
            .nav-tabs li:not(.active) { pointer-events: none !important; }
            .nav-tabs li.active a { color: #000 !important; font-weight: bold; background-color: #fff !important; }
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
                    HTML('
                        <div style="font-size:12px; color:#555; text-align:left; margin-top:10px;">
                        Created by Gabriel Crone & Naomi Martinez Gutierrez.<br>
                        Inspired by Michael Friendly\'s 
                        <a href="http://euclid.psych.yorku.ca/cgi/mosaics" target="_blank"><i>mosaics</i></a> visualization tool.
                        </div>
                    ')
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
                fileInput("upload", "Upload CSV/TSV", accept = c(".csv", ".tsv")),
                uiOutput("var_select_inputs")
            ),
            
            hr(),
            h4("Step Guide"),
            tags$small("1. Choose/Upload Data"), br(),
            tags$small("2. Verify Levels & Select Factors"), br(),
            tags$small("3. Build Statistical Model & Customize Plot")
        ),
        
        mainPanel(
            width = 9,
            tabsetPanel(
                id = "tabs",
                tabPanel("1. Data Selection", value = "tab1", br(),
                         shinyjs::hidden(
                             div(id = "tab1_contents",
                                 h4("Data Preview"),
                                 uiOutput("table_preview"),
                                 hr(),
                                 div(style = "float: right;", 
                                     actionButton("next1", "Next: Data Cleaning", class = "btn-primary"))
                             )
                         )
                ),
                
                tabPanel("2. Data Cleaning", value = "tab2", br(),
                         h4("Factor Levels & Variable Selection"),
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
                                                     choices = c("No customization", "Select among defaults", "Write custom formula")),
                                        
                                        conditionalPanel(
                                            condition = "input.customize_formula_options == 'Write custom formula'",
                                            textOutput("preview_vars"),
                                            textInput("custom_formula", "Enter formula:", placeholder = "Freq ~ A + B + C")
                                        ),
                                        
                                        conditionalPanel(
                                            condition = "input.customize_formula_options == 'Select among defaults'",
                                            uiOutput("select_formula_ui"),
                                            textOutput("formula_preview")
                                        ),
                                        
                                        selectInput("residual_type", "Residual type:", choices = NULL),
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
                                        conditionalPanel(
                                            condition = "input.customize_formula_options != 'No customization'",
                                            checkboxInput("show_g_square", "Display G² statistic?", FALSE)
                                        ),
                                        checkboxGroupInput("split", "Split Directions:", 
                                                           choices = c("Horizontal" = "H", "Vertical" = "V"), 
                                                           selected = c("H", "V")),
                                        hr(),
                                        actionButton("back3", "Back", class = "btn-default")
                                    )
                             ),
                             column(8,
                                    plotOutput("mosaic_plot")
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