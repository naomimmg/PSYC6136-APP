library(shinyjs)
library(colourpicker)

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
        windowTitle = "Categorical Data Analysis",
        tagList(
            div(style = "display: flex; align-items: center; padding-top: 10px; margin-bottom: 20px;",
                img(src = "logo.png", height = "100px", style = "margin-right: 20px;"),
                div(h1("Categorical Data Analysis", style = "font-weight: bold; margin: 0; display: inline-block;"))
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
            tags$small("3. Build Statistical Model"), br(),
            tags$small("4. Customize Mosaic Display")
        ),
        
        mainPanel(
            width = 9,
            conditionalPanel(
                condition = "input.select_sample_dat != '' || input.upload != null",
                tabsetPanel(
                    id = "tabs",
                    tabPanel("1. Data Selection", value = "tab1", br(),
                             uiOutput("table_preview"),
                             hr(),
                             div(style = "float: right;", 
                                 actionButton("next1", "Next: Data Cleaning", class = "btn-primary"))
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
                                 actionButton("next2", "Next: Model Specification", class = "btn-primary"))
                    ),
                    
                    tabPanel("3. Model Specification", value = "tab3", br(),
                             wellPanel(
                                 radioButtons("customize_formula_options", "Model Formula Customization:",
                                              choices = c("No customization", "Select among defaults", "Write custom formula")),
                                 
                                 conditionalPanel(
                                     condition = "input.customize_formula_options == 'Write custom formula'",
                                     textInput("custom_formula", "Enter formula:", placeholder = "Freq ~ A + B + C")
                                 ),
                                 
                                 conditionalPanel(
                                     condition = "input.customize_formula_options == 'Select among defaults'",
                                     uiOutput("select_formula_ui")
                                 ),
                                 
                                 selectInput("residual_type", "Residual type:", choices = NULL)
                             ),
                             hr(),
                             actionButton("back3", "Back", class = "btn-default"),
                             div(style = "float: right;", 
                                 actionButton("next3", "Next: Mosaic Plot", class = "btn-primary"))
                    ), 
                    
                    tabPanel("4. Mosaic Plot", value = "tab4", br(),
                             column(4, 
                                    wellPanel(
                                        selectInput("shading_type", "Shading Style:",
                                                    choices  = c("Friendly", "Friendly2", "sieve", "custom")),
                                        conditionalPanel(
                                            condition = "input.shading_type == 'custom'",
                                            colourpicker::colourInput("color_1", "+ve Residuals", value = "#0072B2"),
                                            colourpicker::colourInput("color_2", "-ve Residuals", value = "#F70D20")
                                        ),
                                        checkboxInput("show_residuals", "Show residual labels?", TRUE),
                                        checkboxInput("show_formula", "Display formula title?", TRUE),
                                        checkboxInput("show_g_square", "Display G² statistic?", FALSE),
                                        checkboxGroupInput("split", "Split Directions:", 
                                                           choices = c("Horizontal" = "H", "Vertical" = "V"), 
                                                           selected = c("H", "V"))
                                    ),
                                    actionButton("back4", "Back", class = "btn-default")
                             ),
                             column(8, plotOutput("mosaic_plot"))
                    )
                )
            ), 
            conditionalPanel(
                condition = "input.select_sample_dat == ''",
                div(style = "text-align: center; margin-top: 100px; color: #7f8c8d;",
                    icon("table", class = "fa-4x"),
                    h3("Welcome! Please select (or upload) a dataframe on the sidebar to begin."))
            )
        )
    )
)
