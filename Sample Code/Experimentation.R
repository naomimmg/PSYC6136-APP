##### Figuring out how to emulate app #####

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

# Compile all data sets together
data_sets <- list(Employment, Caesar, Titanic, Suicide, 
                  UCBAdmissions, Divorce, Abortion, HairEyeSex, 
                  Heart, HairEye)
names(data_sets) <- c("Employment", "Caesar", "Titanic", "Suicide", 
                      "UCBAdmissions", "Divorce", "Abortion", "HairEyeSex", 
                      "Heart", "HairEye")

# Create mini-app to allow users to
# select a data set, and have it preview the
# data set as a color table
ui <- fluidPage(
    selectInput("select_sample_dat", "Select a Sample Dataset:", 
                names(data_sets)),
    uiOutput("table_preview"),
    "Factors and their Levels:",
    verbatimTextOutput("level_preview"),
    selectInput("select_factors", 
                label = "Which factors from the data would you like to include in the mosaic display? (If unspecified, will use all factors.)",
                # Server will generate choices based on data
                choices = NULL, 
                multiple = TRUE),
    # verbatimTextOutput("mod_dat_preview"),
    # Allow user to specify if they want to provide a formula, and if so,
    # whether want defaults or customize their own
    radioButtons("customize_formula_options", "How would you like to customize the formula for the model?",
                choices = c("No customization", "Select among defaults", "Write custom formula")),
    
    # If writing custom formula, allow user to do so
    conditionalPanel(
        condition = "input.customize_formula_options == 'Write custom formula'",
        textInput("custom_formula", "Enter formula:", placeholder = "Freq ~ A + B + C")
    ),
    
    # If selecting custom formula, allow user to do so
    conditionalPanel(
        condition = "input.customize_formula_options == 'Select among defaults'",
        uiOutput("select_formula")
    ),
    
    # Plot final mosaic plot
    plotOutput("mosaic")
)

server <- function(input, output, session) {
    # Save selected data to a reactive object
    select_dat <- reactive({
        data_sets[[input$select_sample_dat]]
        })
    
    # Output color table preview
    output$table_preview <- renderUI({
        color_table(select_dat(),
                    legend = "")
        })
    
    # Preview levels reactively
    preview_levels <- reactive(
        select_dat() |>  detect_levels(is_table = TRUE)
    )
    
    # Output level previews
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
    
    # Update input selection options for select_factors
    observeEvent(select_dat(), {
        freezeReactiveValue(input, "select_factors")
        updateSelectInput(
            session = session,
            inputId = "select_factors",
            # The choices are the column names of the table
            # as a data frame, omitting frequency (frequency)
            # is assumed
            choices = names(preview_levels()))
    })
    
    # Save reactive object of modified data table
    mod_dat <- reactive({
        if(is.null(input$select_factors)) select_dat()
        # If selected factors to specify
        else if(!is.null(input$select_factors)) {
            # Modify data so it only shows those factors
            xtabs(convert_xtabs_formula(input$select_factors), data = select_dat())
        }
        
    })
    
    # Save character vector of names from the specific
    # data set
    dat_factors <- reactive({
        mod_dat() |>
            detect_levels() |>
            names()
    })
    
    # Save vector of all reasonable formula names
    reasonable_formulas <- reactive({
        show_all_formulas(dat_factors())
    })
    
    # Update UI to support all reasonable defaults
    output$select_formula <- renderUI({
        req(input$customize_formula_options == "Select among defaults")
        
        selectInput(
            inputId = "selected_formula",
            label   = "Choose a default formula:",
            choices = reasonable_formulas()   # names shown, values returned
        )
    })
    
    # Specify mod_dat_formula
    mod_dat_form <- reactive({
        if(input$customize_formula_options == "Write custom formula") {
            req(input$custom_formula)
            as.formula(input$custom_formula)
        }
        
        else if ((input$customize_formula_options == "Select among defaults") ) {
            req(input$selected_formula)
            as.formula(input$selected_formula)
        }
        else {
            dat_factors() |> convert_loglm_formula()
        }
        
    })
    
    # Output the final mosaic display
    output$mosaic <- renderPlot({
        # If users specify formula
        if (input$customize_formula_options != "No customization") {
            df  <- as.data.frame(select_dat())
            
            mod.glm <- glm(
                formula = mod_dat_form(),
                data    = df,
                family  = poisson()
            )
            
            # Save formula for title
            form_txt <- paste(deparse(mod_dat_form()), collapse = "")
            
            suppressWarnings(vcd::mosaic(mod.glm, data = df, gp = vcd::shading_Friendly(), main = form_txt, cex.main = 0.8))
        }
        
        else {
            vcd::mosaic(mod_dat(), gp = vcd::shading_Friendly())
        }
    })
}

shinyApp(ui, server)
