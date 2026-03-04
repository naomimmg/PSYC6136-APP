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
    # Allow user to specify if they want to provide a formula
    checkboxInput("customize_formula_on", "Specify full formula?", value = FALSE),
    
    conditionalPanel(
        condition = "input.customize_formula_on == true",
        textInput("custom_formula", "Enter formula:", placeholder = "Freq ~ A + B + C")
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
        color_table(select_dat())
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
    
    # FOR TESTING, see what mod_dat() outputs
    # output$mod_dat_preview <- renderText(mod_dat())
    
    # Specify mod_dat_formula
    mod_dat_form <- reactive({
        if(input$customize_formula_on) {
            req(input$custom_formula)
            as.formula(input$custom_formula)
        }
        else
        mod_dat() |>
            detect_levels() |>
            names() |>
            convert_loglm_formula()
    })
    
    # Output the final mosaic display
    output$mosaic <- renderPlot({
        # If users specify formula
        if (input$customize_formula_on) {
            req(input$custom_formula)
            
            df  <- as.data.frame(select_dat())
            
            mod.glm <- glm(
                formula = mod_dat_form(),
                data    = df,
                family  = poisson()
            )
            
            vcd::mosaic(mod.glm, data = df, gp = vcd::shading_Friendly())
        } else {
            vcd::mosaic(mod_dat(), gp = vcd::shading_Friendly())
        }
    })
    
    
    # output$mosaic <- renderPlot({
    #     # If user specified formula, use specified
    #     # formula within informed plot
    #     if (input$customize_formula_on) {
    #         req(input$custom_formula)
    #         mod.glm <- glm(formula = mod_dat_form(), data = as.data.frame(select_dat()))
    #         
    #         vcd::mosaic(mod.glm, data = select_dat(), gp = vcd::shading_Friendly())
    #     # Otherwise, plot based on the mod_dat()
    #     # (selected columns, or default)
    #     } else {
    #         mosaic(
    #             mod_dat(),
    #             gp = vcd::shading_Friendly()
    #         )
    #     }
    # })
}

shinyApp(ui, server)
# NOTES:
# Employment_df <- Employment |> 
#     tibble::as.tibble()
# 
# 
# 
# new_levels <- c("Unemployed", "NewJob")
# 
# 
# Employment_df <- Employment_df |>
#     dplyr::mutate(EmploymentStatus = factor(EmploymentStatus, levels = c("")))
# 
# formula <- as.formula("Freq ~ EmploymentStatus + EmploymentLength * LayoffCause")
# 
# fit_1 <- MASS::loglm(Freq ~ ., data = Employment)
# 
# mosaic(fit_1, gp = shading_Friendly())

