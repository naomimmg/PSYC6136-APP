##### Figuring out how to emulate app #####

# Install current version of vcdExtra
# for color_table() function
# devtools::install_github("https://github.com/friendly/vcdExtra")

# Load packages
library(vcd)
library(vcdExtra)
library(shiny)
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
    verbatimTextOutput("level_preview")
)

server <- function(input, output, session) {
    select_dat <- reactive({
        data_sets[[input$select_sample_dat]]
        })
    
    output$table_preview <- renderUI({
        color_table(select_dat())
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

