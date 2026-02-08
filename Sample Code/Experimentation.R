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


# Import sample data sets
# 1) Employment Status (table)
Employment <- vcd::Employment

# 2) Infection in Caesarean Births (table)
Caesar <- vcdExtra::Caesar

# 3) Titanic Data (df)
Titanic_df <- vcdExtra::Titanicp |>
    mutate(age = case_when(
        age >= 15 ~ "Adult",
        age < 15 ~ "Child",
        TRUE ~ NA
    ))
Titanic <- Titanic_df |>
    count(sex, age, survived, pclass) |>
    xtabs(formula = n ~ sex + age + survived + pclass)

structable(pclass ~ sex + age + survived, 
           data = Titanic)

# 4) Suicide Data
Suicide <- vcd::Suicide |>
    xtabs(formula = Freq ~ sex + method + age.group)

# 5) Berkeley Admission Data (table)
UCBAdmissions <- datasets::UCBAdmissions

# 6) Divorce Data
Divorce <- vcd::PreSex

# 7) Abortion Opinion Data
Abortion <- vcdExtra::Abortion

# 8) HairEyeSex Data
HairEyeSex_df <- datasets::HairEyeColor |>
    as.data.frame()
HairEyeSex <- xtabs(Freq ~ Eye + Hair + Sex, data = HairEyeSex_df)
# structable(HairEyeSex, Sex + Eye ~ Hair)

# 9) HairEye Data
HairEye <- datasets::HairEyeColor

# 10) Heart Disease Data
Heart <- vcdExtra::Heart

# List of all data sets
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
    uiOutput("table_preview")
)

server <- function(input, output, session) {
    select_dat <- reactive({
        data_sets[[input$select_sample_dat]]
        })
    
    output$table_preview <- renderUI({
        color_table(select_dat())
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
# formula <- as.symbol("Freq ~ EmploymentStatus * EmploymentLength * LayoffCause")
# 
# # fit_1 <- MASS::loglm(, data = Employment)
# mosaic(fit_1, gp = shading_Friendly())
