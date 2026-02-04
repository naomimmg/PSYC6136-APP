##### Figuring out how to emulate app #####

# Load packages
library(vcd)
library(vcdExtra)
library(tidyverse)

# Load requisite functions

# Import sample data sets
# 1) Employment Status (table)
Employment <- vcd::Employment

# 2) Infection in Caesarean Births (table)
Caesar <- vcdExtra::Caesar

# 3) Titanic Data (df)
Titanic <- vcdExtra::Titanicp

# 4) Suicide Data (df)
Suicide <- vcd::Suicide

# 5) Berkeley Admission Data (table)
UCBAdmissions <- datasets::UCBAdmissions

# 6) Divorce Data

# 7) Abortion Opinion Data

# 8) HairEyeSex Data

# 9) Heart Disease Data



### 

Employment_df <- Employment |> 
    tibble::as.tibble()



new_levels <- c("Unemployed", "NewJob")


Employment_df <- Employment_df |>
    dplyr::mutate(EmploymentStatus = factor(EmploymentStatus, levels = c("")))

formula <- as.symbol("Freq ~ EmploymentStatus * EmploymentLength * LayoffCause")

fit_1 <- MASS::loglm(formula, data = Employment)
mosaic(fit_1, gp = shading_Friendly())
