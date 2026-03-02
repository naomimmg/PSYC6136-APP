# Function detects all unique levels of each factor variable
detect_levels <- function(dat, is_table = TRUE) {
    if (!is_table) {
        lapply(dat, unique)
    } else {
        dat |>
            as.data.frame() |>
            select(-any_of(c("n", "Freq"))) |>
            lapply(unique)
    }
}

# Function converts list of vectors into a formula
# based upon the selected variables
# For xtabs
convert_xtabs_formula <- function(factors) {
  as.formula(paste("Freq ~", paste(factors, collapse = " + ")))
}

# For loglm
convert_loglm_formula <- function(factors) {
  as.formula(paste("~", paste(factors, collapse = " + ")))
}
