sam <- data_sets[["Employment"]]

detect_levels(sam, is_table = TRUE)

vec <- c("EmploymentLength", "LayoffCause")

# Function converts list of vectors into a formula
# based upon the selected variables
convert_formula <- function(vars) {
  stopifnot(length(vars) >= 2)
  
  lhs <- vars[1]
  rhs <- paste(vars[-1], collapse = " + ")
  
  as.formula(paste(lhs, "~", rhs))
}

convert_formula(vec)

xtabs(formula = convert_formula(vec), data = sam)
