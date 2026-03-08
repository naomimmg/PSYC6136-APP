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

# Function to convert set of factors to all 
# reasonable model specifications

# NOTE: Function was primarily generated with assistance of
# GPT Ver. 4. Full chat can be seen here:
# https://chatgpt.com/share/69a86cec-7aec-8002-af41-9c6653147a9e

# I somewhat modified the code (omitted stylistic flourishes
# that were unnecessary) and made sure to understand how
# function works.
show_all_formulas <- function(factors, response = "Freq") {
    f <- factors
    f <- f[nzchar(f)] # Drops blank/missing factors
    k <- length(f) # Store number of variables
    
    # Don't execute function if there are less than 1
    # factor or more than 5
    stopifnot(k %in% 1:5)
    
    # Helper functions (each takes char vector, x, and
    # puts symbol in between each value)
    inter <- function(x) paste(x, collapse = ":")
    add   <- function(x) paste(x, collapse = " + ")
    frm   <- function(rhs) paste0(response, " ~ ", rhs)
    full_inter <- function(x) paste(x, collapse = " * ")
    
    # convenience names
    A <- f[1]
    B <- if (k >= 2) f[2] else NA_character_
    C <- if (k >= 3) f[3] else NA_character_
    D <- if (k >= 4) f[4] else NA_character_
    E <- if (k >= 5) f[5] else NA_character_
    
    # Initiate empty list to store formulas
    out <- list()
    
    # If k = 1, output only possible formula
    if(k == 1) {
        c(mutual = frm(add(f)))
    }
    
    # If k = 2, output possible formulas
    else if (k == 2) {
        return(c(
            mutual = frm(add(f)),
            saturated = frm(full_inter(f))
        ))
    }
    
    else {
    
    # mutual: [A] [B] [C] ...
    out$mutual <- frm(add(f))
    
    # Code below 
    # joint: 3-way [AB][C]; 4-way [ABC][D]; 5-way [ABCD][E]
    out$joint <- frm(add(c(inter(f[1:(k-1)]), f[k])))
    
    # joint (with=1): 3-way [A][BC]; 4-way [A][BCD]; 5-way [A][BCDE]
    out$`joint (with=1)` <- frm(add(c(A, inter(f[2:k]))))
    
    # conditional: 3-way [AC][BC]; 4-way [AD][BD][CD]; 5-way [AE][BE][CE][DE]
    out$conditional <- frm(add(sapply(f[1:(k-1)], \(x) inter(c(x, f[k])))))
    
    # conditional (with=1):
    # 3-way [AB][AC]
    # 4-way [AB][AC][AD]
    # 5-way [AB][AC][AD][AE]
    out$`conditional (with=1)` <- frm(add(sapply(f[2:k], \(x) inter(c(A, x)))))
    
    # markov (order=1): adjacent pairs [AB][BC]...[DE]
    out$`markov (order=1)` <- frm(add(sapply(1:(k-1), \(i) inter(f[i:(i+1)]))))
    
    # markov (order=2):
    # 3-way [A][B][C]
    # 4-way [ABC][BCD]
    # 5-way [ABC][BCD][CDE]
    if (k == 3) {
        out$`markov (order=2)` <- frm(add(f))
    } else {
        out$`markov (order=2)` <- frm(add(sapply(1:(k-2), \(i) inter(f[i:(i+2)]))))
    }
    
    # saturated: [ABC], [ABCD], [ABCDE]
    out$saturated <- frm(inter(f))
    
    unlist(out)
    }
}

# Function to detect whether variable is categorical
# (i.e., is character or factor type)
is.cat <- function(obj) {
    is.factor(obj) | is.character(obj)
}

# Function to extract vector of columns for categorical data
extract_cat <- function(dat) {
    names(dat)[
        sapply(dat, is.cat) |
            tolower(names(dat)) %in% c("freq", "n", "count")
    ]
}

# Function to select columns with categorical data
select_cat <- function(dat) {
    dat |> 
        dplyr::select(extract_cat(dat)) |>
        tibble::as_tibble()
}

# accident <- vcdExtra::Accident |> dplyr::mutate(numeric = rnorm(80))
# accident |> select_cat()


