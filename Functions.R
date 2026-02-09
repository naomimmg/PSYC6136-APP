detect_levels <- function(dat, is_table = FALSE) {
    if (!is_table) {
        lapply(dat, unique)
    } else {
        dat |>
            as.data.frame() |>
            select(-any_of(c("n", "Freq"))) |>
            lapply(unique)
    }
}
