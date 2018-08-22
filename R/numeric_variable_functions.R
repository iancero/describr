numeric_statistics <- function(df, stats) {
  purrr::map(df, function(var) purrr::map(stats, function(stat) stat(var))) %>%
    purrr::map(as.data.frame) %>%
    dplyr::bind_rows(.id = 'var')
}

