clause <- function(noun, verb, obs, sep = ' '){
  paste(noun, verb, obs, sep = sep)
}

stat_clause <- function(stat_vec, verb = '=', sep = ' '){
  if (is.null(names(stat_vec))){
    stop('names(stat_vec) is NULL, need names for stat_vec')
  }
  clause(names(stat_vec), verb, unlist(stat_vec), sep = sep)
}

collapse_clauses <- function(strings, collapse = ' ') {
  paste(strings, collapse = collapse)
}

addon_and <- function(strings, collapse = NULL) {
  strings[length(strings)] <- paste('and', strings[length(strings)])

  paste(strings, collapse = collapse)
}

numeric_sentence <- function(var_name, stat_vec, prep = '', verb = 'was') {
  pattern <- 'The {stat1} {prep} {var_name} {verb} {num1} ({other_stats}).'
  stat1 <- names(stat_vec)[1]
  num1 <- stat_vec[[1]]
  other_stats <- stat_vec[2:length(stat_vec)] %>%
    stat_clause() %>%
    collapse_clauses(collapse = ', ')

  pattern %>%
    glue::glue() %>%
    gsub('\\s{2,}', ' ', x = .)
}

numeric_sentences <- function(data, prep = '', verb = 'was'){
  purrr::pmap(
    .l = list(data, names(data), prep, verb),
    .f = ~ numeric_sentence(..2, stat_vec = ..1, prep = ..3, verb = ..4))
}

# x <- list(mean = 1.1, SD = 2.2, min = 3.3, max = 4.4)
# y <- list(mean = 11.1, SD = 22.2, min = 33.3, max = 44.4)

# data <- list(x = x, y = y)


numeric_sentences(data, verb = 'is', prep = 'of')

#
# c(1:3)[[1]]
#
# x <- list(mean = 1.1, SD = 2.2, min = 3.3, max = 4.4)
#
# library(glue)
#
# s1 <- names(x)[1]
# var <- 'age'
# v1 <- x[[s1]]
# remaining_stats <- collapse_clauses(stat_clause(x[2:length(x)]), ', ')
# glue::glue('The {s1} {var} was {v1} ({remaining_stats})')
