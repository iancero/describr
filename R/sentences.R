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


