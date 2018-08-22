# library(testthat)

context('Test that sentences.R functions work appropriately')

# df <- mtcars[, c('mpg', 'hp', 'wt')]
# df[sample(1:nrow(df), size = .10*nrow(df)), ] <- NA
#
# stats <- list(
#   mean = function(x) mean(x, na.rm = T),
#   SD = function(x) sd(x, na.rm = T),
#   missing = function(col) mean(is.na(col)))

test_that(
  desc = 'clause() appends elements appropriately',
  code = {
    expect_equal(clause(1, 2, 3), '1 2 3')
    expect_equal(clause(1:3, 2:4, 3:5), c('1 2 3', '2 3 4', '3 4 5'))
    expect_equal(
      object = clause(1:3, 2:4, 3:5, sep = '-'),
      expect = c('1-2-3', '2-3-4', '3-4-5'))
  }
)

test_that(
  desc = 'stat_clause() compiles statistics clauses correctly',
  code = {
    named_vec <- c(a = 1, b = 2, c = 3)
    expect_equal(stat_clause(named_vec), c('a = 1', 'b = 2', 'c = 3'))
    expect_equal(stat_clause(named_vec, 'x'), c('a x 1', 'b x 2', 'c x 3'))
  }
)

test_that(
  desc = 'stat_clause() throws error if stat_vec is unnamed',
  code = {
    unnamed_vec <- 1:3
    expect_error(stat_clause(unnamed_vec))
  }
)

test_that(
  desc = 'collapse_clauses() collapses input correctly',
  code = {
    expect_equal(collapse_clauses(letters[1:3]), 'a b c')
    expect_equal(collapse_clauses(letters[1:3], collapse = ','), 'a,b,c')
  }
)

test_that(
  desc = 'addon_add() adds "and" to final element of a list',
  code = {
    expect_equal(addon_and(letters[1:3]), c('a', 'b', 'and c'))
    expect_equal(addon_and(letters[1:3], collapse = ' '), 'a b and c')
  }
)
