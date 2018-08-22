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

test_that(
  desc = 'numeric_sentence() delivers appropriately formatted sentence',
  code = {
    stat_vec <- list(mean = 1.1, SD = 2.2, min = 3.3, max = 4.4)
    expect_type(numeric_sentence('age', stat_vec), 'character')
    expect_equal(
      object = numeric_sentence('age', stat_vec),
      expected = 'The mean age was 1.1 (SD = 2.2, min = 3.3, max = 4.4).')
    expect_equal(
      object = numeric_sentence('age', stat_vec, prep = 'of', verb = 'is'),
      expected = 'The mean of age is 1.1 (SD = 2.2, min = 3.3, max = 4.4).')
  }
)

test_that(
  desc = 'numeric_sentence() should not include two sequential whitespaces',
  code = {
    stat_vec <- list(mean = 1.1, SD = 2.2, min = 3.3, max = 4.4)
    expect_false(any(grepl('\\s{2,}', numeric_sentence('age', stat_vec))))
  }
)

test_that(
  desc = 'numeric_sentences() delivers appropriate sentence output',
  code = {
    data <- list(
      x = list(mean = 1.1, SD = 2.2, min = 3.3, max = 4.4),
      y = list(mean = 11.1, SD = 22.2, min = 33.3, max = 44.4))
    expect_type(numeric_sentences(data), 'list')
    expect_equal(
      object = numeric_sentences(data),
      expected = list(
        x = 'The mean x was 1.1 (SD = 2.2, min = 3.3, max = 4.4).',
        y = 'The mean y was 11.1 (SD = 22.2, min = 33.3, max = 44.4).'))
    expect_equal(
      object = numeric_sentences(data, prep = 'of', verb = 'is'),
      expected = list(
        x = 'The mean of x is 1.1 (SD = 2.2, min = 3.3, max = 4.4).',
        y = 'The mean of y is 11.1 (SD = 22.2, min = 33.3, max = 44.4).'))
  }
)
