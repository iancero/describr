# library(testthat)

context('Numeric variable function tests')

df <- mtcars[, c('mpg', 'hp', 'wt')]
df[sample(1:nrow(df), size = .10*nrow(df)), ] <- NA

stats <- list(
  mean = function(x) mean(x, na.rm = T),
  SD = function(x) sd(x, na.rm = T),
  missing = function(col) mean(is.na(col)))

num_stat_df <- numeric_statistics(df, stats)
sentence_list <- numeric_sentences(df, stats, as_list = T)
sentence_df <- numeric_sentences(df, stats, as_list = F)

test_that(
  desc = 'numeric_statistics() should return statistics that are requested',
  code = {
    expect_equal(dim(num_stat_df), c(length(df), length(stats) + 1))
    expect_equal(names(num_stat_df), c('var', names(stats)))
    expect_equivalent(num_stat_df$mean, sapply(df, mean, na.rm = T))
    expect_equivalent(num_stat_df$SD, sapply(df, sd, na.rm = T))
    expect_equivalent(
      object = num_stat_df$missing,
      expected = sapply(df, function(x) mean(is.na(x))))
  }
)

test_that(
  desc = 'numeric_sentences returns appropriate output type',
  code = {
    expect_true(inherits(sentence_list, 'list'))
    expect_true(inherits(sentence_df, 'data.frame'))
    expect_false(inherits(sentence_list, 'data.frame'))
    expect_false(inherits(sentence_df, 'list'))
  }
)

test_that(
  desc = 'numeric_sentences(as_list = T) returns correct names and length',
  code = {
    expect_equal(length(sentence_list), length(df))
    expect_equal(names(sentence_list), names(df))
  }
)

