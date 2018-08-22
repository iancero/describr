# library(testthat)

context('Numeric variable function tests')

df <- mtcars[, c('mpg', 'hp', 'wt')]
df[sample(1:nrow(df), size = .10*nrow(df)), ] <- NA

test_that(
  desc = 'numeric_statistics() should return statistics that are requested',
  code = {
    stats <- list(
      mean = function(x) mean(x, na.rm = T),
      SD = function(x) sd(x, na.rm = T),
      missing = function(col) mean(is.na(col)))
    num_stat_df <- numeric_statistics(df, stats)

    expect_equal(names(num_stat_df), c('var', names(stats)))
    expect_equivalent(num_stat_df$mean, sapply(df, mean, na.rm = T))
    expect_equivalent(num_stat_df$SD, sapply(df, sd, na.rm = T))
    expect_equivalent(
      object = num_stat_df$missing,
      expected = sapply(df, function(x) mean(is.na(x))))
  }
)


