fit <- blbglm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

test_that("blbglm ruturns an object of class blbglm", {
  expect_s3_class(fit, "blbglm")
})

test_that("coef.blbglm runs well", {
  co <- coef(fit)
  expect_equal(length(co), 4)
})

test_that("print.blbglm runs well", {
  expect_output(print(fit), cat("blbglm model:mpg ~ wt * hp"))
  cat("\n")
})

test_that("sigma.blbglm runs well", {
  expect_length(sigma(fit, confidence = TRUE), 3)
})



df_bin <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
df_bin$rank <- as.factor(df_bin$rank)
fit_bin <- blbglm(admit ~ gre + gpa + rank, data = df_bin, m = 5, B = 10, family = "binomial")
test_that("blbglm runs well for logistic regression", {
  expect_s3_class(fit_bin, "blbglm")
  co <- coef(fit_bin)
  expect_equal(length(co), 6)
})

cases <-
  structure(list(
    Days = c(
      1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L,
      8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L,
      23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L,
      42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L,
      49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L,
      67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L,
      80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L,
      95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L,
      106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L
    ),
    Students = c(
      6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L,
      4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L,
      3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L,
      5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L,
      5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L,
      0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L,
      2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L,
      0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L
    )
  ), .Names = c("Days", "Students"), class = "data.frame", row.names = c(NA, -109L))

test_that("blbglm runs well for poisson regression", {
  fit_poisson <- blbglm(Students ~ Days, data = cases, m = 5, B = 10, family = "poisson")
  expect_s3_class(fit_poisson, "blbglm")
  co <- coef(fit_poisson)
  expect_equal(length(co), 2)
})
