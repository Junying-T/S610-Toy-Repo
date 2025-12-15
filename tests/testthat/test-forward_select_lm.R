source(normalizePath(file.path("..", "..", "R", "forward_select_lm.R")))
library(testthat)

test_that("forward_select_lm returns a valid lm object", {
  set.seed(1)
  n <- 200
  d <- data.frame(
    y  = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  
  fit <- forward_select_lm(d, "y", c("x1","x2","x3"))
  
  expect_s3_class(fit, "lm")
  expect_true("y" %in% all.vars(formula(fit)))
})

test_that("AIC improves relative to null model", {
  set.seed(2)
  n <- 300
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
  )
  d$y <- rnorm(n)
  
  fit_null <- lm(y ~ 1, data = d)
  fit_sel  <- forward_select_lm(d, "y", c("x1","x2","x3"))
  
  expect_lte(AIC(fit_sel), AIC(fit_null))
})

'
> testthat::test_dir("tests/testthat")
✔ | F W  S  OK | Context
✔ |          3 | forward_select_lm                                             

══ Results ════════════════════════════════════════════════════════════════════
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]

'