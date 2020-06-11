#' @import purrr
#' @import stats
#' @importFrom magrittr %>%
#' @import future
#' @import furrr
#' @import vroom
#' @importFrom utils capture.output
#' @details
#' Generalized Linear Model with Little Bag of Bootstraps and Parallelization
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' Fit generalized linear models using bootstrap
#'
#' @param formula formula
#' @param data dataframe, list
#' @param m numeric
#' @param B numeric
#' @param family family
#'
#' @return blbglm
#' @export
#'
#' @examples
#' fit <- blbglm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, family = gaussian)
blbglm <- function(formula, data, m = 10, B = 200, family = gaussian) {

  # check if data is a dataframe or list of file path
  if (is.data.frame(data)) {
    data_list <- split_data(data, m)
  }
  else {
    # read data from file path
    data_file <- data %>% future_map(~ vroom(., col_types = cols()))
    # split data from each filepath then bind. Note that data now is in smaller chunks
    data_list <- data_file %>%
      future_map(~ split_data(., m)) %>%
      future_map(~ bind_rows(.))
  }

  estimates <- data_list %>% future_map(
    ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B, family = gaussian)
  )
  res <- list(estimates = estimates, formula = formula, family = gaussian)
  class(res) <- "blbglm"
  invisible(res)
}


#' Split data into m parts of approximated equal sizes
#'
#' @param data dataframe
#' @param  m numeric
#'
#' @return list of dataframe
#'
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula formula
#' @param data dataframe, list
#' @param n numeric
#' @param B numeric
#' @param family family
#'
glm_each_subsample <- function(formula, data, n, B, family) {
  replicate(B, glm_each_boot(formula, data, n, family), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula formula
#' @param data dataframe, list
#' @param n numeric
#' @param family family
#'
glm_each_boot <- function(formula, data, n, family) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs, family)
}


#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula formula
#' @param data dataframe, list
#' @param freqs numeric
#' @param family family
#'
glm1 <- function(formula, data, freqs, family) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = family)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#'
#' @param fit blbglm
#'
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#'
#' @param fit blbglm
#'
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' Displays blbglm model formula
#'
#' @export
#' @method print blbglm
#' @param x blbglm
#' @param ... arg  arg
#'
print.blbglm <- function(x, ...) {
  cat("blbglm model:", capture.output(x$formula))
  cat("\n")
}


#' Computes variance of coefficients with confidence interval at alpha = 1-level
#'
#' @export
#' @method sigma blbglm
#'
#' @param object blbglm
#' @param confidence logical
#' @param level numeric
#' @param ... arg
#'
#'
sigma.blbglm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(future_map_dbl(est, ~ mean(future_map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(future_map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Extracts model coefficients
#'
#' @export
#' @method coef blbglm
#'
#' @param object blbglm
#' @param ... arg
#'
coef.blbglm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' Confidence Intervals For Model Parameters
#'
#' @export
#' @method confint blbglm
#'
#' @param object blbglm
#' @param level numeric
#' @param parm list
#' @param ... arg
#'
confint.blbglm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ future_map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Obtains predictions and optionally estimates standard errors of those predictions from a fitted generalized linear model object.
#'
#' @export
#' @method predict blbglm
#'
#' @param  object blbglm
#' @param new_data dataframe
#' @param confidence logical
#' @param level numeric
#' @param ... arg
#'
predict.blbglm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (future_map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(rbind)
}
