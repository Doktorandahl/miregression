#' Multiply imputed regression using Amelia
#' @description
#' Function for running regression analysis with mulitply imputed data using Amelia
#'
#'
#' @param object A fitted model object (such as a regression model), e.g. an lm or glm object. Should work with any model object compatible with the update() function
#' @param data The original data frame including missing values
#' @param use_only_model_vars Logical: TRUE (the default) indicates that only variables in the model are used for imputation. Note: if you use time series or cross-sectional variables which may be useful for the imputation but which are not in the model you need to set this to FALSE.
#' @param conf.int Logical indicating if confidence intervals should be computed for each regression coefficient (default is FALSE).
#' @param ... further arguments to be passed to amelia. See the documentation for \link[Amelia]{amelia} to see these options. Note, if you have nominal or ordinal variables (factors) in the model which need to be imputed you need to explicitly tell Amelia that these variables are factors by providing the arguments noms (for nominal variables) or ords (for ordinal variables) which should be a vector of column names (variable names) or positions of the factor variables.
#'
#' @return Returns a tibble containing the estimate, standard error, statistic, p.value, and other information. For a detailed description of what is returned, see \link[Amelia]{mi.combine}. Note that some common regression output such as the R2 values, RMSE, etc are not returned since they cannot be interpreted in the regular way when using multiple imputation
#' @export
#'
#' @examples model <- lm(y~x1+x2,data=df)
#' mi_results <- amelia_mi_model(object=model,data=df)
amelia_mi_model <- function(
  object,
  data,
  use_only_model_vars = TRUE,
  conf.int = FALSE,
  ...
) {
  if (use_only_model_vars) {
    data <- data %>% dplyr::select(all.vars(formula(object)))
  }
  mi_data <- Amelia::amelia(as.data.frame(data), ...)

  mi_mods <- foreach::foreach(i = 1:length(mi_data$imputations)) %do%
    update(object, data = mi_data$imputations[[i]])

  mi.combine_debugged(mi_mods, conf.int = conf.int)
}

#' Use stargazer to make output
#'
#' @param object an amelia_mi_model object
#' @param digits number of digits to round to
#' @param include_missingess_diagnostics Include missingness diagnostics,  df, r, and miss.info, in the table
#' @param remove_rows Rows to remove from the output (eg fixed effects)
#' @param ... Other arguments to be passed to stargazer
#'
#' @return Stargazer table of the model object
#' @export
#'
#' @examples stargazer_amelia_mi_model(m1,type='text',out='table.txt')
stargazer_amelia_mi_model <- function(
  object,
  digits = 3,
  include_missingess_diagnostics = F,
  remove_rows = NULL,
  ...
) {
  object[, 2:8] <- round(object[, 2:8], digits)
  if (!is.null(remove_rows)) {
    object <- object[-remove_rows, ]
  }
  if (!include_missingess_diagnostics) {
    object <- object[, 1:5]
  }
  stargazer::stargazer(object, summary = FALSE, rownames = F, ...)
}


mi.combine_debugged <- function(x, conf.int = FALSE, conf.level = 0.95) {
  if (requireNamespace("broom", quietly = TRUE)) {
    tidiers <- grep("^tidy\\.", ls(getNamespace("broom")), value = TRUE)
    tidiers <- gsub("tidy\\.", "", tidiers)
  } else {
    rlang::abort("{broom} package required for mi.combine")
  }
  if (any(!(class(x[[1L]]) %in% tidiers))) {
    rlang::abort("analysis model does not have tidy() method.")
  }
  mi_tidy <- lapply(x, function(x) broom::tidy(x))
  m <- length(mi_tidy)
  out <- mi_tidy[[1L]]
  ests <- est.matrix(mi_tidy, "estimate")
  ses <- est.matrix(mi_tidy, "std.error")
  wi.var <- rowMeans(ses^2)
  out$estimate <- rowMeans(ests)
  diffs <- sweep(ests, 1, rowMeans(ests))
  bw.var <- rowSums(diffs^2) / (m - 1)
  out$std.error <- sqrt(wi.var + bw.var * (1 + 1 / m))
  r <- ((1 + 1 / m) * bw.var) / wi.var
  df <- (m - 1) * (1 + 1 / r)^2
  miss.info <- (r + 2 / (df + 3)) / (r + 1)
  out$statistic <- out$estimate / out$std.error
  out$p.value <- 2 * stats::pt(out$statistic, df = df, lower.tail = FALSE)
  out$df <- df
  out$r <- r
  out$miss.info <- miss.info
  if (conf.int) {
    t.c <- stats::qt(1 - (1 - conf.level) / 2, df = df, lower.tail = FALSE)
    out$conf.low <- out$estimate - t.c * out$std.error
    out$conf.high <- out$estimate + t.c * out$std.error
  }
  out
}
