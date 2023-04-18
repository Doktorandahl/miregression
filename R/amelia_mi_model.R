

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
amelia_mi_model <- function(object, data, use_only_model_vars = TRUE, conf.int = FALSE, ...){

  if(use_only_model_vars){

    data <- data %>% dplyr::select(all.vars(formula(object)))
  }
  mi_data <- Amelia::amelia(as.data.frame(data),...)

  mi_mods <- foreach::foreach(i = 1:length(mi_data$imputations)) %do%
    update(object,data=mi_data$imputations[[i]])

  Amelia::mi.combine(mi_mods,conf.int = conf.int)


}
