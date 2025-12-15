## The forward stepwise model selection function:

forward_select_lm <- function(df, response, candidates) {
  data <- as.data.frame(df)
  in_model   <- character(0)
  best_model <- lm(as.formula(paste(response, "~ 1")), data = data)
  best_aic   <- AIC(best_model)
  
  repeat {
    remaining <- setdiff(candidates, in_model)
    if (length(remaining) == 0) break
    
    aic_vec <- numeric(length(remaining))
    for (j in seq_along(remaining)) {
      vars <- c(in_model, remaining[j])
      form <- as.formula(paste(response, "~", paste(vars, collapse = " + ")))
      fit  <- lm(form, data = data)
      aic_vec[j] <- AIC(fit)
    }
    
    j_best  <- which.min(aic_vec)
    new_aic <- aic_vec[j_best]
    
    if (new_aic >= best_aic) break
    
    in_model   <- c(in_model, remaining[j_best])
    best_aic   <- new_aic
    best_model <- lm(
      as.formula(paste(response, "~", paste(in_model, collapse = " + "))),
      data = data
    )
  }
  
  best_model
}