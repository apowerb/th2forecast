library("recipes")

step_th2_feature_engineering_new <-
  function(terms   = NULL,
           role    = NA,
           skip    = FALSE,
           trained = FALSE,
           feature_target = NULL,
           columns = NULL) {
    recipes::step(
      subclass = "th2_feature_engineering",
      terms    = terms,
      role     = role,
      skip     = skip,
      trained  = trained,
      feature_target = feature_target,
      columns  = columns
    )
  }

step_th2_feature_engineering <-
  function(recipe,
           ...,
           role    = NA,
           skip    = FALSE,
           trained = FALSE,
           feature_target = "",
           columns = NULL) {
    recipes::add_step(
      recipe,
      step_th2_feature_engineering_new(
        terms   = enquos(...),
        role    = role,
        skip    = skip,
        trained = trained,
        feature_target = feature_target,
        columns = columns
      )
    )
  }

prep.step_th2_feature_engineering <- function(x,
                                         training,
                                         info = NULL,
                                         ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  step_th2_feature_engineering_new(
    terms   = x$terms,
    role    = x$role,
    skip    = x$skip,
    trained = TRUE,
    feature_target = x$feature_target,
    columns = col_names
  )
}

bake.step_th2_feature_engineering <- function(object,
                                         new_data,
                                         ...) {
  new_data <- feature_selection(new_data, feature_target = object$feature_target)

  as_tibble(new_data)
}

print.step_th2_feature_engineering <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Feature engineering for columns", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

tidy.step_th2_feature_engineering <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
