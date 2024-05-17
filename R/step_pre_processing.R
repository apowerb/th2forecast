library("recipes")

step_th2_pre_processing_new <-
  function(terms   = NULL,
           role    = NA,
           skip    = FALSE,
           trained = FALSE,
           columns = NULL) {
    recipes::step(
      subclass = "th2_pre_processing",
      terms    = terms,
      role     = role,
      skip     = skip,
      trained  = trained,
      columns  = columns
    )
  }

step_th2_pre_processing <-
  function(recipe,
           ...,
           role    = NA,
           skip    = FALSE,
           trained = FALSE,
           columns = NULL) {
    recipes::add_step(
      recipe,
      step_th2_pre_processing_new(
        terms   = enquos(...),
        role    = role,
        skip    = skip,
        trained = trained,
        columns = columns
      )
    )
  }

prep.step_th2_pre_processing <- function(x,
                                     training,
                                     info = NULL,
                                     ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  step_th2_pre_processing_new(
    terms   = x$terms,
    role    = x$role,
    skip    = x$skip,
    trained = TRUE,
    columns = col_names
  )
}

bake.step_th2_pre_processing <- function(object,
                                     new_data,
                                     ...) {
  new_data <- preprocessing_data(new_data)[["dataset_clean"]]

  as_tibble(new_data)
}

print.step_th2_pre_processing <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Preprocessing data for columns", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

tidy.step_th2_pre_processing <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
