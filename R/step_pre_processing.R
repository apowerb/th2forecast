#' Step pre processing
#'
#' @param recipe recipe
#' @param ... formula
#' @param role role
#' @param skip skipe
#' @param trained trained
#' @param columns columns
#' @param id id
#'
#' @return a fonction renvoie un dataset propre
#' @export
#'
#' @examples
step_th2_pre_processing <-
  function(recipe,
           ...,
           role = NA,
           skip = FALSE,
           trained = FALSE,
           columns = NULL,
           id = recipes::rand_id("th2_pre_processing")) {
    recipes::add_step(
      recipe,
      step_th2_pre_processing_new(
        terms = enquos(...),
        role = role,
        skip = skip,
        trained = trained,
        columns = columns,
        id = id
      )
    )
  }

step_th2_pre_processing_new <-
  function(terms,
           role,
           skip,
           trained,
           columns,
           id) {
    recipes::step(
      subclass = "th2_pre_processing",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      columns = columns,
      id = id
    )
  }

#' @export
prep.step_th2_pre_processing <- function(x,
                                         training,
                                         info = NULL,
                                         ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  step_th2_pre_processing_new(
    terms = x$terms,
    role = x$role,
    skip = x$skip,
    trained = TRUE,
    columns = col_names,
    id = x$id
  )
}

#' @export
bake.step_th2_pre_processing <- function(object,
                                         new_data,
                                         ...) {
  # print(object)
  # print(dim(new_data))
  for (col_name in object$columns) {
    if (all(is.na(new_data[[col_name]])) == FALSE) {
      pre_proces_data <- preprocessing_data(new_data)[["dataset_clean"]]

      new_data[[col_name]] <- pre_proces_data[[col_name]]
    }
  }


  if (!tibble::is_tibble(new_data)) {
    new_data <- tibble::as_tibble(new_data)
  }
  # print(new_data)

  new_data
}

#' @export
print.step_th2_pre_processing <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Preprocessing data for columns", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

# tidy.step_th2_pre_processing <- function(x, ...) {
#   if (is_trained(x)) {
#     res <- tibble(terms = x$columns)
#   } else {
#     res <- tibble(terms = sel2char(x$terms))
#   }
#   res
# }
