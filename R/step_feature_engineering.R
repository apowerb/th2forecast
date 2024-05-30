#' Step feature engineering
#'
#' @param recipe recipe
#' @param ... formula
#' @param role role
#' @param skip skip
#' @param trained trained
#' @param feature_target feature_target
#' @param columns columns
#' @param id id
#'
#' @return a fonction renvoie un dataset avec les caractéristiques importantes
#' @export
#'
#' @examples
step_th2_feature_engineering <-
  function(recipe,
           ...,
           role = "predictor",
           skip = FALSE,
           trained = FALSE,
           feature_target = "",
           use_holidays = TRUE,
           all_data = "",
           id_name = "",
           columns = NULL,
           id = recipes::rand_id("th2_feature_engineering")) {
    recipes::add_step(
      recipe,
      step_th2_feature_engineering_new(
        terms = recipes::ellipse_check(...),
        role = role,
        skip = skip,
        trained = trained,
        feature_target = feature_target,
        use_holidays = use_holidays,
        all_data = all_data,
        id_name = id_name,
        columns = columns,
        id = id
      )
    )
  }

#' @export
step_th2_feature_engineering_new <-
  function(terms,
           role,
           skip,
           trained,
           feature_target,
           use_holidays,
           all_data,
           id_name,
           columns,
           id) {
    recipes::step(
      subclass = "th2_feature_engineering",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      feature_target = feature_target,
      use_holidays = use_holidays,
      all_data = all_data,
      id_name = id_name,
      columns = columns,
      id = id
    )
  }


#' @export
prep.step_th2_feature_engineering <- function(x,
                                              training,
                                              info = NULL,
                                              ...) {
  col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

  recipes::check_type(training[, col_names], types = c("date", "datetime"))

  step_th2_feature_engineering_new(
    terms = x$terms,
    role = x$role,
    skip = x$skip,
    trained = TRUE,
    feature_target = x$feature_target,
    use_holidays = x$use_holidays,
    all_data = x$all_data,
    id_name = x$id_name,
    columns = col_names,
    id = x$id
  )
}


#' @export
bake.step_th2_feature_engineering <- function(object,
                                              new_data,
                                              ...) {
  # print(dim(new_data))

  target_col <- object$feature_target
  use_holidays <- object$use_holidays

  all_data <- object$all_data
  print("all_data")
  print(dim(all_data))

  print("data")
  print(dim(new_data))

  print(object$id_name)

  feat_len <- (feature_selection(new_data, feature_target = target_col, use_holidays = use_holidays) %>% ncol()) - 2
  # print("size")
  # print(feat_len)

  # feat_len <- 11

  new_cols <- rep(
    feat_len,
    each = length(object$columns)
  )

  date_values <- matrix(NA, nrow = nrow(new_data), ncol = sum(new_cols))

  colnames(date_values) <- as.character(seq_len(sum(new_cols)))

  date_values <- tibble::as_tibble(date_values)

  new_names <- vector("character", length = ncol(date_values))

  strt <- 1
  for (i in seq_along(object$columns)) {
    cols <- (strt):(strt + new_cols[i] - 1)

    tmp <- new_data %>%
      feature_selection(feature_target = target_col, use_holidays = use_holidays) %>%
      dplyr::select(-object$columns[i], -target_col) %>%
      dplyr::as_tibble()

    date_values[, cols] <- tmp

    new_names[cols] <- paste(
      object$columns[i],
      names(tmp),
      sep = "_"
    )

    strt <- max(cols) + 1
  }

  names(date_values) <- new_names

  new_data <- dplyr::bind_cols(new_data, date_values)

  if (!tibble::is_tibble(new_data)) {
    new_data <- tibble::as_tibble(new_data)
  }
  # print(dim(new_data))
  new_data
}

#' @export
print.step_th2_feature_engineering <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Feature engineering for columns"
    recipes::print_step(x$columns, x$terms, x$trained, width = width, title = title)
    invisible(x)
  }
