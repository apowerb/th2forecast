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
step_th2_exogenous_variable <-
  function(recipe,
           ...,
           role = "predictor",
           skip = FALSE,
           trained = FALSE,
           feature_target = "",
           use_holidays = TRUE,
           external_data = "",
           id_name = "",
           columns = NULL,
           exogenous_var = FALSE,
           id = recipes::rand_id("th2_exogenous_variable")) {
    recipes::add_step(
      recipe,
      step_th2_exogenous_variable_new(
        terms = recipes::ellipse_check(...),
        role = role,
        skip = skip,
        trained = trained,
        feature_target = feature_target,
        use_holidays = use_holidays,
        external_data = external_data,
        id_name = id_name,
        columns = columns,
        exogenous_var = exogenous_var,
        id = id
      )
    )
  }

#' @export
step_th2_exogenous_variable_new <-
  function(terms,
           role,
           skip,
           trained,
           feature_target,
           use_holidays,
           external_data,
           id_name,
           columns,
           exogenous_var,
           id) {
    recipes::step(
      subclass = "th2_exogenous_variable",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      feature_target = feature_target,
      use_holidays = use_holidays,
      external_data = external_data,
      id_name = id_name,
      columns = columns,
      exogenous_var = exogenous_var,
      id = id
    )
  }


#' My S3 Method for step_th2_exogenous_variable
#'
#' Description of what this S3 method does.
#'
#' @param x An object of class myclass.
#' @param ... Additional arguments.
#' @export
#' @exportS3Method recipes::prep
#' @importFrom recipes prep
prep.step_th2_exogenous_variable <- function(x,
                                              training,
                                              info = NULL,
                                              ...) {
  # print(x$terms)
  # print("name_id" %in% colnames(training))
  # print(training_n)

  # if ("name_id" %in% colnames(training)) {
  #   training_n <- as.character(training[[2]][1])
  #   training <- training %>%
  #     dplyr::select(-name_id)
  # }

  # training <- as_tibble(cbind(training, training_n))

  # print(training)
  col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)
  # print(col_names)

  recipes::check_type(training[, col_names], types = c("date", "datetime"))

  step_th2_exogenous_variable_new(
    terms = x$terms,
    role = x$role,
    skip = x$skip,
    trained = TRUE,
    feature_target = x$feature_target,
    use_holidays = x$use_holidays,
    external_data = x$external_data,
    id_name = x$id,
    columns = col_names,
    exogenous_var = x$exogenous_var,
    id = x$id
  )
}


#' My S3 Method for step_th2_exogenous_variable
#'
#' Description of what this S3 method does.
#'
#' @param x An object of class myclass.
#' @param ... Additional arguments.
#' @export
#' @exportS3Method recipes::bake
#' @importFrom recipes bake
bake.step_th2_exogenous_variable <- function(object,
                                              new_data,
                                              ...) {
  print("entroooooooooooooooooooo exo var")
  # browser()
  # print(new_data)
  # print(object$exogenous_var)
  # print(object$external_data)

  min_date <- min(new_data[["date"]])
  max_date <- max(new_data[["date"]])


  exogen_data <- object$external_data %>%
    filter(date >= min_date & date <= max_date) %>%
    select(object$exogenous_var)

  new_data <- dplyr::bind_cols(new_data, exogen_data)

  # print(dim(new_data))
  # print("llego a bake")
  # print(new_data)

  # training_n <- ""
  # if ("name_id" %in% colnames(new_data)) {
  #   training_n <- as.character(new_data[[2]][1])
  #   new_data <- new_data %>%
  #     dplyr::select(-name_id)
  # }
  #
  # target_col <- object$feature_target
  # use_holidays <- object$use_holidays
  #
  # external_data <- object$external_data
  #
  # # print(object$id_name)
  #
  # # feat_len <- (feature_selection(new_data, feature_target = target_col, use_holidays = use_holidays, external_data = external_data, id_name = object$id_name, exogenous_var = 5) %>% ncol()) - 2
  # feat_len <- ((timetk::tk_get_timeseries_signature(lubridate::ymd("2016-01-01")) %>% ncol()) - 1 + object$exogenous_var) - 2
  # # print(feat_len)
  # # print("size")
  # # print(feat_len)
  #
  # # feat_len <- 11
  #
  # new_cols <- rep(
  #   feat_len,
  #   each = length(object$columns)
  # )
  #
  # date_values <- matrix(NA, nrow = nrow(new_data), ncol = sum(new_cols))
  #
  # colnames(date_values) <- as.character(seq_len(sum(new_cols)))
  #
  # date_values <- tibble::as_tibble(date_values)
  #
  # new_names <- vector("character", length = ncol(date_values))

  # strt <- 1
  # for (i in seq_along(object$columns)) {
  #   cols <- (strt):(strt + new_cols[i] - 1)
  #
  #   tmp <- new_data %>%
  #     feature_selection(feature_target = target_col, use_holidays = use_holidays, external_data = external_data, id_name = training_n, exogenous_var = object$exogenous_var) %>%
  #     dplyr::select(-object$columns[i], -target_col) %>%
  #     dplyr::as_tibble()
  #
  #   date_values[, cols] <- tmp
  #
  #   new_names[cols] <- paste(
  #     object$columns[i],
  #     names(tmp),
  #     sep = "_"
  #   )
  #
  #   strt <- max(cols) + 1
  # }
  #
  # names(date_values) <- new_names

  # new_data <- dplyr::bind_cols(new_data, date_values)
  #
  if (!tibble::is_tibble(new_data)) {
    new_data <- tibble::as_tibble(new_data)
  }
  print(new_data)
  new_data
}

#' @export
print.step_th2_exogenous_variable <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Feature engineering for columns"
    recipes::print_step(x$columns, x$terms, x$trained, width = width, title = title)
    invisible(x)
  }
