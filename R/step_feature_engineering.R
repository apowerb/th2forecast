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
           use_holidays = NULL,
           all_data = "",
           id_name = "",
           columns = NULL,
           lags = FALSE,
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
        lags = lags,
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
           lags,
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
      lags = lags,
      id = id
    )
  }


#' My S3 Method for step_th2_feature_engineering
#'
#' Description of what this S3 method does.
#'
#' @param x An object of class myclass.
#' @param ... Additional arguments.
#' @export
#' @exportS3Method recipes::prep
#' @importFrom recipes prep
prep.step_th2_feature_engineering <- function(x,
                                              training,
                                              info = NULL,
                                              ...) {
  # print(x$terms)
  # print("name_id" %in% colnames(training))
  # print(training_n)
  # browser()
  if ("name_id" %in% colnames(training)) {
    training_n <- as.character(training$name_id[1])
    training <- training %>%
      dplyr::select(-name_id)
  }

  if(!is.null(x$use_holidays)) {
    bh_country <- x$use_holidays
    if (!!bh_country %in% colnames(training)) {
      country_n <- as.character(training[[bh_country]][1])
      training <- training %>%
        dplyr::select(-!!bh_country)
    }
  }

  # training <- as_tibble(cbind(training, training_n))

  # print(training)

  col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)
  # print(col_names)

  recipes::check_type(training[, col_names], types = c("date", "datetime"))

  step_th2_feature_engineering_new(
    terms = x$terms,
    role = x$role,
    skip = x$skip,
    trained = TRUE,
    feature_target = x$feature_target,
    use_holidays = x$use_holidays,
    all_data = x$all_data,
    id_name = training_n[1],
    columns = col_names,
    lags = x$lags,
    id = x$id
  )
}


#' My S3 Method for step_th2_feature_engineering
#'
#' Description of what this S3 method does.
#'
#' @param x An object of class myclass.
#' @param ... Additional arguments.
#' @export
#' @exportS3Method recipes::bake
#' @importFrom recipes bake
bake.step_th2_feature_engineering <- function(object,
                                              new_data,
                                              ...) {
  # print(dim(new_data))
  # print("llego a bake")
  # print(new_data)
  # browser()
  training_n <- ""
  if ("name_id" %in% colnames(new_data)) {
    training_n <- as.character(new_data$name_id[1])
    new_data <- new_data %>%
      dplyr::select(-name_id)
  }

  feat_len <- ((timetk::tk_get_timeseries_signature(lubridate::ymd("2016-01-01")) %>% ncol()) - 1 + object$lags) - 2

  if(!is.null(object$use_holidays)) {
    bh_country <- object$use_holidays
    if (!!bh_country %in% colnames(new_data)) {
      use_holidays <- as.character(new_data[[bh_country]][1])
      new_data <- new_data %>%
        dplyr::select(-!!bh_country)
    }else{
      use_holidays <- object$use_holidays
    }
  }else{
    use_holidays <- object$use_holidays
    feat_len <- feat_len - 1
  }

  target_col <- object$feature_target

  all_data <- object$all_data

  # print(object$id_name)

  # feat_len <- (feature_selection(new_data, feature_target = target_col, use_holidays = use_holidays, all_data = all_data, id_name = object$id_name, lags = 5) %>% ncol()) - 2
  # print(feat_len)
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
      feature_selection(feature_target = target_col, use_holidays = use_holidays, all_data = all_data, id_name = training_n, lags = object$lags) %>%
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
