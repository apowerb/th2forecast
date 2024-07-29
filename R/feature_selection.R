#' Extraction des caractéristiques avec le package tsfeatures
#'
#' Une fonction permettant augmenter les caractéristiques
#'
#' @param ts_movil liste pour le calcul des features
#' @param features_input sélection des features à retourner
#'
#' @return features
#' @export
th2_tsfeatures <- function(
    ts_movil,
    features_input = c("trend", "spike", "linearity", "curvature", "e_acf1", "entropy")) {
  ts <- ts(ts_movil)
  features <- tsfeatures::tsfeatures(ts)
  features <- features %>%
    dplyr::select(all_of(features_input))
  return(features)
}

#' @export
th2_lag_roll_transformer <- function(data) {
  data %>%
    timetk::tk_augment_lags(target_g, .lags = 1:lag_g)
}

#' Extraction des caractéristiques
#'
#' Une fonction permettant d'extraire les caractéristiques.
#'
#' @param dataset un dataframe
#'
#' @return a fonction renvoie un dataset avec les caractéristiques importantes
#' @export
#'
#' @examples feature_selection(input_data)
feature_selection <- function(
    input_data,
    feature_target,
    list_features = c(),
    use_holidays = NULL,
    use_meteo = NULL,
    lags = 5,
    window = 5,
    all_data = "",
    id_name = "",
    db_conn = NULL) {
  if (is.data.frame(input_data)) {
    if (nrow(input_data) == 0 || ncol(input_data) == 0) {
      return(warning("The *input_date* variable is empty"))
    }
    # browser()
    lag_g <<- lags
    target_g <<- feature_target

    column_date <- sapply(input_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
    var_date_feature <- names(input_data)[column_date]

    if (length(list_features) > 0) {
      list_features <- c(var_date_feature, feature_target, list_features)
      data_features <- input_data %>%
        dplyr::select(all_of(list_features))
    } else {
      list_features <- c(var_date_feature, feature_target)
      data_features <- input_data %>%
        dplyr::select(all_of(list_features))
    }

    if (any(is.na(data_features[feature_target])) && lags != FALSE) {
      var_temp <- ((all_data %>%
        filter(id == id_name) %>%
        select(.actual_data))[[1]][[1]]) %>%
        dplyr::select(all_of(list_features))
      data_features <- var_temp %>% rbind(data_features)
    }

    if (!is.null(use_holidays)) {
      holidays_list <- holidays_detection(data_features, model = "ml", calendar = use_holidays, db_conn = db_conn)
      data_features["holidays"] <- holidays_list
    }

    if (use_meteo == TRUE && !is.null(use_meteo)) {
      meteo_list <- meteo_feature(data_features, region = use_holidays)
      meteo_list <- meteo_list[meteo_list$date %in% data_features[[var_date_feature]], ]
      data_features["temperature"] <- meteo_list$daily_temperature_2m_max
      data_features["precipitation"] <- meteo_list$daily_precipitation_sum
    }

    data_signature <- timetk::tk_get_timeseries_signature(data_features[[var_date_feature]]) %>%
      janitor::remove_empty() %>%
      # janitor::remove_constant() %>%
      dplyr::select(- c(index, diff, wday.lbl, month.lbl, hour, minute, second, hour12, am.pm))

    data_features <- cbind(data_features, data_signature)

    data_features <- as_tibble(data_features)


    if (lags != FALSE) {
      if (!any(is.na(data_features[feature_target]))) {
        for (i in 1:lags) {
          data_lag <- dplyr::lag(data_features[feature_target], i)
          data_lag[1:i, 1] <- data_features[i + 1, 2]
          data_features[paste(feature_target, "_lag", i, sep = "")] <- data_lag
        }
      } else {
        dim_input_data <- nrow(input_data)
        dim_train_data <- nrow(data_features) - dim_input_data

        all_train_data <- data_features[1:dim_train_data, ] %>%
          dplyr::select(all_of(list_features))

        lag_data_features <- data_features %>%
          dplyr::select(all_of(list_features))

        for (i in 1:lags) {
          data_lag <- dplyr::lag(data_features[feature_target], i)
          data_lag[1:i, 1] <- data_features[i + 1, 2]
          lag_data_features[paste(feature_target, "_lag", i, sep = "")] <- data_lag
        }

        train_data_lags <- lag_data_features[1:dim_train_data, ] %>%
          tidyr::drop_na()

        # formula <- as.formula(paste(feature_target, "~ ."))
        #
        # model_fit_lm_recursive <- parsnip::linear_reg() %>%
        #   parsnip::set_engine(engine = "lm") %>%
        #   parsnip::set_mode("regression") %>%
        #   parsnip::fit(formula, data = train_data_lags) %>%
        #   modeltime::recursive(
        #     transform  = th2_lag_roll_transformer,
        #     train_tail = tail(train_data_lags, 200)
        #   )

        formula <- as.formula(paste(feature_target, "~ ."))
        set.seed(123)
        model_fit_xgb_recursive <- parsnip::boost_tree(
          mode = "regression",
          learn_rate = 0.05
        ) %>%
          parsnip::set_engine("xgboost") %>%
          parsnip::fit(formula, data = dplyr::select(train_data_lags, -var_date_feature)) %>%
          modeltime::recursive(
            transform  = th2_lag_roll_transformer,
            train_tail = tail(train_data_lags, dim_input_data)
          )

        model_tbl <- modeltime_table(
          model_fit_xgb_recursive
        )

        future_data_recursive <- lag_data_features[(dim_train_data + 1):(dim_train_data + dim_input_data), ]

        forecast_recursive_result <- model_tbl %>%
          modeltime::modeltime_forecast(
            new_data    = future_data_recursive,
            actual_data = all_train_data
          )

        future_lags_result <- forecast_recursive_result %>%
          dplyr::filter(.key == "prediction") %>%
          dplyr::select(.value)

        data_lags_final <- data_features
        data_lags_final[(dim_train_data + 1):(dim_train_data + dim_input_data), 2] <- future_lags_result

        for (i in 1:lags) {
          data_lag <- dplyr::lag(data_lags_final[feature_target], i)
          data_lag[1:i, 1] <- data_lags_final[i + 1, 2]
          lag_data_features[paste(feature_target, "_lag", i, sep = "")] <- data_lag
        }

        data_features <- cbind(data_features, lag_data_features[, 3:ncol(lag_data_features)]) %>%
          tail(dim_input_data) %>%
          dplyr::as_tibble()
      }
    }

    # window <- window
    #
    if (lags != FALSE) {
      data_features["rolling_mean"] <- zoo::rollapplyr(input_data[feature_target], lags, mean, fill = NA)
      data_features["rolling_std"] <- zoo::rollapplyr(input_data[feature_target], lags, sd, fill = NA)

      col_lags <- c(paste0(feature_target,"_lag", 1:lags))

      for (i in 1:lags) {
        data_features[i, "rolling_mean"] <- rowMeans(data_features[i, col_lags])
        data_features[i, "rolling_std"] <- sd(data_features[i, col_lags])
      }
    }

    # st_features <- zoo::rollapplyr(
    #   input_data[feature_target],
    #   width = window,
    #   FUN = th2_tsfeatures,
    #   by.column = FALSE,
    #   fill = NA
    #   )
    #
    # data_features <- cbind(data_features, st_features)
    #
    # data_features <- data_features[complete.cases(data_features), ]

    # print(data_features)
    # data_features[is.na(data_features)] <- 0

    return(data_features)
  } else {
    return(warning("The *input_date* variable is not a data.frame"))
  }
}
