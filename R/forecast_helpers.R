prepare_input_fcast_data <- function(raw_data, non_numeric_variables = NULL,
                                     input_object = NULL,
                                     var_granularity = NULL,
                                     input_time_freq = NULL,
                                     fc_target_var = NULL,
                                     fc_date_var = NULL) {
  if (length(non_numeric_variables) > 0) {
    categ_input_filter <- non_numeric_variables %>%
      purrr::map(~ input_object[[paste0("non_numeric_variables_", .x)]]) %>%
      stats::setNames(non_numeric_variables)
    categ_input_filter <- categ_input_filter[!unlist(lapply(categ_input_filter, is.null))]
    for (cat_input in names(categ_input_filter)) {
      if ("NA" %in% categ_input_filter[[cat_input]]) categ_input_filter[[cat_input]] <- c(categ_input_filter[[cat_input]], NA)
      raw_data <- raw_data %>% dplyr::filter(!!rlang::sym(cat_input) %in% categ_input_filter[[cat_input]])
    }
  }

  if (!is.null(var_granularity)) {
    grouping_elements <- c(fc_date_var, var_granularity) %>%
      unique()
  } else {
    grouping_elements <- fc_date_var
  }
  raw_data <- raw_data %>%
    dplyr::select(!!c(grouping_elements, fc_target_var)) %>%
    dplyr::group_by(dplyr::across(!!grouping_elements)) %>%
    dplyr::summarise_all(th2reporting:::th2_agg_func, "sum")
  raw_data <- raw_data %>%
    janitor::clean_names()
  raw_data <- raw_data %>%
    tidyr::pivot_longer(
      cols = fc_target_var,
      names_to = "target_vars",
      values_to = "actuals"
    )
  if (!is.null(var_granularity)) {
    raw_data <- raw_data %>%
      dplyr::rename(kpi_granularity = !!var_granularity) %>%
      tidyr::unite("target_vars", target_vars:kpi_granularity, remove = TRUE)
  }
  raw_data <- raw_data %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[fc_date_var]])
  if (!is.null(input_time_freq)) {
    grouping_elements <- c(fc_date_var, "target_vars")
    raw_data <- raw_data %>%
      dplyr::mutate(
        days = lubridate::date(get(fc_date_var)),
        weeks = lubridate::floor_date(as.Date(get(fc_date_var)), unit = "week", week_start = 1),
        months = lubridate::floor_date(as.Date(get(fc_date_var)), unit = "month"),
        quarters = lubridate::floor_date(as.Date(get(fc_date_var)), unit = "quarter")
      ) %>%
      dplyr::select(-!!fc_date_var) %>%
      dplyr::rename(!!quo_name(fc_date_var) := !!input_time_freq) %>%
      dplyr::select(!!grouping_elements, actuals) %>%
      dplyr::group_by(dplyr::across(!!grouping_elements)) %>%
      dplyr::summarise_all(th2reporting:::th2_agg_func, "sum")
  }
  # raw_data2 <<- raw_data
  return(raw_data)
}
