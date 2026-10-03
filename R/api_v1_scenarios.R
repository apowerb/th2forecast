#' @name api_v1_scenarios
NULL

# "What if" scenarios: the base forecast plus explicit adjustments (`percent`
# or `add`), applied pro rata to the days each period covers. Same contract as
# the Python engine (docs/API.md). Events -- known future dates used as model
# covariates -- are not supported by this engine: they are reported, never
# silently dropped.

MAX_SCENARIOS <- 5L
MAX_ADJUSTMENTS <- 20L

# Request fields only the Python engine implements.
PYTHON_ONLY_FIELDS <- c("events", "hierarchy", "reconciliation")

#' Turns a JSON array of objects into a list of lists, whether the parser
#' simplified it into a data.frame or not.
#' @keywords internal
.api_v1_records <- function(x) {
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      lapply(as.list(x[i, , drop = FALSE]), function(col) if (is.list(col)) col[[1]] else col)
    }))
  }
  x
}

#' TRUE when a field was sent: an empty list counts, a missing value (absent
#' key, or NA in a simplified data.frame column) does not.
#' @keywords internal
.api_v1_present <- function(x) !is.null(x) && !(is.atomic(x) && length(x) == 1 && is.na(x))

#' Parses a calendar day, ISO (YYYY-MM-DD) or day-first (DD/MM/YYYY); a time
#' suffix is ignored. NA when unreadable.
#' @keywords internal
.api_v1_parse_day <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(as.Date(NA))
  iso <- regmatches(x, regexec("^(\\d{4})-(\\d{1,2})-(\\d{1,2})", x))[[1]]
  dmy <- regmatches(x, regexec("^(\\d{1,2})/(\\d{1,2})/(\\d{4})", x))[[1]]
  parts <- if (length(iso)) iso[2:4] else if (length(dmy)) dmy[c(4, 3, 2)] else return(as.Date(NA))
  as.Date(sprintf("%s-%s-%s", parts[1], parts[2], parts[3]), optional = TRUE)
}

#' Last day of the period that starts on `d`.
#'
#' Month-based steps roll back to the month's last day (31 Jan + 1 month =
#' 29 Feb), like the Python engine's DateOffset.
#' @export
api_v1_period_end <- function(d, frequency) {
  next_start <- switch(frequency,
    day = d + 1,
    week = d + 7,
    month = lubridate::add_with_rollback(d, months(1)),
    quarter = lubridate::add_with_rollback(d, months(3)),
    year = lubridate::add_with_rollback(d, months(12)),
    d + 1
  )
  next_start - 1
}

#' Share (0 to 1) of each period's days that fall within `[start, end]`.
#' @export
api_v1_coverage <- function(periods, frequency, start, end) {
  vapply(seq_along(periods), function(k) {
    p <- periods[k]
    p_end <- api_v1_period_end(p, frequency)
    lo <- max(start, p)
    hi <- min(end, p_end)
    if (lo > hi) return(0)
    (as.numeric(hi - lo) + 1) / (as.numeric(p_end - p) + 1)
  }, numeric(1))
}

#' Validates `scenarios`; errors go through `add_error(field, message)`.
#'
#' @return list(scenarios = <list of list(name, adjustments, has_events)>)
#' @keywords internal
api_v1_parse_scenarios <- function(raw, add_error) {
  if (is.null(raw)) return(list())
  items <- .api_v1_records(raw)
  if (!is.list(items) || !is.null(names(items)) || length(items) > MAX_SCENARIOS) {
    add_error("scenarios", sprintf("'scenarios' doit être une liste d'au plus %d scénarios.", MAX_SCENARIOS))
    return(list())
  }
  out <- list()
  for (i in seq_along(items)) {
    item <- items[[i]]
    label <- sprintf("scenarios[%d]", i - 1L)
    name <- if (is.list(item)) item[["name"]] else NULL
    if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(trimws(name))) {
      add_error(label, sprintf("%s : un nom ('name') est requis.", label))
      next
    }
    raw_adj <- .api_v1_records(item[["adjustments"]])
    if (is.null(raw_adj)) raw_adj <- list()
    if (!is.list(raw_adj) || !is.null(names(raw_adj)) || length(raw_adj) > MAX_ADJUSTMENTS) {
      add_error(paste0(label, ".adjustments"), sprintf(
        "%s : 'adjustments' doit être une liste d'au plus %d ajustements.", label, MAX_ADJUSTMENTS
      ))
      next
    }
    adjustments <- list()
    for (j in seq_along(raw_adj)) {
      a <- raw_adj[[j]]
      where <- sprintf("%s.adjustments[%d]", label, j - 1L)
      if (!is.list(a)) {
        add_error(where, sprintf("%s : objet {start, end, percent | add} attendu.", where))
        next
      }
      start <- .api_v1_parse_day(a[["start"]])
      end_raw <- if (is.null(a[["end"]]) || (length(a[["end"]]) == 1 && is.na(a[["end"]]))) a[["start"]] else a[["end"]]
      end <- .api_v1_parse_day(end_raw)
      if (is.na(start) || is.na(end)) {
        add_error(where, sprintf(
          "%s : date invalide (formats acceptés : YYYY-MM-DD ou JJ/MM/AAAA).", where
        ))
        next
      }
      is_num <- function(v) is.numeric(v) && length(v) == 1 && !is.na(v)
      pct <- a[["percent"]]
      add <- a[["add"]]
      if (is_num(pct) == is_num(add)) {
        add_error(where, sprintf("%s : indiquez soit 'percent' (ex. -20), soit 'add' (ex. 150).", where))
        next
      }
      if (is_num(pct) && pct <= -100) {
        add_error(where, sprintf("%s : 'percent' doit être supérieur à -100.", where))
        next
      }
      if (end < start) {
        add_error(where, sprintf("%s : 'end' précède 'start'.", where))
        next
      }
      adjustments[[length(adjustments) + 1]] <- list(
        start = start, end = end,
        percent = if (is_num(pct)) as.numeric(pct) else NULL,
        add = if (is_num(add)) as.numeric(add) else NULL
      )
    }
    out[[length(out) + 1]] <- list(
      name = trimws(name), adjustments = adjustments, has_events = .api_v1_present(item[["events"]])
    )
  }
  out
}

#' Applies a scenario's adjustments to a forecast.
#'
#' @param values named list of numeric vectors: `value` plus the interval
#'   bounds (`lower_80`, ...); every vector is shifted the same way, so the
#'   bands keep the base forecast's calibration.
#' @param periods Date vector, one per forecast row
#' @export
api_v1_apply_adjustments <- function(values, adjustments, periods, frequency) {
  for (a in adjustments) {
    share <- api_v1_coverage(periods, frequency, a$start, a$end)
    values <- lapply(values, function(v) {
      if (!is.null(a$percent)) v * (1 + share * a$percent / 100) else v + share * a$add
    })
  }
  values
}

#' Scenario entries of one series' response.
#'
#' @param future data.frame of the base forecast: `.index`, `.value` and the
#'   interval columns, unrounded
#' @param fmt_num the series' number formatter
#' @keywords internal
api_v1_scenario_entries <- function(scenarios, future, frequency, fmt_num) {
  periods <- as.Date(future$.index)
  cols <- c(".value", setdiff(names(future), c(".key", ".index", ".value")))
  base <- stats::setNames(lapply(cols, function(col) as.numeric(future[[col]])), cols)
  base_total <- sum(base$.value)
  lapply(scenarios, function(scn) {
    shifted <- api_v1_apply_adjustments(base, scn$adjustments, periods, frequency)
    rows <- lapply(seq_along(periods), function(i) {
      row <- list(date = format(periods[i], "%Y-%m-%d"), value = fmt_num(shifted$.value[i]))
      for (col in setdiff(cols, ".value")) row[[col]] <- fmt_num(shifted[[col]][i])
      row
    })
    total <- sum(shifted$.value)
    list(
      name = scn$name,
      forecast = rows,
      difference = list(
        total = fmt_num(total - base_total),
        percent = if (base_total != 0) round((total / base_total - 1) * 100, 2) else NA
      )
    )
  })
}

#' Response-level warnings for request fields this engine does not implement.
#' @keywords internal
api_v1_unsupported_warnings <- function(body) {
  out <- character(0)
  for (field in PYTHON_ONLY_FIELDS) {
    if (.api_v1_present(body[[field]])) {
      out <- c(out, sprintf(
        "Le champ '%s' n'est pas pris en charge par le moteur R : il est ignoré.", field
      ))
    }
  }
  out
}

#' Per-series warnings for scenarios that asked for events.
#' @keywords internal
api_v1_scenario_warnings <- function(scenarios) {
  out <- character(0)
  for (scn in scenarios) {
    if (isTRUE(scn$has_events)) {
      out <- c(out, sprintf(
        "Scénario '%s' : les événements ne sont pas pris en charge par le moteur R ; seuls les ajustements sont appliqués.",
        scn$name
      ))
    }
  }
  out
}
