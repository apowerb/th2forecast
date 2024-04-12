# library(hts)
#
# nodes <- list(2, c(3, 2))
# abc <- ts(5 + matrix(sort(rnorm(500)), ncol = 5, nrow = 100))
#
# x <- hts(abc, nodes)
#
#
# abc <- ts(5 + matrix(sort(rnorm(1000)), ncol = 10, nrow = 100))
# colnames(abc) <- c("A10A", "A10B", "A10C", "A20A", "A20B",
#                    "B30A", "B30B", "B30C", "B40A", "B40B")
# y <- hts(abc, characters = c(1, 2, 1))
#
# library(dplyr)
# library(tsibble)
# library(fabletools)
# library(ggplot2)
# library(fable)
#
# prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") |>
#   mutate(Quarter = yearquarter(Date)) |>
#   select(-Date)  |>
#   as_tsibble(key = c(Gender, Legal, State, Indigenous),
#              index = Quarter) |>
#   relocate(Quarter)
#
#
# prison_gts <- prison |>
#   aggregate_key(Gender * Legal * State, Count = sum(Count)/1e3)
#
#
#
# prison_gts |>
#   filter(!is_aggregated(Gender), is_aggregated(Legal),
#          is_aggregated(State)) |>
#   autoplot(Count) +
#   labs(y = "Number of prisoners ('000)")
#
#
# prison_gts |>
#   filter(!is_aggregated(Gender), !is_aggregated(Legal),
#          !is_aggregated(State)) |>
#   mutate(Gender = as.character(Gender)) |>
#   ggplot(aes(x = Quarter, y = Count,
#              group = Gender, colour=Gender)) +
#   stat_summary(fun = sum, geom = "line") +
#   labs(title = "Prison population by state and gender",
#        y = "Number of prisoners ('000)") +
#   facet_wrap(~ as.character(State),
#              nrow = 1, scales = "free_y") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#
#
#
#
# fit <- prison_gts |>
#   filter(year(Quarter) <= 2014) |>
#   model(base = ETS(Count)) |>
#   reconcile(
#     bottom_up = bottom_up(base),
#     MinT = min_trace(base, method = "mint_shrink")
#   )
# fc <- fit |> forecast(h = 8)
#
#
# fit <- prison_gts |>
#   filter(year(Quarter) <= 2014) |>
#   model(base = ETS(Count)) |>
#   reconcile(
#     bottom_up = bottom_up(base),
#     MinT = min_trace(base, method = "mint_shrink")
#   )
# fc <- fit |> forecast(h = 8)
#
#
# fc |>
#   filter(is_aggregated(State), is_aggregated(Gender),
#          is_aggregated(Legal)) |>
#   accuracy(data = prison_gts,
#            measures = list(mase = MASE,
#                            ss = skill_score(CRPS)
#            )
#   ) |>
#   group_by(.model) |>
#   summarise(mase = mean(mase), sspc = mean(ss) * 100)
#
#
# # ---------------
# #---------------- Tourisme
#
# tourism_full <- tourism |>
#   aggregate_key((State/Region) * Purpose, Trips = sum(Trips))
#
#
#
#
# tourism_states <- tourism |>
#   aggregate_key(State, Trips = sum(Trips))
#
#
#
# fcasts_state <- tourism_states |>
#   filter(!is_aggregated(State)) |>
#   model(ets = ETS(Trips)) |>
#   forecast()
#
# # Sum bottom-level forecasts to get top-level forecasts
# fcasts_national <- fcasts_state |>
#   summarise(value = sum(Trips), .mean = mean(value))
#
#
#
# tourism_states |>
#   model(ets = ETS(Trips)) |>
#   reconcile(bu = bottom_up(ets)) |>
#   forecast()
#
#
# #----------------------------------
# library(lubridate)
#
# tourism_full <- tourism |>
#   aggregate_key((State/Region) * Purpose, Trips = sum(Trips))
#
# fit <- tourism_full |>
#   filter(year(Quarter) <= 2015) |>
#   model(base = ETS(Trips)) |>
#   reconcile(
#     bu = bottom_up(base),
#     ols = min_trace(base, method = "ols"),
#     mint = min_trace(base, method = "mint_shrink")
#   )
#
#
# # -------------------------
#
#
# library(fable.prophet)
# cement <- aus_production |>
#   filter(year(Quarter) >= 1988)
# train <- cement |>
#   filter(year(Quarter) <= 2007)
# fit <- train |>
#   model(
#     arima = ARIMA(Cement),
#     ets = ETS(Cement),
#     prophet = prophet(Cement ~ season(period = 4, order = 2,
#                                       type = "multiplicative"))
#   )
