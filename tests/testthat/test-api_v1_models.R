test_that("api_v1_reliability renvoie 'unknown' sans metriques exploitables", {
  expect_equal(api_v1_reliability(NA, 1, 10), "unknown")
  expect_equal(api_v1_reliability(0.5, NA, 10), "unknown")
  expect_equal(api_v1_reliability(0.5, 1, 1), "unknown")
})

test_that("api_v1_reliability renvoie 'poor' quand le modele ne bat pas la baseline", {
  expect_equal(api_v1_reliability(1.2, 1.0, 10), "poor")
  expect_equal(api_v1_reliability(1.0, 1.0, 10), "poor")
})

test_that("api_v1_reliability renvoie 'good' avec un net gain et assez de holdout", {
  expect_equal(api_v1_reliability(0.5, 1.0, 6), "good")
})

test_that("api_v1_reliability renvoie 'fair' quand le gain est faible ou le holdout court", {
  expect_equal(api_v1_reliability(0.9, 1.0, 10), "fair")
  expect_equal(api_v1_reliability(0.5, 1.0, 3), "fair")
})
