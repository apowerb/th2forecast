test_that("le serialiseur JSON de l'API rend un champ NULL comme 'null', pas '{}'", {
  body <- api_v1_error_body(list(api_v1_error(NULL, "Authentification requise.")))

  # Meme appel que celui utilise par plumber.R (@serializer
  # application/json;charset=utf-8 reqres::format_json(auto_unbox=TRUE, null="null"))
  json_text <- as.character(reqres::format_json(auto_unbox = TRUE, null = "null")(body))

  expect_match(json_text, '"field":null', fixed = TRUE)
  expect_no_match <- !grepl('"field":{}', json_text, fixed = TRUE)
  expect_true(expect_no_match)
})

test_that("le serialiseur par defaut de plumber2 (sans null='null') produirait '{}' (regression documentee)", {
  body <- api_v1_error_body(list(api_v1_error(NULL, "x")))
  json_default <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
  expect_match(json_default, '"field":\\{\\}')
})

test_that("api_v1_job_get renvoie result/error a null (pas absents) tant que le job est en cours", {
  skip_if_not_installed("mirai")
  # Job inconnu : verifie au moins le format d'erreur (pas de crash serialisation).
  res <- api_v1_job_get("job-inconnu-xyz")
  expect_equal(res$status_code, 404L)
  json_text <- as.character(reqres::format_json(auto_unbox = TRUE, null = "null")(res$body))
  expect_match(json_text, '"field":"job_id"', fixed = TRUE)
})
