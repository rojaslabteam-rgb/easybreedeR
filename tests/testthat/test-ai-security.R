test_that("host credentials and endpoint cannot be independently overridden", {
  ai <- source_helpers("Xmosome_blupf90kit/ai_backend.R")
  withr::local_envvar(c(XMOSOME_AI_API_KEY = "audit-host-secret",
                       XMOSOME_AI_BASE_URL = "https://api.openai.com/v1",
                       XMOSOME_AI_MODEL = "host-model", XMOSOME_AI_ALLOWED_BASE_URLS = ""))
  cfg <- ai$ai_resolve_config(list(model = "expensive-user-model"))
  expect_identical(cfg$model, "host-model")
  expect_identical(cfg$source, "default")
  expect_error(ai$ai_resolve_config(list(base_url = "https://openrouter.ai/api/v1")), "own API key")
  expect_error(ai$ai_resolve_config(list(api_preset = "custom")), "own API key")
  custom <- ai$ai_resolve_config(list(api_key = "user-secret", base_url = "https://openrouter.ai/api/v1"))
  expect_identical(custom$api_key, "user-secret")
  expect_identical(custom$source, "user")
  for (url in c("http://127.0.0.1:8080/v1", "https://unapproved.example/v1",
                "https://api.openai.com.evil.example/v1", "https://user:pass@api.openai.com/v1",
                "https://api.openai.com/v1?target=secret")) {
    expect_error(ai$ai_resolve_config(list(api_key = "user-secret", base_url = url)), "not approved")
  }
  expect_error(ai$ai_resolve_config(list(api_key = c("one", "two"))), "Invalid AI setting")
  expect_error(ai$ai_resolve_config(list(base_url = NA_character_)), "Invalid AI setting")
})

test_that("author-funded chat is limited to 10 questions per IP per day", {
  ai <- source_helpers("Xmosome_blupf90kit/ai_backend.R")
  quota_dir <- tempfile("xmosome-ai-quota-")
  dir.create(quota_dir)
  withr::defer(unlink(quota_dir, recursive = TRUE, force = TRUE))
  withr::local_envvar(c(
    XMOSOME_AI_DAILY_LIMIT = "10",
    XMOSOME_AI_QUOTA_DIR = quota_dir
  ))
  expect_identical(ai$ai_author_daily_limit(), 10L)
  st0 <- ai$ai_quota_status("203.0.113.10")
  expect_identical(st0$used, 0L)
  expect_identical(st0$remaining, 10L)
  expect_true(st0$allowed)
  for (i in 1:10) {
    got <- ai$ai_quota_consume("203.0.113.10")
    expect_true(got$ok)
    expect_identical(got$used, i)
    expect_identical(got$remaining, 10L - i)
  }
  blocked <- ai$ai_quota_consume("203.0.113.10")
  expect_false(blocked$ok)
  expect_match(blocked$error, "daily limit", ignore.case = TRUE)
  other <- ai$ai_quota_consume("203.0.113.11")
  expect_true(other$ok)
  expect_identical(other$used, 1L)
})

test_that("legacy httr transport also disables redirects", {
  skip_if_not_installed("httr")
  ai <- source_helpers("Xmosome_blupf90kit/ai_backend.R")
  ai$requireNamespace <- function(package, quietly = TRUE) {
    if (package == "httr2") return(FALSE)
    base::requireNamespace(package, quietly = quietly)
  }
  withr::local_envvar(c(XMOSOME_AI_API_KEY = "audit-host-secret", XMOSOME_AI_BASE_URL = "https://api.openai.com/v1"))
  seen <- NULL
  testthat::local_mocked_bindings(POST = function(url, ..., body, encode) {
    seen <<- list(...)
    structure(list(status_code = 302L, content = charToRaw("audit-host-secret"),
                   url = url, headers = list(`Content-Type` = "text/plain")), class = "response")
  }, .package = "httr")
  result <- ai$ai_chat_request(list(), ai$ai_resolve_config())
  expect_false(result$ok)
  expect_true(any(vapply(seen, function(x) identical(x$options$followlocation, FALSE), logical(1))))
  expect_false(grepl("audit-host-secret", result$error, fixed = TRUE))
})

test_that("requests disable redirects and never echo provider secrets", {
  skip_if_not_installed("httr2")
  ai <- source_helpers("Xmosome_blupf90kit/ai_backend.R")
  withr::local_envvar(c(XMOSOME_AI_API_KEY = "audit-host-secret", XMOSOME_AI_BASE_URL = "https://api.openai.com/v1"))
  seen <- NULL
  testthat::local_mocked_bindings(req_perform = function(req) {
    seen <<- req
    httr2::response(302L, headers = list(location = "https://evil.example"),
                    body = charToRaw("audit-host-secret"))
  }, .package = "httr2")
  result <- ai$ai_chat_request(list(list(role = "user", content = "hello")), ai$ai_resolve_config())
  expect_false(result$ok)
  expect_identical(seen$options$followlocation, FALSE)
  expect_false(grepl("audit-host-secret", result$error, fixed = TRUE))
  seen <- NULL
  cfg <- ai$ai_resolve_config()
  cfg$base_url <- "https://openrouter.ai/api/v1"
  expect_false(ai$ai_chat_request(list(), cfg)$ok)
  expect_null(seen)
})
