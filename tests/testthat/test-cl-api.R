test_that("cl wrappers map to core functions", {
  expect_true(dir.exists(cl_cache_dir()))
  expect_type(cl_whatsapp_import, "closure")
})

test_that("period API returns period columns", {
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "hello",
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC", store_dir = tempfile("cl_store_")))

  periods <- cl_chat_split_periods(chat, period = c("all", "day"))
  expect_true("period" %in% names(periods))
  expect_true("period" %in% names(periods))

  selected <- cl_periods_select(periods, select = "day", data_type = "default")
  expect_true(all(selected$period == "day"))
})

test_that("cl_chat_filter_period filters by day key", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))

  filtered <- cl_chat_filter_period(chat, period = "day", key = "2025-01-01")
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$text, "one")
})

test_that("cl_periods_select accepts chat + period alias", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))

  p1 <- cl_periods_select(chat, period = "all", data_type = "default")
  expect_equal(nrow(p1), 1)
  expect_equal(p1$period, "all")

  p2 <- cl_periods_select(chat, select = "day", data_type = "default")
  expect_true(all(p2$period == "day"))
})

test_that("cl_periods_select supports chat selectors by key and row index", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))

  by_key <- cl_periods_select(chat, select = "2025-01-01", data_type = "default")
  expect_equal(nrow(by_key), 1)
  expect_equal(by_key$period, "day")
  expect_equal(by_key$key, "2025-01-01")

  by_index <- cl_periods_select(chat, select = 1, data_type = "default")
  expect_equal(nrow(by_index), 1)
})

test_that("cl_periods_select default returns selected rows", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))
  out <- cl_periods_select(chat, select = "day", data_type = "default")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
})

test_that("cl_periods_select supports simple and aggregate data frame outputs", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))

  simple_out <- cl_periods_select(chat, select = "day", data_type = "simple")
  expect_s3_class(simple_out, "data.frame")
  expect_equal(nrow(simple_out), 1)
  expect_false("period" %in% names(simple_out))
  expect_false("key" %in% names(simple_out))
  expect_true(grepl("one", simple_out$text[1], fixed = TRUE))
  expect_true(grepl("two", simple_out$text[1], fixed = TRUE))

  aggregate_out <- cl_periods_select(chat, select = "day", data_type = "aggregate")
  expect_s3_class(aggregate_out, "data.frame")
  expect_equal(nrow(aggregate_out), 2)
  expect_false("period" %in% names(aggregate_out))
  expect_false("key" %in% names(aggregate_out))
  expect_true(grepl("one", aggregate_out$text[1], fixed = TRUE))
  expect_true(grepl("two", aggregate_out$text[2], fixed = TRUE))
})

test_that("cl_periods_analyze works when period and key are absent", {
  testthat::local_mocked_bindings(
    cl_analyze_text = function(...) {
      list(text = "ok", meta = list(source = "mock"), saved_file = NULL, meta_file = NULL)
    },
    .package = "chatlens"
  )

  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))

  simple_out <- cl_periods_select(chat, select = "day", data_type = "simple")
  out <- cl_periods_analyze(simple_out, prompt = "summarize", return = "standard")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_false("period" %in% names(out))
  expect_false("key" %in% names(out))
  expect_equal(out$analysis_text, "ok")
})

test_that("cl_whatsapp_summary prints clean labels and data-quality notes", {
  df <- data.frame(
    message_id = 1:2,
    timestamp = as.POSIXct(c(NA, "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c(NA_character_, "A"),
    text = c("file attached", "hello"),
    message_type = c(NA_character_, "text"),
    stringsAsFactors = FALSE
  )
  df$attachments <- I(list(NA_character_, character(0)))
  df$attachment_types <- I(list(NA_character_, character(0)))
  df$attachment_paths <- I(list(NA_character_, character(0)))
  df$attachment_statuses <- I(list("placeholder", character(0)))
  df$attachment_keys <- I(list(NA_character_, character(0)))
  df$attachment_placeholder <- c(TRUE, FALSE)
  df$attachment_omitted <- c(FALSE, FALSE)
  df$attachment <- c(NA_character_, NA_character_)
  df$attachment_type <- c(NA_character_, NA_character_)

  chat <- chatlens:::new_chatlens_chat(df, source = list(tz = "UTC"))
  out <- capture.output(res <- cl_whatsapp_summary(chat))

  expect_false(any(grepl("<NA>", out, fixed = TRUE)))
  expect_true(any(grepl("Data Quality Notes", out, fixed = TRUE)))
  expect_true(any(grepl("missing timestamps", out, fixed = TRUE)))
  expect_true(any(grepl("unknown", out, fixed = TRUE)))
  expect_true(length(res$notes) > 0)
})

test_that("analysis response label uses first 10 chars", {
  expect_equal(chatlens:::cl_response_label("Hello world from model", n = 10), "hello_worl")
  expect_equal(chatlens:::cl_response_label("   ", n = 10), "analysis")
  expect_equal(chatlens:::cl_response_label(NA_character_, n = 10), "analysis")
})

test_that("cl_periods_analyze accepts aggregated text input", {
  testthat::local_mocked_bindings(
    cl_analyze_text = function(...) {
      list(text = "ok", meta = list(source = "mock"), saved_file = NULL, meta_file = NULL)
    },
    .package = "chatlens"
  )

  out <- cl_periods_analyze("line one\nline two", prompt = "summarize", return = "standard")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(out$period, "text")
  expect_equal(out$key, "input")
  expect_equal(out$analysis_text, "ok")
})
