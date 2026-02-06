make_chat <- function(df, source = list(tz = "UTC", store_dir = tempfile("chatlens_store_"))) {
  chatlens:::new_chatlens_chat(df, source = source, chat_key = "chat")
}

test_that("period slicing handles empty chat rows without error", {
  df <- data.frame(
    timestamp = as.POSIXct(character(0), tz = "UTC"),
    sender = character(0),
    text = character(0),
    stringsAsFactors = FALSE
  )
  chat <- make_chat(df)

  periods <- cl_chat_split_periods(chat, period = c("year", "month", "week", "day"))
  expect_s3_class(periods, "data.frame")
  expect_equal(nrow(periods), 0)
})

test_that("period slicing drops NA timestamps for time periods", {
  df <- data.frame(
    timestamp = as.POSIXct(c(NA, NA), tz = "UTC"),
    sender = c("A", "B"),
    text = c("x", "y"),
    stringsAsFactors = FALSE
  )
  chat <- make_chat(df)

  periods <- cl_chat_split_periods(chat, period = c("year", "week", "day"))
  expect_equal(nrow(periods), 0)
})

test_that("week period uses ISO year-week keys", {
  df <- data.frame(
    timestamp = as.POSIXct("2024-12-30 10:00:00", tz = "UTC"),
    sender = "A",
    text = "hello",
    stringsAsFactors = FALSE
  )
  chat <- make_chat(df)

  periods <- cl_chat_split_periods(chat, period = "week")
  expect_equal(periods$key, "2025-W01")
})
