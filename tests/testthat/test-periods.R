make_chat <- function(df, source = list(tz = "UTC", store_dir = tempfile("chatlens_store_"))) {
  chatlens:::.clh_new_chat(df, source = source, chat_key = "chat")
}

test_that("analysis preparation handles empty chat rows without error", {
  df <- data.frame(
    timestamp = as.POSIXct(character(0), tz = "UTC"),
    sender = character(0),
    text = character(0),
    stringsAsFactors = FALSE
  )
  chat <- make_chat(df)

  prepared <- cl_prepare_analysis(chat, period = "day", save = FALSE)
  expect_s3_class(prepared, "data.frame")
  expect_s3_class(prepared, "chatlens_analysis_input")
  expect_equal(nrow(prepared), 0)
})

test_that("analysis preparation drops NA timestamps for time periods", {
  df <- data.frame(
    timestamp = as.POSIXct(c(NA, NA), tz = "UTC"),
    sender = c("A", "B"),
    text = c("x", "y"),
    stringsAsFactors = FALSE
  )
  chat <- make_chat(df)

  prepared <- cl_prepare_analysis(chat, period = "week", save = FALSE)
  expect_equal(nrow(prepared), 0)
})

test_that("week period uses ISO year-week keys", {
  df <- data.frame(
    timestamp = as.POSIXct("2024-12-30 10:00:00", tz = "UTC"),
    sender = "A",
    text = "hello",
    stringsAsFactors = FALSE
  )
  chat <- make_chat(df)

  prepared <- cl_prepare_analysis(chat, period = "week", save = FALSE)
  expect_equal(prepared$key, "2025-W01")
})
