.legacy_format_chat_simple <- function(chat,
                                       text_col = "text_enriched",
                                       date_format = "%Y-%m-%d",
                                       time_format = "%H:%M") {
  chat <- chatlens:::.clh_chat_text_snapshot(chat, text_col = text_col)
  if (!text_col %in% names(chat)) text_col <- "text"
  if (nrow(chat) == 0L) return("")

  ts <- chat$timestamp
  date_key <- ifelse(is.na(ts), "unknown date", format(ts, date_format))
  time_key <- ifelse(is.na(ts), "unknown time", format(ts, time_format))
  sender <- ifelse(is.na(chat$sender) | !nzchar(chat$sender), "SYSTEM", chat$sender)
  text <- as.character(chat[[text_col]])
  text[is.na(text)] <- ""

  lines <- character(0)
  current_date <- NULL
  current_sender <- NULL
  for (i in seq_len(nrow(chat))) {
    if (!identical(current_date, date_key[i])) {
      if (length(lines) > 0L) lines <- c(lines, "")
      lines <- c(lines, date_key[i], "")
      current_date <- date_key[i]
      current_sender <- NULL
    }
    if (!identical(current_sender, sender[i])) {
      if (!is.null(current_sender)) lines <- c(lines, "")
      lines <- c(lines, sprintf("%s %s", time_key[i], sender[i]))
      current_sender <- sender[i]
    }
    lines <- c(lines, text[i])
  }
  paste(lines, collapse = "\n")
}

.prepare_edge_case_chat <- function() {
  df <- data.frame(
    timestamp = as.POSIXct(
      c(
        "2025-01-01 10:00:00", "2025-01-01 10:01:00",
        "2025-01-01 10:02:00", NA, NA, "2025-01-02 09:00:00"
      ),
      tz = "UTC"
    ),
    sender = c("Alice", "Alice", "Bob", NA, "", "Alice"),
    text = c("one", "two\ncontinued", NA, "system note", "again", "next"),
    stringsAsFactors = FALSE
  )
  df$text_enriched <- df$text
  chatlens:::.clh_new_chat(df, source = list(tz = "UTC"), chat_key = "prepare_edges")
}

test_that("linear simple formatter preserves exact legacy output", {
  chat <- .prepare_edge_case_chat()
  expected <- paste(
    c(
      "2025-01-01", "", "10:00 Alice", "one", "two\ncontinued", "",
      "10:02 Bob", "", "", "unknown date", "", "unknown time SYSTEM",
      "system note", "again", "", "2025-01-02", "", "09:00 Alice", "next"
    ),
    collapse = "\n"
  )

  expect_identical(chatlens:::.clh_format_chat_simple(chat), expected)
  expect_identical(
    chatlens:::.clh_format_chat_simple(chat),
    .legacy_format_chat_simple(chat)
  )
})

test_that("simple formatter matches legacy behavior across transitions", {
  timestamps <- as.POSIXct("2025-01-01 08:00:00", tz = "UTC") +
    c(0, 60, 120, 86400, 86460, 172800, 172860, 172920)
  df <- data.frame(
    timestamp = c(timestamps, as.POSIXct(NA, tz = "UTC")),
    sender = c("A", "A", "B", "B", "B", "A", NA, "", "A"),
    text = c("a", "", "b\nline", "c", NA, "d", "e", "f", "g"),
    stringsAsFactors = FALSE
  )
  df$text_enriched <- df$text
  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"))

  expect_identical(
    chatlens:::.clh_format_chat_simple(chat),
    .legacy_format_chat_simple(chat)
  )

  selected <- c(1L, 3L, 5L, 8L)
  expect_identical(
    chatlens:::.clh_format_chat_simple(chat, rows = selected),
    .legacy_format_chat_simple(chatlens:::.clh_subset_chat(chat, selected))
  )
  expect_identical(
    chatlens:::.clh_format_chat_simple(chat, rows = integer(0)),
    ""
  )

  trailing <- chatlens:::.clh_subset_chat(chat, 1:2)
  trailing$text_enriched[2] <- ""
  trailing_text <- chatlens:::.clh_format_chat_simple(trailing)
  expect_identical(trailing_text, .legacy_format_chat_simple(trailing))
  expect_true(endsWith(trailing_text, "\n"))

  # Preserve the old scalar `identical()` behavior even for unusually named
  # columns, rather than silently changing grouping semantics during the speedup.
  names(chat$sender) <- paste0("sender_", seq_len(nrow(chat)))
  names(chat$timestamp) <- paste0("time_", seq_len(nrow(chat)))
  expect_identical(
    chatlens:::.clh_format_chat_simple(chat),
    .legacy_format_chat_simple(chat)
  )
})

test_that("analysis preparation formats row groups without full chat slices", {
  chat <- .prepare_edge_case_chat()
  chat$irrelevant_media <- lapply(seq_len(nrow(chat)), function(i) {
    raw(1024L + i)
  })
  cache_dir <- tempfile("chatlens_prepare_rows_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .clh_subset_chat = function(...) stop("full chat slice should not be used"),
    .package = "chatlens"
  )
  prepared <- cl_prepare_analysis(
    chat,
    period = "day",
    save = FALSE,
    cache_dir = cache_dir
  )

  expect_identical(prepared$key, c("2025-01-01", "2025-01-02"))
  expect_identical(
    prepared$text[1],
    chatlens:::.clh_format_chat_simple(chat, rows = 1:3)
  )
  expect_identical(
    prepared$text[2],
    chatlens:::.clh_format_chat_simple(chat, rows = 6L)
  )
})

test_that("row-indexed raw and custom text formatting preserve contracts", {
  chat <- .prepare_edge_case_chat()
  chat$custom_text <- paste0("custom ", seq_len(nrow(chat)))
  cache_dir <- tempfile("chatlens_prepare_text_columns_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  raw <- cl_prepare_analysis(
    chat,
    period = "day",
    formatting = "raw",
    save = FALSE,
    cache_dir = cache_dir
  )
  raw_lines <- sprintf(
    "%s - %s: %s",
    format(chat$timestamp, "%Y-%m-%d %H:%M:%S"),
    ifelse(is.na(chat$sender), "SYSTEM", chat$sender),
    chat$text_enriched
  )
  expect_identical(raw$text[1], paste(raw_lines[1:3], collapse = "\n"))
  expect_identical(raw$text[2], raw_lines[6])

  custom <- cl_prepare_analysis(
    chat,
    text_col = "custom_text",
    save = FALSE,
    cache_dir = cache_dir
  )
  expect_true(grepl("custom 1", custom$text, fixed = TRUE))
  expect_true(grepl("custom 6", custom$text, fixed = TRUE))

  chat$text_enriched <- NULL
  fallback <- cl_prepare_analysis(chat, save = FALSE, cache_dir = cache_dir)
  expect_identical(
    fallback$text,
    chatlens:::.clh_format_chat_simple(chat, text_col = "text")
  )
})

test_that("analysis preparation materializes enriched text once", {
  df <- data.frame(
    timestamp = as.POSIXct(
      c("2025-01-01 10:00:00", "2025-01-02 10:00:00"),
      tz = "UTC"
    ),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  df$audio_transcripts <- list(c(a = "first"), c(b = "second"))
  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"), chat_key = "prepare_media_once")
  cache_dir <- tempfile("chatlens_prepare_media_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  calls <- 0L

  testthat::local_mocked_bindings(
    cl_chat_process_media = function(chat, save_chat = FALSE, ...) {
      calls <<- calls + 1L
      chat$text_enriched <- paste0(chat$text, " enriched")
      chat
    },
    .package = "chatlens"
  )
  prepared <- cl_prepare_analysis(
    chat,
    period = "day",
    save = FALSE,
    cache_dir = cache_dir
  )

  expect_equal(calls, 1L)
  expect_true(all(grepl("enriched", prepared$text, fixed = TRUE)))
})

test_that("saved analysis RDS and text contain the same prepared payload", {
  chat <- .prepare_edge_case_chat()
  cache_dir <- tempfile("chatlens_prepare_save_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  prepared <- cl_prepare_analysis(chat, cache_dir = cache_dir)

  saved <- readRDS(prepared$input_rds)
  saved_text <- paste(readLines(prepared$input_file, warn = FALSE), collapse = "\n")
  expect_identical(saved$text, prepared$text)
  expect_identical(saved_text, prepared$text)
})
