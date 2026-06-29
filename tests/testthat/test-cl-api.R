test_that("cl wrappers map to core functions", {
  expect_true(dir.exists(chatlens:::.clh_cache_dir()))
  expect_equal(chatlens:::.clh_cache_dir(NULL), chatlens:::.clh_cache_dir())
  custom_cache <- tempfile("chatlens_cache_")
  on.exit(unlink(custom_cache, recursive = TRUE), add = TRUE)
  expect_equal(chatlens:::.clh_cache_dir(custom_cache), path.expand(custom_cache))
  expect_true(dir.exists(custom_cache))
  expect_type(cl_whatsapp_import, "closure")
})

test_that(".clh_chat_filter_period filters by day key", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-02 10:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"))

  filtered <- chatlens:::.clh_chat_filter_period(chat, period = "day", key = "2025-01-01")
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$text, "one")
})

test_that("cl_prepare_analysis saves all-chat input under analysis/all", {
  cache_dir <- tempfile("chatlens_cache_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:01:00"), tz = "UTC"),
    sender = c("Alice", "Alice"),
    text = c("one", "two"),
    text_enriched = c("one", "two"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"), chat_key = "prepare_all")

  prepared <- cl_prepare_analysis(chat, cache_dir = cache_dir)
  store <- chatlens:::.clh_chat_store_dir("prepare_all", cache_dir = cache_dir)
  input_dir <- file.path(store, "analysis", "all")

  expect_s3_class(prepared, "chatlens_analysis_input")
  expect_equal(nrow(prepared), 1)
  expect_equal(prepared$period, "all")
  expect_equal(prepared$key, "all")
  expect_equal(prepared$input_dir, input_dir)
  expect_true(file.exists(file.path(input_dir, "input.txt")))
  expect_true(file.exists(file.path(input_dir, "input.rds")))

  txt <- paste(readLines(file.path(input_dir, "input.txt"), warn = FALSE), collapse = "\n")
  expect_true(grepl("10:00 Alice\none\ntwo", txt, fixed = TRUE))
  expect_false(grepl("2025-01-01 10:00:00 - Alice", txt, fixed = TRUE))
})

test_that("cl_prepare_analysis day selection writes one input per selected day", {
  cache_dir <- tempfile("chatlens_cache_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct(
      c("2020-10-01 09:00:00", "2020-10-02 09:00:00", "2020-11-01 09:00:00"),
      tz = "UTC"
    ),
    sender = c("Alice", "Bob", "Alice"),
    text = c("oct one", "oct two", "nov one"),
    text_enriched = c("oct one", "oct two", "nov one"),
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"), chat_key = "prepare_days")

  prepared <- cl_prepare_analysis(
    chat,
    period = "day",
    select = "2020-10",
    cache_dir = cache_dir
  )
  store <- chatlens:::.clh_chat_store_dir("prepare_days", cache_dir = cache_dir)

  expect_equal(prepared$key, c("2020-10-01", "2020-10-02"))
  expect_true(file.exists(file.path(store, "analysis", "by_day", "2020", "10", "01", "input.txt")))
  expect_true(file.exists(file.path(store, "analysis", "by_day", "2020", "10", "02", "input.txt")))
  expect_false(dir.exists(file.path(store, "analysis", "by_day", "2020", "11", "01")))
})

test_that("cl_prepare_analysis explicit cache_dir overrides stored chat location", {
  source_store <- tempfile("chatlens_source_store_")
  cache_dir <- tempfile("chatlens_cache_")
  on.exit(unlink(source_store, recursive = TRUE), add = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "Alice",
    text = "hello",
    text_enriched = "hello",
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(
    df,
    source = list(tz = "UTC", store_dir = source_store),
    chat_key = "prepare_override"
  )

  prepared <- cl_prepare_analysis(chat, cache_dir = cache_dir)
  expected_store <- chatlens:::.clh_chat_store_dir("prepare_override", cache_dir = cache_dir)

  expect_true(startsWith(prepared$input_dir, file.path(expected_store, "analysis")))
  expect_false(startsWith(prepared$input_dir, source_store))
})

test_that("cl_analyze_chat saves prompt result metadata and run files with provider model names", {
  cache_dir <- tempfile("chatlens_cache_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "Alice",
    text = "hello",
    text_enriched = "hello",
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"), chat_key = "analyze_chat")
  prepared <- cl_prepare_analysis(chat, cache_dir = cache_dir)

  testthat::local_mocked_bindings(
    gen_txt = function(prompt, add, ...) {
      list(text = paste("analysis:", prompt, add), status_api = "SUCCESS")
    },
    .package = "genflow"
  )

  out <- cl_analyze_chat(
    prepared,
    prompt = "summarize",
    service = "openai",
    model = "gpt-5.2"
  )

  expect_s3_class(out, "chatlens_analysis_result")
  expect_equal(out$analysis_text, "analysis: summarize 2025-01-01\n\n10:00 Alice\nhello")
  expect_true(file.exists(out$prompt_file))
  expect_true(file.exists(out$result_file))
  expect_true(file.exists(out$result_rds))
  expect_true(file.exists(out$meta_file))
  expect_true(file.exists(out$run_file))
  expect_true(grepl("openai_gpt-5_2", basename(out$result_file), fixed = TRUE))
  expect_equal(paste(readLines(out$prompt_file, warn = FALSE), collapse = "\n"), "summarize")
  expect_equal(paste(readLines(out$result_file, warn = FALSE), collapse = "\n"), out$analysis_text)
  expect_equal(readRDS(out$result_rds)$analysis_text, out$analysis_text)
})

test_that("anonymize preserves original and updates current chat backups", {
  cache_dir <- tempfile("chatlens_cache_")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  store_dir <- file.path(cache_dir, "whatsapp", "chats", "secret_chat")

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "Alice",
    text = "Alice says hello",
    text_enriched = "Alice says hello [enriched]",
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(
    df,
    source = list(tz = "UTC"),
    chat_key = "secret_chat"
  )
  alias_map <- list(items = list(
    list(person = "Alice", alias = "Alex", variants = "Alice")
  ))

  out <- cl_chat_anonymize(
    chat,
    alias_map = alias_map,
    save_original = TRUE,
    cache_dir = cache_dir
  )

  original_rds_path <- file.path(store_dir, "chat_original.rds")
  original_txt_path <- file.path(store_dir, "chat_original.txt")
  chat_rds_path <- file.path(store_dir, "chat.rds")
  chat_txt_path <- file.path(store_dir, "chat.txt")

  expect_true(file.exists(original_rds_path))
  expect_true(file.exists(original_txt_path))
  expect_true(file.exists(chat_rds_path))
  expect_true(file.exists(chat_txt_path))
  expect_equal(readRDS(original_rds_path)$sender, "Alice")
  expect_equal(readRDS(chat_rds_path)$sender, "Alex")
  expect_equal(readRDS(chat_rds_path)$text_enriched, "Alex says hello [enriched]")
  expect_equal(out$sender, "Alex")

  original_txt <- paste(readLines(original_txt_path, warn = FALSE), collapse = "\n")
  chat_txt <- paste(readLines(chat_txt_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("2025-01-01 10:00:00 - Alice: Alice says hello [enriched]", original_txt, fixed = TRUE))
  expect_true(grepl("2025-01-01 10:00:00 - Alex: Alex says hello [enriched]", chat_txt, fixed = TRUE))

  writeLines("stale transcript", chat_txt_path)
  df$text_enriched <- "Alice fresh mirrored text"
  chat <- chatlens:::.clh_new_chat(
    df,
    source = list(tz = "UTC"),
    chat_key = "secret_chat"
  )

  cl_chat_anonymize(
    chat,
    alias_map = alias_map,
    save_original = TRUE,
    cache_dir = cache_dir
  )

  original_txt <- paste(readLines(original_txt_path, warn = FALSE), collapse = "\n")
  chat_txt <- paste(readLines(chat_txt_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("2025-01-01 10:00:00 - Alice: Alice says hello [enriched]", original_txt, fixed = TRUE))
  expect_false(grepl("stale transcript", chat_txt, fixed = TRUE))
  expect_true(grepl("2025-01-01 10:00:00 - Alex: Alex fresh mirrored text", chat_txt, fixed = TRUE))
  expect_equal(readRDS(original_rds_path)$text_enriched, "Alice says hello [enriched]")
  expect_equal(readRDS(chat_rds_path)$text_enriched, "Alex fresh mirrored text")
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

  chat <- chatlens:::.clh_new_chat(df, source = list(tz = "UTC"))
  out <- capture.output(res <- cl_whatsapp_summary(chat))

  expect_false(any(grepl("<NA>", out, fixed = TRUE)))
  expect_true(any(grepl("Data Quality Notes", out, fixed = TRUE)))
  expect_true(any(grepl("missing timestamps", out, fixed = TRUE)))
  expect_true(any(grepl("unknown", out, fixed = TRUE)))
  expect_true(length(res$notes) > 0)
})
