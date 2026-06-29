test_that("audio transcript cache is keyed by attachment key, not filename", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 11:00:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("AUDIO.mp3 (file attached)", "AUDIO.mp3 (file attached)"),
    message_id = c(1L, 2L),
    attachment = c("AUDIO.mp3", "AUDIO.mp3"),
    attachment_type = c("audio", "audio"),
    attachment_path = c("/tmp/a/AUDIO.mp3", "/tmp/b/AUDIO.mp3"),
    attachment_key = c("path:/tmp/a/AUDIO.mp3", "path:/tmp/b/AUDIO.mp3"),
    stringsAsFactors = FALSE
  )
  df$attachments <- list("AUDIO.mp3", "AUDIO.mp3")
  df$attachment_types <- list("audio", "audio")
  df$attachment_paths <- list("/tmp/a/AUDIO.mp3", "/tmp/b/AUDIO.mp3")
  df$attachment_keys <- list("path:/tmp/a/AUDIO.mp3", "path:/tmp/b/AUDIO.mp3")
  df$attachment_statuses <- list("present", "present")

  chat <- chatlens:::.clh_new_chat(df, chat_key = "my_chat", zip_id = "zip1")

  store <- chatlens:::.clh_chat_store_dir("my_chat", cache_dir = cache_dir)
  manifest_path <- file.path(store, "audio_manifest.json")
  manifest <- list(
    updated_at = NULL,
    items = list(
      "path:/tmp/a/AUDIO.mp3" = list(
        attachment_key = "path:/tmp/a/AUDIO.mp3",
        attachment = "AUDIO.mp3",
        status = "processed",
        service = "replicate",
        model = "openai/whisper",
        transcript = "first transcript",
        sources = "zip1"
      ),
      "path:/tmp/b/AUDIO.mp3" = list(
        attachment_key = "path:/tmp/b/AUDIO.mp3",
        attachment = "AUDIO.mp3",
        status = "processed",
        service = "replicate",
        model = "openai/whisper",
        transcript = "second transcript",
        sources = "zip1"
      )
    )
  )
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  out <- cl_chat_transcribe_audio(chat, cache_dir = cache_dir, verbose = FALSE, overwrite = FALSE)
  expect_equal(unname(out$audio_transcripts[[1]]), "first transcript")
  expect_equal(unname(out$audio_transcripts[[2]]), "second transcript")

  current_path <- file.path(store, "chat.rds")
  current_txt <- file.path(store, "chat.txt")
  expect_true(file.exists(current_path))
  expect_true(file.exists(current_txt))
  expect_equal(readRDS(current_path)$audio_transcript, c("first transcript", "second transcript"))

  txt <- paste(readLines(current_txt, warn = FALSE), collapse = "\n")
  expect_true(grepl("first transcript", txt, fixed = TRUE))
  expect_true(grepl("second transcript", txt, fixed = TRUE))
})

test_that("media processing updates current chat text backup", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "AUDIO.mp3 (file attached)",
    message_id = 1L,
    attachment = "AUDIO.mp3",
    attachment_type = "audio",
    stringsAsFactors = FALSE
  )
  df$attachments <- list("AUDIO.mp3")
  df$audio_transcripts <- list(c("AUDIO.mp3" = "voice note text"))

  chat <- chatlens:::.clh_new_chat(df, chat_key = "processed_chat", zip_id = "zip1")
  out <- cl_chat_process_media(chat, cache_dir = cache_dir)

  store <- chatlens:::.clh_chat_store_dir("processed_chat", cache_dir = cache_dir)
  current_rds <- file.path(store, "chat.rds")
  current_txt <- file.path(store, "chat.txt")

  expect_true(file.exists(current_rds))
  expect_true(file.exists(current_txt))
  expect_true(grepl("voice note text", readRDS(current_rds)$text_enriched[1], fixed = TRUE))

  txt <- paste(readLines(current_txt, warn = FALSE), collapse = "\n")
  expect_true(grepl("voice note text", txt, fixed = TRUE))
  expect_equal(out$text_enriched, readRDS(current_rds)$text_enriched)
})

test_that("audio transcription errors print and persist useful provider reasons", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  audio_path <- file.path(cache_dir, "PTT-1.mp3")
  writeLines("fake audio bytes", audio_path)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "PTT-1.mp3 (file attached)",
    message_id = 1L,
    attachment = "PTT-1.mp3",
    attachment_type = "audio",
    attachment_path = audio_path,
    attachment_key = paste0("path:", audio_path),
    stringsAsFactors = FALSE
  )
  df$attachments <- list("PTT-1.mp3")
  df$attachment_types <- list("audio")
  df$attachment_paths <- list(audio_path)
  df$attachment_keys <- list(paste0("path:", audio_path))
  df$attachment_statuses <- list("present")

  chat <- chatlens:::.clh_new_chat(df, chat_key = "audio_error_chat", zip_id = "zip1")

  testthat::local_mocked_bindings(
    gen_stt = function(...) {
      "API_ERROR: invalid model openai/whisper for provider groq"
    },
    .package = "genflow"
  )

  out <- NULL
  messages <- capture.output(
    out <- cl_chat_transcribe_audio(
      chat,
      service = "groq",
      model = "openai/whisper",
      cache_dir = cache_dir,
      verbose = TRUE,
      overwrite = TRUE
    ),
    type = "message"
  )

  expect_true(any(grepl("API_ERROR: invalid model openai/whisper", messages, fixed = TRUE)))
  expect_true(any(grepl("[groq/openai/whisper]", messages, fixed = TRUE)))
  expect_true(is.na(out$audio_transcript[1]))

  store <- chatlens:::.clh_chat_store_dir("audio_error_chat", cache_dir = cache_dir)
  manifest <- jsonlite::read_json(file.path(store, "audio_manifest.json"), simplifyVector = FALSE)
  key <- paste0("path:", audio_path)

  expect_equal(manifest$items[[key]]$status, "error")
  expect_equal(
    manifest$items[[key]]$error_message,
    "API_ERROR: invalid model openai/whisper for provider groq"
  )
})

test_that("media functions do not fail when chat_key is missing", {
  chat <- chatlens:::.clh_new_chat(
    data.frame(
      timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
      sender = "A",
      text = "hello",
      message_id = 1L,
      stringsAsFactors = FALSE
    ),
    source = list(tz = "UTC")
  )
  chat$attachments <- list(character(0))
  chat$attachment_types <- list(character(0))

  out_audio <- NULL
  expect_warning(
    out_audio <- cl_chat_transcribe_audio(chat, verbose = FALSE),
    "No attachments found."
  )
  expect_true("audio_transcript" %in% names(out_audio))

  out_image <- NULL
  expect_warning(
    out_image <- cl_chat_describe_images(chat, verbose = FALSE),
    "No attachments found."
  )
  expect_true("image_description" %in% names(out_image))
})

test_that("image cache reuse respects prompt", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  img_path <- file.path(cache_dir, "IMG.jpg")
  writeLines("fake image bytes", img_path)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "IMG.jpg (file attached)",
    message_id = 1L,
    attachment = "IMG.jpg",
    attachment_type = "image",
    attachment_path = img_path,
    attachment_key = paste0("path:", img_path),
    stringsAsFactors = FALSE
  )
  df$attachments <- list("IMG.jpg")
  df$attachment_types <- list("image")
  df$attachment_paths <- list(img_path)
  df$attachment_keys <- list(paste0("path:", img_path))
  df$attachment_statuses <- list("present")

  chat <- chatlens:::.clh_new_chat(df, chat_key = "my_chat", zip_id = "zip1")

  store <- chatlens:::.clh_chat_store_dir("my_chat", cache_dir = cache_dir)
  desc_dir <- file.path(store, "image_descriptions")
  dir.create(desc_dir, recursive = TRUE)
  desc_path <- file.path(desc_dir, "IMG.txt")
  writeLines("cached prompt a", desc_path)

  manifest_path <- file.path(store, "image_manifest.json")
  key <- paste0("path:", img_path)
  items <- list()
  items[[key]] <- list(
    attachment_key = key,
    attachment = "IMG.jpg",
    status = "processed",
    service = "openrouter",
    model = "google/gemini-3-flash-preview",
    prompt = "Prompt A",
    description = "cached prompt a",
    saved_file = desc_path,
    sources = "zip1"
  )
  manifest <- list(
    updated_at = NULL,
    items = items
  )
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  calls <- 0L
  testthat::local_mocked_bindings(
    gen_txt = function(prompt, add_img, ...) {
      calls <<- calls + 1L
      paste("generated", prompt)
    },
    .package = "genflow"
  )

  out_same <- cl_chat_describe_images(chat, prompt = "Prompt A", cache_dir = cache_dir, verbose = FALSE)
  expect_equal(calls, 0L)
  expect_true(grepl("cached prompt a", out_same$image_description[1], fixed = TRUE))

  out_diff <- cl_chat_describe_images(chat, prompt = "Prompt B", cache_dir = cache_dir, verbose = FALSE)
  expect_equal(calls, 1L)
  expect_true(grepl("generated Prompt B", out_diff$image_description[1], fixed = TRUE))

  manifest2 <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  expect_equal(manifest2$items[[key]]$prompt, "Prompt B")
})

test_that("image API errors are not saved as text files", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  img_path <- file.path(cache_dir, "ERR.jpg")
  writeLines("fake image bytes", img_path)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "ERR.jpg (file attached)",
    message_id = 1L,
    attachment = "ERR.jpg",
    attachment_type = "image",
    attachment_path = img_path,
    attachment_key = paste0("path:", img_path),
    stringsAsFactors = FALSE
  )
  df$attachments <- list("ERR.jpg")
  df$attachment_types <- list("image")
  df$attachment_paths <- list(img_path)
  df$attachment_keys <- list(paste0("path:", img_path))
  df$attachment_statuses <- list("present")

  chat <- chatlens:::.clh_new_chat(df, chat_key = "my_chat_error", zip_id = "zip1")
  store <- chatlens:::.clh_chat_store_dir("my_chat_error", cache_dir = cache_dir)
  desc_dir <- file.path(store, "image_descriptions")
  dir.create(desc_dir, recursive = TRUE)
  desc_path <- file.path(desc_dir, "ERR.txt")
  writeLines("stale previous description", desc_path)

  calls <- 0L
  error_msgs <- c("API_ERROR: model timeout", "Bad Request: invalid image payload")
  testthat::local_mocked_bindings(
    gen_txt = function(prompt, add_img, ...) {
      calls <<- calls + 1L
      error_msgs[[calls]]
    },
    .package = "genflow"
  )

  out1 <- cl_chat_describe_images(chat, cache_dir = cache_dir, verbose = FALSE, overwrite = TRUE)
  out2 <- cl_chat_describe_images(chat, cache_dir = cache_dir, verbose = FALSE, overwrite = TRUE)

  manifest_path <- file.path(store, "image_manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  key <- paste0("path:", img_path)

  expect_equal(calls, 2L)
  expect_false(file.exists(desc_path))
  expect_true(is.na(out1$image_description[1]))
  expect_true(is.na(out2$image_description[1]))
  expect_equal(manifest$items[[key]]$status, "error")
  expect_equal(manifest$items[[key]]$error_message, "Bad Request: invalid image payload")
})
