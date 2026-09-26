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

test_that("audio preflight excludes import-time missing references", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  audio_path <- file.path(cache_dir, "PTT-available.mp3")
  writeLines("fake audio bytes", audio_path)
  present_key <- paste0("path:", audio_path)
  missing_key <- "name:missing-audio.m4a"

  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:01:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("PTT-available.mp3 (file attached)", "<Media omitted>\nMissing-audio.m4a"),
    message_id = c(1L, 2L),
    attachment = c("PTT-available.mp3", "Missing-audio.m4a"),
    attachment_type = c("audio", "audio"),
    attachment_path = c(audio_path, NA_character_),
    attachment_key = c(present_key, missing_key),
    stringsAsFactors = FALSE
  )
  df$attachments <- list("PTT-available.mp3", "Missing-audio.m4a")
  df$attachment_types <- list("audio", "audio")
  df$attachment_paths <- list(audio_path, NA_character_)
  df$attachment_keys <- list(present_key, missing_key)
  df$attachment_statuses <- list("present", "missing")

  chat <- chatlens:::.clh_new_chat(df, chat_key = "audio_preflight_chat", zip_id = "zip1")
  calls <- 0L
  testthat::local_mocked_bindings(
    gen_stt = function(...) {
      calls <<- calls + 1L
      "available transcript"
    },
    .package = "genflow"
  )

  out <- NULL
  messages <- NULL
  expect_warning(
    messages <- capture.output(
      out <- cl_chat_transcribe_audio(
        chat,
        cache_dir = cache_dir,
        verbose = TRUE,
        overwrite = TRUE
      ),
      type = "message"
    ),
    NA
  )

  expect_equal(calls, 1L)
  expect_true(any(grepl(
    "Audio preflight: 2 unique audio references; 1 file available; 1 missing reference skipped.",
    messages,
    fixed = TRUE
  )))
  expect_true(any(grepl("Transcribing audio (1 of 1", messages, fixed = TRUE)))
  expect_false(any(grepl("1 of 2", messages, fixed = TRUE)))
  expect_equal(out$audio_transcript[1], "available transcript")
  expect_true(is.na(out$audio_transcript[2]))

  store <- chatlens:::.clh_chat_store_dir("audio_preflight_chat", cache_dir = cache_dir)
  manifest <- jsonlite::read_json(file.path(store, "audio_manifest.json"), simplifyVector = FALSE)
  expect_equal(manifest$items[[missing_key]]$status, "missing")
  expect_equal(manifest$items[[missing_key]]$missing_reason, "missing_at_import")
})

test_that("media functions do not fail when chat_key is missing", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

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
    out_audio <- cl_chat_transcribe_audio(chat, cache_dir = cache_dir, verbose = FALSE),
    "No attachments found."
  )
  expect_true("audio_transcript" %in% names(out_audio))

  out_image <- NULL
  expect_warning(
    out_image <- cl_chat_describe_images(chat, cache_dir = cache_dir, verbose = FALSE),
    "No attachments found."
  )
  expect_true("image_description" %in% names(out_image))
})

test_that("image cache reuse obeys overwrite", {
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
    gen_batch_agent = .clh_test_gen_batch_agent,
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
  expect_equal(calls, 0L)
  expect_true(grepl("cached prompt a", out_diff$image_description[1], fixed = TRUE))

  out_overwrite <- cl_chat_describe_images(
    chat,
    prompt = "Prompt B",
    model = "new-model",
    cache_dir = cache_dir,
    verbose = FALSE,
    overwrite = TRUE
  )
  expect_equal(calls, 1L)
  expect_true(grepl("generated Prompt B", out_overwrite$image_description[1], fixed = TRUE))

  index_path <- file.path(desc_dir, "image_manifest.json")
  index <- jsonlite::read_json(index_path, simplifyVector = FALSE)
  image_id <- index$attachment_index[[key]]
  image <- index$images[[image_id]]
  image_dir <- file.path(desc_dir, image_id)
  text_files <- list.files(
    file.path(image_dir, "descriptions"),
    pattern = "\\.txt$",
    full.names = TRUE
  )
  metadata_files <- list.files(
    file.path(image_dir, "descriptions"),
    pattern = "\\.json$",
    full.names = TRUE
  )
  active_metadata <- jsonlite::read_json(
    file.path(desc_dir, image$descriptions[[image$active_description_id]]$metadata_file),
    simplifyVector = FALSE
  )

  expect_equal(index$schema_version, 2L)
  expect_equal(length(text_files), 2L)
  expect_equal(length(metadata_files), 2L)
  expect_equal(image$description_count, 2L)
  expect_equal(active_metadata$prompt, "Prompt B")
  expect_equal(active_metadata$model, "new-model")
  expect_true(file.exists(file.path(image_dir, "image.json")))
  expect_true(file.exists(desc_path))

  legacy_manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  expect_equal(legacy_manifest$items[[key]]$prompt, "Prompt A")
})

test_that("configuration cache mode resumes an exact model and prompt", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  paths <- file.path(cache_dir, c("CONFIG_ONE.jpg", "CONFIG_TWO.jpg"))
  writeLines("first image", paths[1])
  writeLines("second image", paths[2])
  keys <- paste0("path:", paths)
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:01:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = paste(basename(paths), "(file attached)"),
    message_id = c(1L, 2L),
    attachment = basename(paths),
    attachment_type = c("image", "image"),
    attachment_path = paths,
    attachment_key = keys,
    stringsAsFactors = FALSE
  )
  df$attachments <- as.list(df$attachment)
  df$attachment_types <- list("image", "image")
  df$attachment_paths <- as.list(paths)
  df$attachment_keys <- as.list(keys)
  df$attachment_statuses <- list("present", "present")
  chat <- chatlens:::.clh_new_chat(df, chat_key = "configured_images", zip_id = "zip1")

  calls <- character(0)
  testthat::local_mocked_bindings(
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(prompt, add_img, ...) {
      call <- paste(prompt, basename(add_img), sep = "|")
      calls <<- c(calls, call)
      if (identical(call, "Prompt B|CONFIG_TWO.jpg") && sum(calls == call) == 1L) {
        return("API_ERROR: temporary failure")
      }
      paste(prompt, basename(add_img))
    },
    .package = "genflow"
  )

  out_a <- cl_chat_describe_images(
    chat,
    prompt = "Prompt A",
    model = "model-a",
    cache_dir = cache_dir,
    cache_mode = "force",
    verbose = FALSE
  )
  out_b1 <- cl_chat_describe_images(
    chat,
    prompt = "Prompt B",
    model = "model-b",
    cache_dir = cache_dir,
    cache_mode = "configuration",
    verbose = FALSE
  )
  out_b2 <- cl_chat_describe_images(
    chat,
    prompt = "Prompt B",
    model = "model-b",
    cache_dir = cache_dir,
    cache_mode = "configuration",
    verbose = FALSE
  )

  expect_equal(sum(grepl("^Prompt A", calls)), 2L)
  expect_equal(sum(calls == "Prompt B|CONFIG_ONE.jpg"), 1L)
  expect_equal(sum(calls == "Prompt B|CONFIG_TWO.jpg"), 2L)
  expect_true(all(grepl("Prompt A", out_a$image_description, fixed = TRUE)))
  expect_true(grepl("Prompt B", out_b1$image_description[1], fixed = TRUE))
  expect_true(grepl("Prompt A", out_b1$image_description[2], fixed = TRUE))
  expect_true(all(grepl("Prompt B", out_b2$image_description, fixed = TRUE)))

  store <- chatlens:::.clh_chat_store_dir("configured_images", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  index <- jsonlite::read_json(file.path(image_root, "image_manifest.json"), simplifyVector = FALSE)
  image_one <- index$images[[index$attachment_index[[keys[1]]]]]
  image_two <- index$images[[index$attachment_index[[keys[2]]]]]
  expect_equal(image_one$description_count, 2L)
  expect_equal(image_one$attempt_count, 2L)
  expect_equal(image_two$description_count, 2L)
  expect_equal(image_two$attempt_count, 3L)
  expect_equal(
    image_one$descriptions[[image_one$active_description_id]]$configuration_id,
    image_two$descriptions[[image_two$active_description_id]]$configuration_id
  )
})

test_that("identical image content shares one stable image record", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  paths <- file.path(cache_dir, c("DUPLICATE_A.jpg", "DUPLICATE_B.jpg"))
  writeLines("identical image content", paths[1])
  writeLines("identical image content", paths[2])
  keys <- paste0("path:", paths)
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:01:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = paste(basename(paths), "(file attached)"),
    message_id = c(1L, 2L),
    attachment = basename(paths),
    attachment_type = c("image", "image"),
    attachment_path = paths,
    attachment_key = keys,
    stringsAsFactors = FALSE
  )
  df$attachments <- as.list(df$attachment)
  df$attachment_types <- list("image", "image")
  df$attachment_paths <- as.list(paths)
  df$attachment_keys <- as.list(keys)
  df$attachment_statuses <- list("present", "present")
  chat <- chatlens:::.clh_new_chat(df, chat_key = "duplicate_images", zip_id = "zip1")

  calls <- 0L
  testthat::local_mocked_bindings(
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(...) {
      calls <<- calls + 1L
      "one shared description"
    },
    .package = "genflow"
  )

  out <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    cache_mode = "force",
    verbose = FALSE
  )
  out_cached <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    cache_mode = "missing",
    verbose = FALSE
  )
  store <- chatlens:::.clh_chat_store_dir("duplicate_images", cache_dir = cache_dir)
  index <- jsonlite::read_json(
    file.path(store, "image_descriptions", "image_manifest.json"),
    simplifyVector = FALSE
  )

  expect_equal(calls, 1L)
  expect_equal(length(index$images), 1L)
  expect_equal(index$attachment_index[[keys[1]]], index$attachment_index[[keys[2]]])
  expect_equal(index$images[[1]]$occurrence_count, 2L)
  expect_equal(sort(unlist(index$images[[1]]$original_names)), sort(basename(paths)))
  expect_equal(out$image_description, rep("one shared description", 2L))
  expect_equal(out_cached$image_description, out$image_description)
})

test_that("image descriptions without a manifest are recovered after interruption", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  img_path <- file.path(cache_dir, "INTERRUPTED.webp")
  writeLines("fake image bytes", img_path)
  key <- paste0("path:", img_path)
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "INTERRUPTED.webp (file attached)",
    message_id = 1L,
    attachment = "INTERRUPTED.webp",
    attachment_type = "image",
    attachment_path = img_path,
    attachment_key = key,
    stringsAsFactors = FALSE
  )
  df$attachments <- list("INTERRUPTED.webp")
  df$attachment_types <- list("image")
  df$attachment_paths <- list(img_path)
  df$attachment_keys <- list(key)
  df$attachment_statuses <- list("present")
  chat <- chatlens:::.clh_new_chat(df, chat_key = "interrupted_images", zip_id = "zip1")

  store <- chatlens:::.clh_chat_store_dir("interrupted_images", cache_dir = cache_dir)
  desc_dir <- file.path(store, "image_descriptions")
  dir.create(desc_dir, recursive = TRUE)
  writeLines("description saved before Ctrl+C", file.path(desc_dir, "INTERRUPTED.txt"))

  calls <- 0L
  testthat::local_mocked_bindings(
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(...) {
      calls <<- calls + 1L
      "should not be called"
    },
    .package = "genflow"
  )

  messages <- capture.output(
    out <- cl_chat_describe_images(
      chat,
      prompt = "A custom prompt",
      model = "a-different-model",
      cache_dir = cache_dir,
      verbose = TRUE,
      overwrite = FALSE
    ),
    type = "message"
  )

  expect_equal(calls, 0L)
  expect_equal(out$image_description, "description saved before Ctrl+C")
  expect_true(any(grepl("1 cached description will be reused; 0 images remain", messages, fixed = TRUE)))
  expect_true(any(grepl("Migrated 1 legacy image description", messages, fixed = TRUE)))

  image_root <- file.path(store, "image_descriptions")
  manifest <- jsonlite::read_json(file.path(image_root, "image_manifest.json"), simplifyVector = FALSE)
  image_id <- manifest$attachment_index[[key]]
  image <- manifest$images[[image_id]]
  metadata <- jsonlite::read_json(
    file.path(image_root, image$descriptions[[image$active_description_id]]$metadata_file),
    simplifyVector = FALSE
  )
  expect_equal(image$status, "processed")
  expect_equal(metadata$recovered_from, "legacy_flat_file")
  expect_true(file.exists(file.path(image_root, image_id, "image.json")))
})

test_that("image manifest is checkpointed before the next provider call", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  paths <- file.path(cache_dir, c("ONE.jpg", "TWO.jpg"))
  writeLines("first image", paths[1])
  writeLines("second image", paths[2])
  keys <- paste0("path:", paths)
  df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:01:00"), tz = "UTC"),
    sender = c("A", "B"),
    text = c("ONE.jpg (file attached)", "TWO.jpg (file attached)"),
    message_id = c(1L, 2L),
    attachment = c("ONE.jpg", "TWO.jpg"),
    attachment_type = c("image", "image"),
    attachment_path = paths,
    attachment_key = keys,
    stringsAsFactors = FALSE
  )
  df$attachments <- as.list(df$attachment)
  df$attachment_types <- list("image", "image")
  df$attachment_paths <- as.list(paths)
  df$attachment_keys <- as.list(keys)
  df$attachment_statuses <- list("present", "present")
  chat <- chatlens:::.clh_new_chat(df, chat_key = "checkpoint_images", zip_id = "zip1")

  store <- chatlens:::.clh_chat_store_dir("checkpoint_images", cache_dir = cache_dir)
  manifest_path <- file.path(store, "image_descriptions", "image_manifest.json")
  calls <- 0L
  checkpoint_seen <- FALSE
  testthat::local_mocked_bindings(
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(...) {
      calls <<- calls + 1L
      if (calls == 2L) {
        checkpoint <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
        first_image_id <- checkpoint$attachment_index[[keys[1]]]
        first_image <- checkpoint$images[[first_image_id]]
        checkpoint_seen <<- identical(first_image$status, "processed") &&
          identical(first_image$description_count, 1L)
      }
      paste0("description ", calls)
    },
    .package = "genflow"
  )

  out <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    verbose = FALSE,
    workers = 1,
    overwrite = TRUE
  )

  expect_equal(calls, 2L)
  expect_true(checkpoint_seen)
  expect_equal(out$image_description, c("description 1", "description 2"))
})

test_that("image API errors keep earlier descriptions and save only error metadata", {
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
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(prompt, add_img, ...) {
      calls <<- calls + 1L
      error_msgs[[calls]]
    },
    .package = "genflow"
  )

  out1 <- cl_chat_describe_images(chat, cache_dir = cache_dir, verbose = FALSE, overwrite = TRUE)
  out2 <- cl_chat_describe_images(chat, cache_dir = cache_dir, verbose = FALSE, overwrite = TRUE)

  image_root <- file.path(store, "image_descriptions")
  manifest_path <- file.path(image_root, "image_manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  key <- paste0("path:", img_path)
  image_id <- manifest$attachment_index[[key]]
  image <- manifest$images[[image_id]]
  description_dir <- file.path(image_root, image_id, "descriptions")
  metadata_files <- list.files(description_dir, pattern = "\\.json$", full.names = TRUE)
  metadata <- lapply(metadata_files, jsonlite::read_json, simplifyVector = FALSE)
  statuses <- vapply(metadata, function(x) x$status, FUN.VALUE = character(1))
  errors <- metadata[statuses == "error"]

  expect_equal(calls, 2L)
  expect_true(file.exists(desc_path))
  expect_equal(out1$image_description[1], "stale previous description")
  expect_equal(out2$image_description[1], "stale previous description")
  expect_equal(length(list.files(description_dir, pattern = "\\.txt$")), 1L)
  expect_equal(sum(statuses == "processed"), 1L)
  expect_equal(sum(statuses == "error"), 2L)
  expect_equal(image$description_count, 1L)
  expect_equal(image$attempt_count, 3L)
  expect_equal(errors[[2]]$error_message, "Bad Request: invalid image payload")
})

test_that("image global manifest is rebuilt from per-image metadata", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  img_path <- file.path(cache_dir, "REBUILD.jpg")
  writeLines("fake image bytes", img_path)
  key <- paste0("path:", img_path)
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "REBUILD.jpg (file attached)",
    message_id = 1L,
    attachment = "REBUILD.jpg",
    attachment_type = "image",
    attachment_path = img_path,
    attachment_key = key,
    stringsAsFactors = FALSE
  )
  df$attachments <- list("REBUILD.jpg")
  df$attachment_types <- list("image")
  df$attachment_paths <- list(img_path)
  df$attachment_keys <- list(key)
  df$attachment_statuses <- list("present")
  chat <- chatlens:::.clh_new_chat(df, chat_key = "rebuild_images", zip_id = "zip1")

  calls <- 0L
  testthat::local_mocked_bindings(
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(...) {
      calls <<- calls + 1L
      "durable description"
    },
    .package = "genflow"
  )

  out1 <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    verbose = FALSE,
    overwrite = TRUE
  )
  store <- chatlens:::.clh_chat_store_dir("rebuild_images", cache_dir = cache_dir)
  manifest_path <- file.path(store, "image_descriptions", "image_manifest.json")
  unlink(manifest_path)

  out2 <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    verbose = FALSE,
    overwrite = FALSE
  )
  rebuilt <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)

  expect_equal(calls, 1L)
  expect_equal(out1$image_description, "durable description")
  expect_equal(out2$image_description, "durable description")
  expect_true(file.exists(manifest_path))
  expect_equal(rebuilt$images[[rebuilt$attachment_index[[key]]]]$status, "processed")
})

test_that("unfinished image attempts are marked interrupted on resume", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  img_path <- file.path(cache_dir, "PENDING.jpg")
  writeLines("fake image bytes", img_path)
  key <- paste0("path:", img_path)
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "PENDING.jpg (file attached)",
    message_id = 1L,
    attachment = "PENDING.jpg",
    attachment_type = "image",
    attachment_path = img_path,
    attachment_key = key,
    stringsAsFactors = FALSE
  )
  df$attachments <- list("PENDING.jpg")
  df$attachment_types <- list("image")
  df$attachment_paths <- list(img_path)
  df$attachment_keys <- list(key)
  df$attachment_statuses <- list("present")
  chat <- chatlens:::.clh_new_chat(df, chat_key = "pending_images", zip_id = "zip1")

  calls <- 0L
  testthat::local_mocked_bindings(
    gen_batch_agent = .clh_test_gen_batch_agent,
    gen_txt = function(...) {
      calls <<- calls + 1L
      "existing description"
    },
    .package = "genflow"
  )
  cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    cache_mode = "force",
    verbose = FALSE
  )

  store <- chatlens:::.clh_chat_store_dir("pending_images", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  index <- chatlens:::.clh_image_index_load(image_root)
  image_id <- index$attachment_index[[key]]
  record <- chatlens:::.clh_image_record_load(image_root, image_id)
  configuration <- chatlens:::.clh_image_configuration(
    "unfinished prompt",
    "openrouter",
    "unfinished-model"
  )
  index <- chatlens:::.clh_image_index_register_configuration(index, configuration)
  pending <- chatlens:::.clh_image_attempt_metadata(
    record,
    configuration,
    image_root,
    attachment_key = key,
    attachment = "PENDING.jpg",
    zip_id = "zip1",
    status = "processing"
  )
  chatlens:::.clh_image_commit_attempt(record, pending, index, image_root)

  resumed <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    cache_mode = "missing",
    verbose = FALSE
  )
  pending_metadata <- jsonlite::read_json(
    file.path(image_root, pending$metadata_file),
    simplifyVector = FALSE
  )

  expect_equal(calls, 1L)
  expect_equal(resumed$image_description, "existing description")
  expect_equal(pending_metadata$status, "interrupted")
  expect_match(pending_metadata$error_message, "stopped before saving")
})
