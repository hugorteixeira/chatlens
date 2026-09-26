test_that("cl_whatsapp_import reports timing and marks cached extraction", {
  td <- tempfile("chatlens_import_timing_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  chat_file <- file.path(td, "WhatsApp Chat.txt")
  writeLines(
    c(
      "01/01/2025, 10:00 - Alice: hello",
      "01/01/2025, 10:01 - Bob: Contato.vcf (arquivo anexado)",
      "01/01/2025, 10:02 - Alice: <Mídia oculta>",
      "Missing-audio.m4a"
    ),
    chat_file
  )
  writeLines("BEGIN:VCARD", file.path(td, "Contato.vcf"))

  zip_path <- file.path(td, "chat.zip")
  old_wd <- setwd(td)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zipfile = zip_path, files = c(basename(chat_file), "Contato.vcf"), flags = "-q")
  setwd(old_wd)

  cache_dir <- file.path(td, "cache")
  output <- capture.output(
    chat <- cl_whatsapp_import(zip_path, cache_dir = cache_dir, verbose = TRUE),
    type = "message"
  )

  expect_true(any(grepl("Import completed in", output, fixed = TRUE)))
  expect_true(any(grepl("message parsing (1 worker):", output, fixed = TRUE)))
  expect_true(any(grepl(
    "Attachment check: 2 attachment references (1 present, 1 missing).",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("attachment resolution:", output, fixed = TRUE)))
  expect_true(any(grepl("finalize + cache save:", output, fixed = TRUE)))
  expect_s3_class(chat, "chatlens_chat")

  cached_workers <- if (.Platform$OS.type == "windows") 1L else 2L
  cached_output <- capture.output(
    cached_chat <- cl_whatsapp_import(
      zip_path,
      cache_dir = cache_dir,
      verbose = TRUE,
      workers = cached_workers
    ),
    type = "message"
  )
  expect_true(any(grepl("extraction (cached):", cached_output, fixed = TRUE)))
  expected_worker_label <- paste0(
    "message parsing (",
    cached_workers,
    if (cached_workers == 1L) " worker):" else " workers):"
  )
  expect_true(any(grepl(expected_worker_label, cached_output, fixed = TRUE)))
  expect_s3_class(cached_chat, "chatlens_chat")
  cached_rds <- file.path(cache_dir, "whatsapp", "chats", "whatsapp_chat", "chat.rds")
  expect_equal(cached_chat, readRDS(cached_rds))
})
