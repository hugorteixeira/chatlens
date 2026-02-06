test_that("attachment detection supports spaces and parentheses", {
  lines <- c(
    "Joao: IMG-20250101-WA0001.jpg (arquivo anexado)",
    "Maria: IMG-20250101-WA0001 (1).jpg (arquivo anexado)",
    "Ana: DOC-2025-01-01 (final).pdf (file attached)"
  )

  out <- chatlens:::whatsapp_detect_attachments(lines)
  expect_equal(out$filenames[[1]], "IMG-20250101-WA0001.jpg")
  expect_equal(out$filenames[[2]], "IMG-20250101-WA0001 (1).jpg")
  expect_equal(out$filenames[[3]], "DOC-2025-01-01 (final).pdf")
})

test_that("media omitted messages are represented as omitted attachments", {
  df <- chatlens:::whatsapp_parse_lines("01/01/2025, 10:00 - Alice: <Media omitted>")
  chat <- chatlens:::new_chatlens_chat(df)
  att <- cl_attachments(chat)

  expect_equal(df$message_type, "omitted")
  expect_equal(nrow(att), 1)
  expect_true(is.na(att$attachment[1]))
  expect_equal(att$attachment_status[1], "omitted")
})

test_that("duplicate basenames receive distinct resolved keys", {
  td <- tempfile("chatlens_attachments_")
  dir.create(td)
  dir.create(file.path(td, "a"))
  dir.create(file.path(td, "b"))
  writeLines("a", file.path(td, "a", "IMG-1.jpg"))
  writeLines("b", file.path(td, "b", "IMG-1.jpg"))
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    sender = "A",
    text = "IMG-1.jpg (file attached)",
    message_id = 1L,
    text_raw = "IMG-1.jpg (file attached)",
    attachment = NA_character_,
    attachment_type = NA_character_,
    attachment_placeholder = TRUE,
    attachment_omitted = FALSE,
    message_type = "image",
    stringsAsFactors = FALSE
  )
  df$attachments <- list(c("IMG-1.jpg", "IMG-1.jpg"))
  df$attachment_types <- list(c("image", "image"))

  resolved <- chatlens:::whatsapp_resolve_attachments(df, media_dir = td, zip_id = "zip1")
  expect_length(unique(resolved$attachment_paths[[1]]), 2)
  expect_length(unique(resolved$attachment_keys[[1]]), 2)
})

test_that("whatsapp_parse_lines omits missing/NA/NULL senders by default", {
  lines <- c(
    "01/01/2025, 10:00 - Messages and calls are end-to-end encrypted.",
    "01/01/2025, 10:01 - NA: test",
    "01/01/2025, 10:02 - NULL: test",
    "01/01/2025, 10:03 - Alice: hello"
  )

  out <- chatlens:::whatsapp_parse_lines(lines, tz = "UTC", date_order = "dmy")
  expect_equal(nrow(out), 1)
  expect_equal(out$sender, "Alice")
})

test_that("whatsapp_parse_lines can keep missing sender rows", {
  lines <- c(
    "01/01/2025, 10:00 - Messages and calls are end-to-end encrypted.",
    "01/01/2025, 10:01 - Alice: hello"
  )

  out <- chatlens:::whatsapp_parse_lines(lines, tz = "UTC", date_order = "dmy", omit_sender_na = FALSE)
  expect_equal(nrow(out), 2)
  expect_true(any(is.na(out$sender)))
})

test_that("cl_attachments supports placeholder rows without message_id", {
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "file attached",
    attachment_placeholder = TRUE,
    attachment_omitted = FALSE,
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::new_chatlens_chat(df)

  att <- cl_attachments(chat)
  expect_equal(nrow(att), 1)
  expect_equal(att$message_id, 1)
  expect_equal(att$attachment_status, "placeholder")
})
