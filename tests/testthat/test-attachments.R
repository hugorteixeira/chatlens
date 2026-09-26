test_that("attachment detection supports spaces and parentheses", {
  lines <- c(
    "Joao: IMG-20250101-WA0001.jpg (arquivo anexado)",
    "Maria: IMG-20250101-WA0001 (1).jpg (arquivo anexado)",
    "Ana: DOC-2025-01-01 (final).pdf (file attached)"
  )

  out <- chatlens:::.clh_whatsapp_detect_attachments(lines)
  expect_equal(out$filenames[[1]], "IMG-20250101-WA0001.jpg")
  expect_equal(out$filenames[[2]], "IMG-20250101-WA0001 (1).jpg")
  expect_equal(out$filenames[[3]], "DOC-2025-01-01 (final).pdf")
})

test_that("attachment detection supports Unicode names and contact cards", {
  lines <- c(
    "\u200eAndré.vcf (arquivo anexado)",
    "\u200eTainá [V8 Propaganda].vcf (arquivo anexado)",
    "\u200e.vcf (arquivo anexado)",
    "\u200e[EMBAIXADOR] Tiago Digital.vcf (arquivo anexado)",
    "\u200eComo aumentar R$2.500.000 com Inteligência Artificial.\u00a0.pdf (arquivo anexado)"
  )

  out <- chatlens:::.clh_whatsapp_detect_attachments(lines)

  expect_equal(out$filenames[[1]], "André.vcf")
  expect_equal(out$filenames[[2]], "Tainá [V8 Propaganda].vcf")
  expect_equal(out$filenames[[3]], ".vcf")
  expect_equal(out$filenames[[4]], "[EMBAIXADOR] Tiago Digital.vcf")
  expect_equal(
    out$filenames[[5]],
    "Como aumentar R$2.500.000 com Inteligência Artificial.\u00a0.pdf"
  )
  expect_equal(lapply(out$types[1:4], unname), rep(list("contact"), 4))
  expect_equal(unname(out$types[[5]]), "file")
  expect_true(all(out$placeholder))
  expect_equal(chatlens:::.clh_attachment_type_from_name("contact.vcf"), "contact")
})

test_that("ordinary mentions of attached files are not placeholders", {
  lines <- c(
    "Notificação extra judicial com B.O anexado, funciona",
    "I followed the notice with the report attached.",
    "file attached"
  )

  out <- chatlens:::.clh_whatsapp_detect_attachments(lines)

  expect_equal(lengths(out$filenames), c(0L, 0L, 0L))
  expect_equal(out$placeholder, c(FALSE, FALSE, TRUE))
})

test_that("URLs and email addresses are not mistaken for attachments", {
  lines <- c(
    "https://www.opus.pro/pt-br",
    "Veja https://files.example.com/PTT-20250101-WA0001.opus",
    "www.opus.pro/pt-br",
    "Envie para audio.mp3@example.com",
    "<Mídia oculta>\nUma_equipe_de_IAs_como_seu_Life_OS.m4a",
    "www.opus (arquivo anexado)",
    "<Mídia oculta>\n01. Dotcom Secrets - www.fernandobrasao.com - Livros da Gringa.pdf"
  )

  out <- chatlens:::.clh_whatsapp_detect_attachments(lines)

  expect_equal(lengths(out$filenames), c(0L, 0L, 0L, 0L, 1L, 1L, 1L))
  expect_equal(out$filenames[[5]], "Uma_equipe_de_IAs_como_seu_Life_OS.m4a")
  expect_equal(unname(out$types[[5]]), "audio")
  expect_equal(out$filenames[[6]], "www.opus")
  expect_true(out$placeholder[6])
  expect_equal(
    out$filenames[[7]],
    "01. Dotcom Secrets - www.fernandobrasao.com - Livros da Gringa.pdf"
  )
})

test_that("attachment resolution ignores WhatsApp directionality marks", {
  td <- tempfile("chatlens_unicode_attachment_")
  dir.create(td)
  marked_path <- file.path(td, "\u200eContato.vcf")
  writeLines("BEGIN:VCARD", marked_path)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "Contato.vcf (arquivo anexado)",
    message_id = 1L,
    text_raw = "Contato.vcf (arquivo anexado)",
    attachment = "Contato.vcf",
    attachment_type = "contact",
    attachment_placeholder = TRUE,
    attachment_omitted = FALSE,
    message_type = "contact",
    stringsAsFactors = FALSE
  )
  df$attachments <- list("Contato.vcf")
  df$attachment_types <- list("contact")

  resolved <- chatlens:::.clh_whatsapp_resolve_attachments(df, media_dir = td, zip_id = "zip1")

  expect_equal(resolved$attachment_path, marked_path)
  expect_equal(resolved$attachment_status, "present")
})

test_that("media omitted messages are represented as omitted attachments", {
  df <- chatlens:::.clh_whatsapp_parse_lines("01/01/2025, 10:00 - Alice: <Media omitted>")
  chat <- chatlens:::.clh_new_chat(df)
  att <- chatlens:::.clh_attachments(chat)

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

  resolved <- chatlens:::.clh_whatsapp_resolve_attachments(df, media_dir = td, zip_id = "zip1")
  expect_length(unique(resolved$attachment_paths[[1]]), 2)
  expect_length(unique(resolved$attachment_keys[[1]]), 2)
})

test_that(".clh_whatsapp_parse_lines omits missing/NA/NULL senders by default", {
  lines <- c(
    "01/01/2025, 10:00 - Messages and calls are end-to-end encrypted.",
    "01/01/2025, 10:01 - NA: test",
    "01/01/2025, 10:02 - NULL: test",
    "01/01/2025, 10:03 - Alice: hello"
  )

  out <- chatlens:::.clh_whatsapp_parse_lines(lines, tz = "UTC", date_order = "dmy")
  expect_equal(nrow(out), 1)
  expect_equal(out$sender, "Alice")
})

test_that(".clh_whatsapp_parse_lines can keep missing sender rows", {
  lines <- c(
    "01/01/2025, 10:00 - Messages and calls are end-to-end encrypted.",
    "01/01/2025, 10:01 - Alice: hello"
  )

  out <- chatlens:::.clh_whatsapp_parse_lines(lines, tz = "UTC", date_order = "dmy", omit_sender_na = FALSE)
  expect_equal(nrow(out), 2)
  expect_true(any(is.na(out$sender)))
})

test_that(".clh_attachments supports placeholder rows without message_id", {
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"),
    sender = "A",
    text = "file attached",
    attachment_placeholder = TRUE,
    attachment_omitted = FALSE,
    stringsAsFactors = FALSE
  )
  chat <- chatlens:::.clh_new_chat(df)

  att <- chatlens:::.clh_attachments(chat)
  expect_equal(nrow(att), 1)
  expect_equal(att$message_id, 1)
  expect_equal(att$attachment_status, "placeholder")
})
