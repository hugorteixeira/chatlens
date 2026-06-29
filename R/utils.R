# Utility helpers

.clh_cache_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir) || length(cache_dir) == 0L || is.na(cache_dir[1]) || !nzchar(cache_dir[1])) {
    cache_dir <- "~/.chatlens"
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1L) {
    stop("cache_dir must be NULL or a length-1 character string")
  }
  dir <- path.expand(cache_dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

.clh_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

.clh_zip_id <- function(zip_path) {
  if (is.null(zip_path)) return(NA_character_)
  zip_path <- path.expand(zip_path)
  if (!file.exists(zip_path)) return(NA_character_)
  unname(tools::md5sum(zip_path))
}

.clh_extract_dir <- function(zip_id, cache_dir = NULL) {
  if (is.na(zip_id) || is.null(zip_id)) return(NULL)
  cache_dir <- .clh_cache_dir(cache_dir)
  .clh_ensure_dir(file.path(cache_dir, "whatsapp", "extract", zip_id))
}

.clh_chat_key_from_file <- function(chat_file) {
  if (is.null(chat_file) || !nzchar(chat_file)) return("chat")
  base <- tolower(tools::file_path_sans_ext(basename(chat_file)))
  base <- gsub("[^a-z0-9]+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  if (!nzchar(base)) "chat" else base
}

.clh_chat_store_dir <- function(chat_key, cache_dir = NULL) {
  if (is.null(chat_key) || !nzchar(chat_key)) return(NULL)
  cache_dir <- .clh_cache_dir(cache_dir)
  .clh_ensure_dir(file.path(cache_dir, "whatsapp", "chats", chat_key))
}

.clh_rds_text_path <- function(path) {
  path <- path.expand(path)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    return(sub("\\.rds$", ".txt", path, ignore.case = TRUE))
  }
  paste0(path, ".txt")
}

.clh_has_media_annotations <- function(chat) {
  cols <- c("audio_transcripts", "audio_transcript", "image_descriptions", "image_description")
  any(cols %in% names(chat))
}

.clh_chat_text_snapshot <- function(chat, text_col = "text_enriched") {
  if (!identical(text_col, "text_enriched")) return(chat)
  if ("text_enriched" %in% names(chat)) return(chat)
  if (!.clh_has_media_annotations(chat)) return(chat)
  if (!exists("cl_chat_process_media", mode = "function")) return(chat)

  cl_chat_process_media(chat, save_chat = FALSE)
}

.clh_write_chat_text <- function(chat, path, text_col = "text_enriched") {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  path <- path.expand(path)
  .clh_ensure_dir(dirname(path))

  chat <- .clh_chat_text_snapshot(chat, text_col = text_col)
  if (!text_col %in% names(chat)) text_col <- "text"
  text <- paste(.clh_format_messages(chat, text_col = text_col), collapse = "\n")
  writeLines(text, path, useBytes = TRUE)
  invisible(path)
}

.clh_save_rds_with_text <- function(object, path, text_col = "text_enriched") {
  path <- path.expand(path)
  .clh_ensure_dir(dirname(path))

  saveRDS(object, path)
  txt_path <- NA_character_

  if (inherits(object, "chatlens_chat")) {
    object <- readRDS(path)
    txt_path <- .clh_rds_text_path(path)
    .clh_write_chat_text(object, txt_path, text_col = text_col)
  }

  invisible(list(rds = path, txt = txt_path))
}

.clh_chat_store_dir_from_chat <- function(chat, cache_dir = NULL) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  source <- .clh_or(attr(chat, "source"), list())
  if (!is.null(cache_dir)) {
    chat_key <- .clh_or(attr(chat, "chat_key"), source$chat_key, NULL)
    if (is.null(chat_key) || !nzchar(chat_key)) return(NULL)
    return(.clh_chat_store_dir(chat_key, cache_dir))
  }

  store_dir <- .clh_or(source$store_dir, NULL)
  if (!is.null(store_dir) && nzchar(store_dir)) {
    return(.clh_ensure_dir(store_dir))
  }

  chat_key <- .clh_or(attr(chat, "chat_key"), source$chat_key, NULL)
  if (is.null(chat_key) || !nzchar(chat_key)) return(NULL)

  .clh_chat_store_dir(chat_key, cache_dir)
}

.clh_chat_state_path <- function(chat, filename, cache_dir = NULL) {
  store_dir <- .clh_chat_store_dir_from_chat(chat, cache_dir = cache_dir)
  if (is.null(store_dir)) return(NULL)
  file.path(store_dir, filename)
}

.clh_save_chat_state <- function(chat,
                                     filename,
                                     cache_dir = NULL,
                                     text_col = "text_enriched",
                                     overwrite = TRUE) {
  path <- .clh_chat_state_path(chat, filename = filename, cache_dir = cache_dir)
  if (is.null(path)) {
    return(invisible(list(rds = NA_character_, txt = NA_character_, skipped = TRUE)))
  }

  txt_path <- .clh_rds_text_path(path)
  if (!overwrite && file.exists(path)) {
    if (!file.exists(txt_path)) {
      existing <- readRDS(path)
      if (inherits(existing, "chatlens_chat")) {
        .clh_write_chat_text(existing, txt_path, text_col = text_col)
      }
    }
    return(invisible(list(rds = path, txt = txt_path, skipped = TRUE)))
  }

  .clh_save_rds_with_text(chat, path, text_col = text_col)
}

.clh_save_original_chat <- function(chat, cache_dir = NULL, overwrite = FALSE) {
  .clh_save_chat_state(
    chat = chat,
    filename = "chat_original.rds",
    cache_dir = cache_dir,
    overwrite = overwrite
  )
}

.clh_save_current_chat <- function(chat, cache_dir = NULL) {
  .clh_save_chat_state(
    chat = chat,
    filename = "chat.rds",
    cache_dir = cache_dir,
    overwrite = TRUE
  )
  chat
}

.clh_read_lines <- function(path, encoding = c("UTF-8", "latin1")) {
  path <- path.expand(path)
  last_err <- NULL
  for (enc in encoding) {
    out <- tryCatch(
      readLines(path, encoding = enc, warn = FALSE, skipNul = TRUE),
      error = function(e) {
        last_err <<- e
        NULL
      }
    )
    if (!is.null(out)) return(out)
  }
  if (!is.null(last_err)) stop(last_err$message, call. = FALSE)
  character(0)
}

.clh_escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

.clh_path_slug <- function(x, fallback = "value") {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(fallback)
  x <- tolower(as.character(x[1]))
  x <- gsub("[^a-z0-9_-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) fallback else x
}

.clh_parse_datetime <- function(x, tz = "UTC", date_order = "dmy") {
  if (is.na(x) || !nzchar(x)) return(as.POSIXct(NA))
  x <- trimws(x)
  # Try multiple common WhatsApp formats
  if (date_order == "dmy") {
    fmts <- c(
      "%d/%m/%Y %H:%M:%S",
      "%d/%m/%Y %H:%M",
      "%d/%m/%Y, %H:%M:%S",
      "%d/%m/%Y, %H:%M",
      "%d/%m/%y %H:%M",
      "%d/%m/%y, %H:%M",
      "%d/%m/%Y %I:%M %p",
      "%d/%m/%Y, %I:%M %p",
      "%d/%m/%y %I:%M %p",
      "%d/%m/%y, %I:%M %p"
    )
  } else {
    fmts <- c(
      "%m/%d/%Y %H:%M:%S",
      "%m/%d/%Y %H:%M",
      "%m/%d/%Y, %H:%M:%S",
      "%m/%d/%Y, %H:%M",
      "%m/%d/%y %H:%M",
      "%m/%d/%y, %H:%M",
      "%m/%d/%Y %I:%M %p",
      "%m/%d/%Y, %I:%M %p",
      "%m/%d/%y %I:%M %p",
      "%m/%d/%y, %I:%M %p"
    )
  }

  for (fmt in fmts) {
    parsed <- suppressWarnings(as.POSIXct(strptime(x, fmt, tz = tz)))
    if (!is.na(parsed)) return(parsed)
  }
  as.POSIXct(NA)
}

.clh_or <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0) return(value)
  }
  NULL
}

.clh_coerce_text <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) {
    if (!is.null(x$response_value)) return(.clh_coerce_text(x$response_value))
    if (!is.null(x$text)) return(.clh_coerce_text(x$text))
    return(NA_character_)
  }
  if (length(x) == 0) return(NA_character_)
  if (length(x) > 1) return(paste(x, collapse = "\n"))
  if (is.na(x)) return(NA_character_)
  as.character(x)
}

.clh_is_error_response <- function(raw, text) {
  if (is.list(raw)) {
    if (!is.null(raw$status_api) && !identical(raw$status_api, "SUCCESS")) return(TRUE)
    if (!is.null(raw$status_msg) && grepl("error", raw$status_msg, ignore.case = TRUE)) return(TRUE)
  }
  text <- as.character(text)
  if (length(text) == 0 || is.na(text[1])) return(TRUE)
  text <- trimws(text[1])
  if (!nzchar(text)) return(TRUE)

  # Common provider failure payloads that should never be treated as valid content.
  error_patterns <- c(
    "\\bAPI_ERROR\\b",
    "\\bBad\\s+Request\\b",
    "OpenRouter API error",
    "not a valid model ID"
  )
  if (any(grepl(paste(error_patterns, collapse = "|"), text, ignore.case = TRUE, perl = TRUE))) return(TRUE)
  FALSE
}

.clh_compact_error_message <- function(x, max_chars = 500) {
  if (is.null(x)) return(NA_character_)
  x <- .clh_coerce_text(x)
  if (length(x) == 0 || is.na(x[1])) return(NA_character_)
  x <- gsub("[\r\n\t]+", " ", x[1])
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) return(NA_character_)
  if (nchar(x) > max_chars) {
    x <- paste0(substr(x, 1, max_chars - 3), "...")
  }
  x
}

.clh_error_response_message <- function(raw = NULL,
                                            text = NULL,
                                            fallback = "unknown provider error") {
  candidates <- character(0)

  if (inherits(raw, "condition")) {
    candidates <- c(candidates, conditionMessage(raw))
  } else if (is.list(raw)) {
    fields <- c("status_msg", "error_message", "message", "error", "detail", "response_value", "text")
    for (field in fields) {
      if (!is.null(raw[[field]])) {
        candidates <- c(candidates, .clh_error_response_message(raw[[field]], fallback = ""))
      }
    }
    if (!is.null(raw$response) && !identical(raw$response, raw)) {
      candidates <- c(candidates, .clh_error_response_message(raw$response, fallback = ""))
    }
    if (!is.null(raw$status_api) && !identical(raw$status_api, "SUCCESS")) {
      candidates <- c(candidates, paste("status_api:", raw$status_api))
    }
  } else {
    candidates <- c(candidates, .clh_compact_error_message(raw))
  }

  if (!is.null(text)) {
    candidates <- c(candidates, .clh_compact_error_message(text))
  }

  candidates <- unique(stats::na.omit(candidates))
  candidates <- candidates[nzchar(candidates)]
  if (length(candidates) == 0) return(fallback)
  .clh_compact_error_message(candidates[1])
}

.clh_attachment_type_from_name <- function(name) {
  if (is.na(name) || !nzchar(name)) return(NA_character_)
  ext <- tolower(sub(".*\\.", "", name))
  audio_ext <- c("opus", "mp3", "m4a", "wav", "ogg", "aac", "flac")
  image_ext <- c("jpg", "jpeg", "png", "gif", "webp", "heic", "bmp", "tiff")
  video_ext <- c("mp4", "mov", "mkv", "avi", "3gp", "webm")
  doc_ext <- c("pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "txt", "zip", "rar", "7z", "csv", "vcf")
  if (ext %in% audio_ext) return("audio")
  if (ext %in% image_ext) return("image")
  if (ext %in% video_ext) return("video")
  if (ext %in% doc_ext) return("file")
  "file"
}

.clh_attachment_key <- function(name, path = NA_character_, zip_id = NULL) {
  if (!is.na(path) && nzchar(path)) {
    key_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (!is.null(zip_id) && !is.na(zip_id) && nzchar(zip_id)) {
      pos <- regexpr(zip_id, key_path, fixed = TRUE)
      if (pos[1] > 0) {
        key_path <- substr(key_path, pos[1], nchar(key_path))
      }
    }
    key_path <- gsub("/+", "/", key_path)
    return(paste0("path:", key_path))
  }

  if (!is.na(name) && nzchar(name)) {
    return(paste0("name:", tolower(trimws(name))))
  }

  "unknown"
}
