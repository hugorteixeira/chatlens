# Utility helpers

#' Get the cache directory used by chatlens
#' @export
cl_cache_dir <- function() {
  dir <- path.expand("~/.chatlens")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

chatlens_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

chatlens_zip_id <- function(zip_path) {
  if (is.null(zip_path)) return(NA_character_)
  zip_path <- path.expand(zip_path)
  if (!file.exists(zip_path)) return(NA_character_)
  unname(tools::md5sum(zip_path))
}

chatlens_extract_dir <- function(zip_id, cache_dir = cl_cache_dir()) {
  if (is.na(zip_id) || is.null(zip_id)) return(NULL)
  chatlens_ensure_dir(file.path(cache_dir, "whatsapp", "extract", zip_id))
}

chatlens_chat_key_from_file <- function(chat_file) {
  if (is.null(chat_file) || !nzchar(chat_file)) return("chat")
  base <- tolower(tools::file_path_sans_ext(basename(chat_file)))
  base <- gsub("[^a-z0-9]+", "_", base)
  base <- gsub("^_+|_+$", "", base)
  if (!nzchar(base)) "chat" else base
}

chatlens_chat_store_dir <- function(chat_key, cache_dir = cl_cache_dir()) {
  if (is.null(chat_key) || !nzchar(chat_key)) return(NULL)
  chatlens_ensure_dir(file.path(cache_dir, "whatsapp", "chats", chat_key))
}

chatlens_read_lines <- function(path, encoding = c("UTF-8", "latin1")) {
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

chatlens_escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

chatlens_parse_datetime <- function(x, tz = "UTC", date_order = "dmy") {
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

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

chatlens_coerce_text <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) {
    if (!is.null(x$response_value)) return(chatlens_coerce_text(x$response_value))
    if (!is.null(x$text)) return(chatlens_coerce_text(x$text))
    return(NA_character_)
  }
  if (length(x) == 0) return(NA_character_)
  if (length(x) > 1) return(paste(x, collapse = "\n"))
  if (is.na(x)) return(NA_character_)
  as.character(x)
}

chatlens_is_error_response <- function(raw, text) {
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

chatlens_attachment_type_from_name <- function(name) {
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

chatlens_attachment_key <- function(name, path = NA_character_, zip_id = NULL) {
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
