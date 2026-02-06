# WhatsApp Android import and parsing

#' Import a WhatsApp Android ZIP export
#' @param path Path to the WhatsApp export `.zip`
#' @param cache_dir Cache directory for extracted files
#' @param chat_key Optional chat key used for cache storage
#' @param ask_confirmation Ask to confirm detected chat file
#' @param encoding Character encoding to try when reading the chat text
#' @param verbose Whether to emit progress messages
#' @param tz Timezone for parsed timestamps
#' @param date_order Date order, "dmy" (default) or "mdy"
#' @param omit_sender_na Logical; when `TRUE` (default), drops parsed rows
#'   whose sender is missing, empty, `"NA"`, or `"NULL"`.
#' @export
cl_whatsapp_import <- function(path,
                                cache_dir = cl_cache_dir(),
                                chat_key = NULL,
                                ask_confirmation = FALSE,
                                encoding = c("UTF-8", "latin1"),
                                verbose = TRUE,
                                tz = "UTC",
                                date_order = c("dmy", "mdy"),
                                omit_sender_na = TRUE) {
  path <- path.expand(path)
  if (!file.exists(path)) stop("ZIP file not found: ", path)

  date_order <- match.arg(date_order)
  if (!is.logical(omit_sender_na) || length(omit_sender_na) != 1L || is.na(omit_sender_na)) {
    stop("omit_sender_na must be TRUE or FALSE")
  }
  zip_id <- chatlens_zip_id(path)
  extract_dir <- chatlens_extract_dir(zip_id, cache_dir)
  marker <- file.path(extract_dir, ".extracted")

  if (!dir.exists(extract_dir) || !file.exists(marker)) {
    chatlens_ensure_dir(extract_dir)
    if (verbose) message("Extracting ZIP to cache: ", extract_dir)
    utils::unzip(path, exdir = extract_dir)
    file.create(marker)
  } else if (verbose) {
    message("Using cached extraction: ", extract_dir)
  }

  files <- list.files(extract_dir, recursive = TRUE, full.names = TRUE)
  chat_file <- whatsapp_detect_chat_file(files, ask_confirmation = ask_confirmation, encoding = encoding)
  if (verbose) message("Chat file: ", basename(chat_file))
  if (is.null(chat_key) || !nzchar(chat_key)) chat_key <- chatlens_chat_key_from_file(chat_file)
  store_dir <- chatlens_chat_store_dir(chat_key, cache_dir)

  lines <- chatlens_read_lines(chat_file, encoding = encoding)
  chat <- whatsapp_parse_lines(lines, tz = tz, date_order = date_order, omit_sender_na = omit_sender_na)
  chat <- whatsapp_resolve_attachments(chat, media_dir = extract_dir, zip_id = zip_id)

  participants <- cl_detect_participants(chat)
  source <- list(
    path = path,
    extract_dir = extract_dir,
    store_dir = store_dir,
    chat_file = chat_file,
    date_order = date_order,
    tz = tz,
    chat_key = chat_key,
    zip_id = zip_id
  )

  new_chatlens_chat(chat, source = source, participants = participants, chat_key = chat_key, zip_id = zip_id)
}

whatsapp_detect_chat_file <- function(files, ask_confirmation = FALSE, encoding = c("UTF-8", "latin1")) {
  txt_files <- files[grepl("\\.txt$", files, ignore.case = TRUE)]
  if (length(txt_files) == 0) stop("No .txt files found in the ZIP export.")

  base_names <- basename(txt_files)
  priority <- grepl("whatsapp|conversa|chat", base_names, ignore.case = TRUE)

  scores <- vapply(seq_along(txt_files), function(i) {
    f <- txt_files[i]
    lines <- tryCatch(chatlens_read_lines(f, encoding = encoding), error = function(e) character(0))
    # Only look at the first 200 lines for speed
    if (length(lines) > 200) lines <- lines[1:200]
    sum(whatsapp_is_message_line(lines)) + if (priority[i]) 1000 else 0
  }, FUN.VALUE = numeric(1))

  best_idx <- which.max(scores)
  candidate <- txt_files[best_idx]

  if (ask_confirmation && interactive()) {
    message("Detected chat file: ", basename(candidate))
    ans <- readline("Use this file? [Y/n]: ")
    if (nzchar(ans) && tolower(substr(ans, 1, 1)) == "n") {
      message("Available .txt files:")
      for (i in seq_along(txt_files)) {
        message(sprintf("  %d) %s", i, basename(txt_files[i])))
      }
      idx <- suppressWarnings(as.integer(readline("Choose file number: ")))
      if (!is.na(idx) && idx >= 1 && idx <= length(txt_files)) {
        candidate <- txt_files[idx]
      }
    }
  }
  candidate
}

whatsapp_is_message_line <- function(line) {
  pattern <- "^[\\s\\u200e\\ufeff]*\\d{1,2}/\\d{1,2}/\\d{2,4}[, ]+\\d{1,2}:\\d{2}(?::\\d{2})?(?:\\s?[APMapm]{2})?\\s+-\\s+"
  grepl(pattern, line)
}

whatsapp_parse_lines <- function(lines, tz = "UTC", date_order = "dmy", omit_sender_na = TRUE) {
  if (length(lines) == 0) return(data.frame())
  if (!is.logical(omit_sender_na) || length(omit_sender_na) != 1L || is.na(omit_sender_na)) {
    stop("omit_sender_na must be TRUE or FALSE")
  }

  starts <- which(whatsapp_is_message_line(lines))
  if (length(starts) == 0) stop("No WhatsApp message lines detected.")
  ends <- c(starts[-1] - 1, length(lines))

  n <- length(starts)
  out <- vector("list", n)

  for (i in seq_len(n)) {
    raw_lines <- lines[starts[i]:ends[i]]
    header <- raw_lines[1]

    sep <- regexpr(" - ", header, fixed = TRUE)
    if (sep[1] == -1) {
      out[[i]] <- NULL
      next
    }

    date_time <- substr(header, 1, sep[1] - 1)
    rest <- substr(header, sep[1] + 3, nchar(header))

    ts <- chatlens_parse_datetime(date_time, tz = tz, date_order = date_order)

    sender <- NA_character_
    text <- rest
    colon <- regexpr(": ", rest, fixed = TRUE)
    if (colon[1] != -1) {
      sender <- substr(rest, 1, colon[1] - 1)
      text <- substr(rest, colon[1] + 2, nchar(rest))
    }

    if (isTRUE(omit_sender_na)) {
      sender_trim <- if (is.na(sender)) "" else trimws(sender)
      sender_norm <- tolower(sender_trim)
      if (!nzchar(sender_trim) || sender_norm %in% c("na", "null")) {
        out[[i]] <- NULL
        next
      }
    }

    if (length(raw_lines) > 1) {
      text <- paste(c(text, raw_lines[-1]), collapse = "\n")
    }

    out[[i]] <- list(timestamp = ts, sender = sender, text = text)
  }

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(data.frame())
  df <- do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
  if (nrow(df) == 0) return(df)

  df$message_id <- seq_len(nrow(df))
  df$text_raw <- df$text

  attachment_info <- whatsapp_detect_attachments(df$text)
  df$attachments <- attachment_info$filenames
  df$attachment_types <- attachment_info$types
  df$attachment_placeholder <- attachment_info$placeholder
  df$attachment_omitted <- attachment_info$omitted
  df$attachment <- vapply(df$attachments, function(x) if (length(x)) x[1] else NA_character_, FUN.VALUE = character(1))
  df$attachment_type <- vapply(df$attachment_types, function(x) if (length(x)) x[1] else NA_character_, FUN.VALUE = character(1))

  df$message_type <- vapply(seq_len(nrow(df)), function(i) {
    if (is.na(df$sender[i])) return("system")
    types <- df$attachment_types[[i]]
    if (length(types) == 0) {
      if (isTRUE(df$attachment_omitted[i])) return("omitted")
      if (isTRUE(df$attachment_placeholder[i])) return("attachment")
      return("text")
    }
    if (length(unique(types)) == 1) return(types[1])
    "mixed"
  }, FUN.VALUE = character(1))

  # Normalize system messages to English
  if (any(df$message_type == "system", na.rm = TRUE)) {
    idx <- which(df$message_type == "system")
    df$text[idx] <- vapply(df$text[idx], whatsapp_normalize_system_text, FUN.VALUE = character(1))
  }

  df
}

whatsapp_detect_attachments <- function(text) {
  audio_ext <- c("opus", "mp3", "m4a", "wav", "ogg", "aac", "flac")
  image_ext <- c("jpg", "jpeg", "png", "gif", "webp", "heic", "bmp", "tiff")
  video_ext <- c("mp4", "mov", "mkv", "avi", "3gp", "webm")
  doc_ext <- c("pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "txt", "zip", "rar", "7z", "csv", "vcf")

  exts <- c(audio_ext, image_ext, video_ext, doc_ext)
  file_pattern <- paste0(
    "(?<![[:alnum:]])",
    "[[:alnum:]_][[:alnum:]_ .()'\\-]{0,160}\\.(?:",
    paste(exts, collapse = "|"),
    ")\\b"
  )

  matches <- regmatches(text, gregexpr(file_pattern, text, ignore.case = TRUE, perl = TRUE))
  filenames <- lapply(matches, function(x) {
    if (length(x) == 0) return(character(0))
    x <- trimws(x)
    x <- gsub("^[\"'`<\\[(]+", "", x)
    x <- gsub("[\"'`>\\])]+$", "", x)
    x <- gsub("\\s+", " ", x)
    x <- x[nzchar(x)]
    # Common attachment placeholders are not part of the filename.
    x <- gsub("^(arquivo|file|attached|anexado|anexo|media|midia)\\s+", "", x, ignore.case = TRUE)
    if (length(x) <= 1) return(x)
    x[!duplicated(tolower(x))]
  })

  placeholder_pattern <- "arquivo anexado|file attached|attached|anexado"
  omitted_pattern <- "m(?:i|\\x{00ed})dia omitida|media omitted|<media omitted>"

  placeholder <- grepl(placeholder_pattern, text, ignore.case = TRUE)
  omitted <- grepl(omitted_pattern, text, ignore.case = TRUE, perl = TRUE)

  classify <- function(name) {
    ext <- tolower(sub(".*\\.", "", name))
    if (ext %in% audio_ext) return("audio")
    if (ext %in% image_ext) return("image")
    if (ext %in% video_ext) return("video")
    if (ext %in% doc_ext) return("file")
    "file"
  }

  types <- lapply(filenames, function(x) {
    if (length(x) == 0) return(character(0))
    vapply(x, classify, FUN.VALUE = character(1))
  })

  list(filenames = filenames, placeholder = placeholder, omitted = omitted, types = types)
}

whatsapp_normalize_system_text <- function(text) {
  if (is.na(text) || !nzchar(text)) return(text)

  # Encryption notices
  text <- gsub(
    "As mensagens e chamadas[^\\n]*criptografia de ponta a ponta\\.?$",
    "Messages and calls are end-to-end encrypted.",
    text, ignore.case = TRUE
  )
  text <- gsub(
    "Messages and calls are end-to-end encrypted\\.[^\\n]*$",
    "Messages and calls are end-to-end encrypted.",
    text, ignore.case = TRUE
  )

  # Group creation
  text <- gsub(
    "^(.+) criou o grupo \\\"(.*)\\\"$",
    "\\1 created the group \"\\2\"",
    text
  )
  text <- gsub(
    "^You created group \\\"(.*)\\\"$",
    "You created the group \"\\1\"",
    text
  )

  # Subject changes
  text <- gsub(
    "^(.+) mudou o assunto de \\\"(.*)\\\" para \\\"(.*)\\\"$",
    "\\1 changed the subject from \"\\2\" to \"\\3\"",
    text
  )
  text <- gsub(
    "^(.+) changed the subject from \\\"(.*)\\\" to \\\"(.*)\\\"$",
    "\\1 changed the subject from \"\\2\" to \"\\3\"",
    text
  )

  # Description changes
  text <- gsub(
    "^(.+) mudou a descri(?:c|\\x{00e7})[a\\x{00e3}]o do grupo$",
    "\\1 changed the group description",
    text,
    perl = TRUE
  )
  text <- gsub(
    "^(.+) changed the group description$",
    "\\1 changed the group description",
    text
  )

  # Group image changes
  text <- gsub(
    "^(.+) mudou a imagem do grupo$",
    "\\1 changed the group image",
    text
  )
  text <- gsub(
    "^(.+) changed the group image$",
    "\\1 changed the group image",
    text
  )

  # Added / removed
  text <- gsub(
    "^(.+) adicionou (.+)$",
    "\\1 added \\2",
    text
  )
  text <- gsub(
    "^(.+) removeu (.+)$",
    "\\1 removed \\2",
    text
  )

  # Joined/left
  text <- gsub(
    "^(.+) saiu$",
    "\\1 left",
    text
  )
  text <- gsub(
    "^(.+) entrou usando o link de convite deste grupo$",
    "\\1 joined using this group's invite link",
    text
  )
  text <- gsub(
    "^Voc(?:e|\\x{00ea}) entrou usando o link de convite deste grupo$",
    "You joined using this group's invite link",
    text,
    perl = TRUE
  )
  text <- gsub(
    "^Voc(?:e|\\x{00ea}) entrou$",
    "You joined",
    text,
    perl = TRUE
  )
  text <- gsub(
    "^Voc(?:e|\\x{00ea}) saiu$",
    "You left",
    text,
    perl = TRUE
  )

  # Admin changes
  text <- gsub(
    "^(.*) agora (?:e|\\x{00e9}) administrador$",
    "\\1 is now an admin",
    text,
    perl = TRUE
  )
  text <- gsub(
    "^You are now an admin$",
    "You are now an admin",
    text
  )

  # Phone number changes
  text <- gsub(
    "^Voc(?:e|\\x{00ea}) mudou seu n(?:u|\\x{00fa})mero de telefone para um novo n(?:u|\\x{00fa})mero\\..*$",
    "You changed your phone number to a new number.",
    text,
    perl = TRUE
  )

  text
}

whatsapp_resolve_attachments <- function(chat, media_dir, zip_id = NA_character_) {
  if (nrow(chat) == 0) return(chat)
  files <- list.files(media_dir, recursive = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  file_map <- split(files, tolower(basename(files)))
  file_map <- lapply(file_map, sort)
  assignment_counts <- new.env(hash = TRUE, parent = emptyenv())

  select_attachment_path <- function(name) {
    if (is.na(name) || !nzchar(name)) return(list(path = NA_character_, ambiguous = FALSE))
    key <- tolower(name)
    candidates <- file_map[[key]]
    if (length(candidates) == 0) return(list(path = NA_character_, ambiguous = FALSE))

    used <- get0(key, envir = assignment_counts, ifnotfound = 0L) + 1L
    assign(key, used, envir = assignment_counts)
    idx <- ((used - 1L) %% length(candidates)) + 1L
    list(path = candidates[idx], ambiguous = length(candidates) > 1L)
  }

  if (!"attachments" %in% names(chat)) {
    chat$attachments <- lapply(chat$attachment, function(x) if (is.na(x)) character(0) else x)
  }

  attachment_paths <- vector("list", nrow(chat))
  attachment_ambiguous <- vector("list", nrow(chat))
  attachment_keys <- vector("list", nrow(chat))
  for (i in seq_len(nrow(chat))) {
    names_i <- chat$attachments[[i]]
    if (length(names_i) == 0) {
      attachment_paths[[i]] <- character(0)
      attachment_ambiguous[[i]] <- logical(0)
      attachment_keys[[i]] <- character(0)
      next
    }

    paths_i <- character(length(names_i))
    ambiguous_i <- logical(length(names_i))
    keys_i <- character(length(names_i))
    for (j in seq_along(names_i)) {
      selected <- select_attachment_path(names_i[j])
      paths_i[j] <- selected$path
      ambiguous_i[j] <- selected$ambiguous
      keys_i[j] <- chatlens_attachment_key(names_i[j], selected$path, zip_id = zip_id)
    }
    attachment_paths[[i]] <- paths_i
    attachment_ambiguous[[i]] <- ambiguous_i
    attachment_keys[[i]] <- keys_i
  }

  attachment_exists_all <- lapply(attachment_paths, function(paths) {
    if (length(paths) == 0) return(logical(0))
    !is.na(paths) & file.exists(paths)
  })

  placeholder_flags <- if ("attachment_placeholder" %in% names(chat)) chat$attachment_placeholder else rep(FALSE, nrow(chat))
  omitted_flags <- if ("attachment_omitted" %in% names(chat)) chat$attachment_omitted else rep(FALSE, nrow(chat))

  attachment_statuses <- mapply(function(names, exists, omitted_flag, placeholder_flag) {
    if (length(names) == 0) return(character(0))
    if (isTRUE(omitted_flag)) return(rep("omitted", length(names)))
    if (isTRUE(placeholder_flag)) return(ifelse(exists, "present", "placeholder"))
    ifelse(exists, "present", "missing")
  }, chat$attachments, attachment_exists_all, omitted_flags, placeholder_flags, SIMPLIFY = FALSE)

  # First-attachment convenience columns
  attachment_path <- vapply(attachment_paths, function(x) if (length(x)) x[1] else NA_character_, FUN.VALUE = character(1))
  attachment_ambiguous_first <- vapply(attachment_ambiguous, function(x) if (length(x)) x[1] else NA, FUN.VALUE = logical(1))
  attachment_key <- vapply(attachment_keys, function(x) if (length(x)) x[1] else NA_character_, FUN.VALUE = character(1))
  attachment_exists <- vapply(attachment_exists_all, function(x) if (length(x)) x[1] else NA, FUN.VALUE = logical(1))
  attachment_status <- vapply(attachment_statuses, function(x) if (length(x)) x[1] else NA_character_, FUN.VALUE = character(1))

  chat$attachment_paths <- attachment_paths
  chat$attachment_ambiguous <- attachment_ambiguous
  chat$attachment_keys <- attachment_keys
  chat$attachment_exists_all <- attachment_exists_all
  chat$attachment_statuses <- attachment_statuses
  chat$attachment_path <- attachment_path
  chat$attachment_ambiguous_first <- attachment_ambiguous_first
  chat$attachment_key <- attachment_key
  chat$attachment_exists <- attachment_exists
  chat$attachment_status <- attachment_status

  chat
}
