# WhatsApp Android import and parsing

#' Import a WhatsApp Android ZIP export
#' @param path Path to the WhatsApp export `.zip`
#' @param cache_dir Optional cache directory. When `NULL`, uses `~/.chatlens`.
#' @param chat_key Optional chat key used for cache storage
#' @param ask_confirmation Ask to confirm detected chat file
#' @param encoding Character encoding to try when reading the chat text
#' @param verbose Whether to emit progress messages, including total import time
#'   and a stage-by-stage timing breakdown
#' @param tz Timezone for parsed timestamps
#' @param date_order Date order, "dmy" (default) or "mdy"
#' @param omit_sender_na Logical; when `TRUE` (default), drops parsed rows
#'   whose sender is missing, empty, `"NA"`, or `"NULL"`.
#' @param workers Number of parser workers. `NULL` automatically uses up to four
#'   workers for large imports on systems that support process forking. Use `1`
#'   to force single-process parsing.
#' @details
#' WhatsApp attachment markers are used to distinguish actual attachments from
#' ordinary messages that merely mention words such as "attached" or
#' "anexado". Recognized attachment types include audio, image, video, contact
#' cards (`.vcf`), and other files. Filename-like fragments inside URLs and
#' email addresses are ignored. Each detected reference is resolved against the
#' extracted ZIP and marked as present, missing, a placeholder, or omitted.
#'
#' With `verbose = TRUE`, the importer reports attachment availability counts.
#' The final console message reports total elapsed time plus setup, extraction,
#' chat reading, message parsing, attachment resolution, and cache-save timings.
#' @export
cl_whatsapp_import <- function(path,
                                cache_dir = NULL,
                                chat_key = NULL,
                                ask_confirmation = FALSE,
                                encoding = c("UTF-8", "latin1"),
                                verbose = TRUE,
                                tz = "UTC",
                                date_order = c("dmy", "mdy"),
                                omit_sender_na = TRUE,
                                workers = NULL) {
  import_started <- proc.time()[["elapsed"]]

  path <- path.expand(path)
  if (!file.exists(path)) stop("ZIP file not found: ", path)

  date_order <- match.arg(date_order)
  if (!is.logical(omit_sender_na) || length(omit_sender_na) != 1L || is.na(omit_sender_na)) {
    stop("omit_sender_na must be TRUE or FALSE")
  }
  zip_id <- .clh_zip_id(path)
  extract_dir <- .clh_extract_dir(zip_id, cache_dir)
  marker <- file.path(extract_dir, ".extracted")
  extraction_cached <- file.exists(marker)
  setup_finished <- proc.time()[["elapsed"]]

  extraction_started <- setup_finished
  if (!dir.exists(extract_dir) || !file.exists(marker)) {
    .clh_ensure_dir(extract_dir)
    if (verbose) message("Extracting ZIP to cache: ", extract_dir)
    utils::unzip(path, exdir = extract_dir)
    file.create(marker)
  } else if (verbose) {
    message("Using cached extraction: ", extract_dir)
  }
  extraction_finished <- proc.time()[["elapsed"]]

  chat_read_started <- extraction_finished
  files <- list.files(extract_dir, recursive = TRUE, full.names = TRUE)
  chat_file <- .clh_whatsapp_detect_chat_file(files, ask_confirmation = ask_confirmation, encoding = encoding)
  if (verbose) message("Chat file: ", basename(chat_file))
  if (is.null(chat_key) || !nzchar(chat_key)) chat_key <- .clh_chat_key_from_file(chat_file)
  store_dir <- .clh_chat_store_dir(chat_key, cache_dir)

  lines <- .clh_read_lines(chat_file, encoding = encoding)
  chat_read_finished <- proc.time()[["elapsed"]]

  parsing_started <- chat_read_finished
  parse_workers <- .clh_whatsapp_resolve_workers(workers, task_count = length(lines))
  if (verbose) {
    message(
      "Parsing messages with ",
      parse_workers,
      if (parse_workers == 1L) " worker..." else " workers..."
    )
  }
  chat <- .clh_whatsapp_parse_lines(
    lines,
    tz = tz,
    date_order = date_order,
    omit_sender_na = omit_sender_na,
    workers = parse_workers
  )
  parsing_finished <- proc.time()[["elapsed"]]

  attachments_started <- parsing_finished
  chat <- .clh_whatsapp_resolve_attachments(chat, media_dir = extract_dir, zip_id = zip_id)
  if (verbose) message(.clh_whatsapp_attachment_status_message(chat))
  attachments_finished <- proc.time()[["elapsed"]]

  finalize_started <- attachments_finished
  participants <- .clh_detect_participants(chat)
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

  chat <- .clh_new_chat(chat, source = source, participants = participants, chat_key = chat_key, zip_id = zip_id)
  .clh_save_original_chat(chat, cache_dir = cache_dir, overwrite = TRUE)
  chat <- .clh_save_current_chat(chat, cache_dir = cache_dir)
  import_finished <- proc.time()[["elapsed"]]

  timing <- list(
    total_seconds = unname(import_finished - import_started),
    extraction_cached = extraction_cached,
    stages_seconds = list(
      setup = unname(setup_finished - import_started),
      extraction = unname(extraction_finished - extraction_started),
      chat_read = unname(chat_read_finished - chat_read_started),
      message_parsing = unname(parsing_finished - parsing_started),
      attachment_resolution = unname(attachments_finished - attachments_started),
      finalize_and_cache_save = unname(import_finished - finalize_started)
    )
  )

  if (verbose) {
    stages <- timing$stages_seconds
    message(
      "Import completed in ", .clh_format_elapsed(timing$total_seconds), ".\n",
      "  - setup: ", .clh_format_elapsed(stages$setup), "\n",
      "  - extraction", if (isTRUE(timing$extraction_cached)) " (cached)" else "", ": ",
      .clh_format_elapsed(stages$extraction), "\n",
      "  - chat detection + read: ", .clh_format_elapsed(stages$chat_read), "\n",
      "  - message parsing (", parse_workers, if (parse_workers == 1L) " worker" else " workers", "): ",
      .clh_format_elapsed(stages$message_parsing), "\n",
      "  - attachment resolution: ", .clh_format_elapsed(stages$attachment_resolution), "\n",
      "  - finalize + cache save: ", .clh_format_elapsed(stages$finalize_and_cache_save)
    )
  }

  chat
}

.clh_whatsapp_detect_chat_file <- function(files, ask_confirmation = FALSE, encoding = c("UTF-8", "latin1")) {
  txt_files <- files[grepl("\\.txt$", files, ignore.case = TRUE)]
  if (length(txt_files) == 0) stop("No .txt files found in the ZIP export.")

  base_names <- basename(txt_files)
  priority <- grepl("whatsapp|conversa|chat", base_names, ignore.case = TRUE)

  scores <- vapply(seq_along(txt_files), function(i) {
    f <- txt_files[i]
    lines <- tryCatch(.clh_read_lines(f, encoding = encoding), error = function(e) character(0))
    # Only look at the first 200 lines for speed
    if (length(lines) > 200) lines <- lines[1:200]
    sum(.clh_whatsapp_is_message_line(lines)) + if (priority[i]) 1000 else 0
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

.clh_whatsapp_is_message_line <- function(line) {
  pattern <- "^[\\s\\u200e\\ufeff]*\\d{1,2}/\\d{1,2}/\\d{2,4}[, ]+\\d{1,2}:\\d{2}(?::\\d{2})?(?:\\s?[APMapm]{2})?\\s+-\\s+"
  grepl(pattern, line)
}

.clh_whatsapp_resolve_workers <- function(workers = NULL,
                                          task_count = 0L,
                                          auto_max = 4L,
                                          auto_threshold = 50000L) {
  task_count <- suppressWarnings(as.integer(task_count)[1])
  if (is.na(task_count) || task_count < 1L) return(1L)

  if (is.null(workers)) {
    if (.Platform$OS.type == "windows" || task_count < auto_threshold) return(1L)
    detected <- suppressWarnings(parallel::detectCores(logical = FALSE))
    if (length(detected) == 0L || is.na(detected) || detected < 2L) return(1L)
    return(max(1L, min(as.integer(auto_max), detected - 1L, task_count)))
  }

  workers_integer <- suppressWarnings(as.integer(workers))
  if (!is.numeric(workers) || length(workers) != 1L || is.na(workers) ||
      !is.finite(workers) || workers < 1 || is.na(workers_integer) ||
      workers != workers_integer) {
    stop("workers must be NULL or a positive whole number")
  }

  workers <- min(workers_integer, task_count)
  if (.Platform$OS.type == "windows" && workers > 1L) {
    warning("Parallel parsing is not available on Windows; using workers = 1", call. = FALSE)
    return(1L)
  }

  workers
}

.clh_whatsapp_chunk_indices <- function(task_count, workers) {
  task_count <- as.integer(task_count)
  workers <- min(as.integer(workers), task_count)
  boundaries <- floor(seq.int(0, task_count, length.out = workers + 1L))
  lapply(seq_len(workers), function(i) {
    seq.int(boundaries[i] + 1L, boundaries[i + 1L])
  })
}

.clh_whatsapp_detect_attachments_parallel <- function(text, workers = 1L) {
  workers <- .clh_whatsapp_resolve_workers(workers, task_count = length(text))
  if (workers <= 1L) return(.clh_whatsapp_detect_attachments(text))

  chunks <- .clh_whatsapp_chunk_indices(length(text), workers)
  parts <- parallel::mclapply(
    chunks,
    function(idx) .clh_whatsapp_detect_attachments(text[idx]),
    mc.cores = workers,
    mc.preschedule = TRUE,
    mc.set.seed = FALSE
  )
  failed <- vapply(parts, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop("Parallel attachment detection failed: ", as.character(parts[[which(failed)[1]]]))
  }

  list(
    filenames = unlist(lapply(parts, `[[`, "filenames"), recursive = FALSE, use.names = FALSE),
    types = unlist(lapply(parts, `[[`, "types"), recursive = FALSE, use.names = FALSE),
    placeholder = unlist(lapply(parts, `[[`, "placeholder"), use.names = FALSE),
    omitted = unlist(lapply(parts, `[[`, "omitted"), use.names = FALSE)
  )
}

.clh_whatsapp_parse_lines <- function(lines,
                                      tz = "UTC",
                                      date_order = "dmy",
                                      omit_sender_na = TRUE,
                                      workers = 1L) {
  if (length(lines) == 0) return(data.frame())
  if (!is.logical(omit_sender_na) || length(omit_sender_na) != 1L || is.na(omit_sender_na)) {
    stop("omit_sender_na must be TRUE or FALSE")
  }

  starts <- which(.clh_whatsapp_is_message_line(lines))
  if (length(starts) == 0) stop("No WhatsApp message lines detected.")
  ends <- c(starts[-1] - 1, length(lines))

  headers <- lines[starts]
  sep <- regexpr(" - ", headers, fixed = TRUE)
  valid <- sep != -1L
  if (!any(valid)) return(data.frame())
  if (!all(valid)) {
    starts <- starts[valid]
    ends <- ends[valid]
    headers <- headers[valid]
    sep <- sep[valid]
  }

  date_time <- substr(headers, 1L, sep - 1L)
  rest <- substring(headers, sep + 3L)
  timestamp <- .clh_parse_datetimes(date_time, tz = tz, date_order = date_order)

  colon <- regexpr(": ", rest, fixed = TRUE)
  has_sender <- colon != -1L
  sender <- rep(NA_character_, length(rest))
  sender[has_sender] <- substr(rest[has_sender], 1L, colon[has_sender] - 1L)
  text <- rest
  text[has_sender] <- substring(rest[has_sender], colon[has_sender] + 2L)

  multiline <- which(ends > starts)
  if (length(multiline)) {
    text[multiline] <- vapply(multiline, function(i) {
      continuation <- lines[seq.int(starts[i] + 1L, ends[i])]
      paste(c(text[i], continuation), collapse = "\n")
    }, FUN.VALUE = character(1))
  }

  if (isTRUE(omit_sender_na)) {
    sender_trim <- trimws(sender)
    sender_norm <- tolower(sender_trim)
    keep <- !is.na(sender_trim) & nzchar(sender_trim) & !sender_norm %in% c("na", "null")
    timestamp <- timestamp[keep]
    sender <- sender[keep]
    text <- text[keep]
  }

  if (length(text) == 0L) return(data.frame())
  df <- data.frame(
    timestamp = timestamp,
    sender = sender,
    text = text,
    stringsAsFactors = FALSE
  )

  df$message_id <- seq_len(nrow(df))
  df$text_raw <- df$text

  attachment_info <- .clh_whatsapp_detect_attachments_parallel(df$text, workers = workers)
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
    df$text[idx] <- vapply(df$text[idx], .clh_whatsapp_normalize_system_text, FUN.VALUE = character(1))
  }

  df
}

.clh_whatsapp_strip_directionality_marks <- function(x) {
  marks <- intToUtf8(
    c(0x200e, 0x200f, 0x202a:0x202e, 0x2066:0x2069, 0xfeff)
  )
  gsub(paste0("[", marks, "]"), "", as.character(x), perl = TRUE)
}

.clh_whatsapp_clean_attachment_name <- function(x) {
  trimws(.clh_whatsapp_strip_directionality_marks(x))
}

.clh_whatsapp_attachment_name_key <- function(x) {
  tolower(.clh_whatsapp_clean_attachment_name(x))
}

.clh_whatsapp_filter_link_matches <- function(matches, match_locations, link_locations) {
  rows <- which(
    lengths(matches) > 0L &
      vapply(link_locations, function(x) length(x) > 0L && x[1] != -1L, FUN.VALUE = logical(1))
  )
  if (length(rows) == 0L) return(matches)

  for (i in rows) {
    candidate_start <- as.integer(match_locations[[i]])
    candidate_end <- candidate_start + attr(match_locations[[i]], "match.length") - 1L
    link_start <- as.integer(link_locations[[i]])
    link_end <- link_start + attr(link_locations[[i]], "match.length") - 1L

    inside_link <- vapply(seq_along(candidate_start), function(j) {
      any(candidate_end[j] >= link_start & candidate_end[j] <= link_end)
    }, FUN.VALUE = logical(1))
    matches[[i]] <- matches[[i]][!inside_link]
  }

  matches
}

.clh_whatsapp_detect_attachments <- function(text) {
  audio_ext <- c("opus", "mp3", "m4a", "wav", "ogg", "aac", "flac")
  image_ext <- c("jpg", "jpeg", "png", "gif", "webp", "heic", "bmp", "tiff")
  video_ext <- c("mp4", "mov", "mkv", "avi", "3gp", "webm")
  contact_ext <- "vcf"
  doc_ext <- c("pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "txt", "zip", "rar", "7z", "csv")

  exts <- c(audio_ext, image_ext, video_ext, contact_ext, doc_ext)
  marker_labels <- c("arquivo anexado", "file attached", "attached", "anexado")
  marker_terms <- paste(marker_labels, collapse = "|")
  wrapped_marker_pattern <- paste0(
    "(?m)\\((?:",
    marker_terms,
    ")\\)[\\p{Z}\\s]*$"
  )
  marked_file_pattern <- paste0(
    "(?m)^[^\\r\\n]*?\\.(?:",
    paste(exts, collapse = "|"),
    ")(?=[\\p{Z}\\s]*\\((?:",
    marker_terms,
    ")\\)[\\p{Z}\\s]*$)"
  )
  file_pattern <- paste0(
    "(?<![[:alnum:]])",
    "[[:alnum:]_][[:alnum:]_ .()'\\-]{0,160}\\.(?:",
    paste(exts, collapse = "|"),
    ")\\b"
  )
  link_pattern <- paste0(
    "(?i)(?:",
    "\\b(?:https?|ftp)://[^\\s<>]+",
    "|\\bwww\\.[[:alnum:]-]+(?:\\.[[:alnum:]-]+)+(?::[0-9]+)?(?:/[^\\s<>]*)?",
    "|\\b[[:alnum:]_.%+\\-]+@[[:alnum:].\\-]+\\.[[:alpha:]]{2,}\\b",
    ")"
  )

  wrapped_marker <- grepl(wrapped_marker_pattern, text, ignore.case = TRUE, perl = TRUE)
  marked_matches <- rep(list(character(0)), length(text))
  marked_rows <- which(wrapped_marker)
  if (length(marked_rows)) {
    marked_matches[marked_rows] <- regmatches(
      text[marked_rows],
      gregexpr(marked_file_pattern, text[marked_rows], ignore.case = TRUE, perl = TRUE)
    )
  }

  generic_matches <- rep(list(character(0)), length(text))
  generic_rows <- which(lengths(marked_matches) == 0L)
  if (length(generic_rows)) {
    generic_text <- text[generic_rows]
    match_locations <- gregexpr(file_pattern, generic_text, ignore.case = TRUE, perl = TRUE)
    link_locations <- gregexpr(link_pattern, generic_text, perl = TRUE)
    matches <- regmatches(generic_text, match_locations)
    generic_matches[generic_rows] <- .clh_whatsapp_filter_link_matches(
      matches,
      match_locations,
      link_locations
    )
  }

  filenames <- Map(function(marked, generic) {
    if (length(marked)) {
      marked <- .clh_whatsapp_clean_attachment_name(marked)
      # Direct helper tests and pre-parsed inputs may still include "Sender: ".
      # Colons are not valid Android filename characters, so this is safe to
      # remove without changing a real attachment basename.
      marked <- sub("^[^:\\r\\n]{1,80}:\\s+", "", marked, perl = TRUE)
      marked <- .clh_whatsapp_clean_attachment_name(marked)
    }

    if (length(generic)) {
      generic <- trimws(generic)
      generic <- gsub("^[\"'`<\\[(]+", "", generic)
      generic <- gsub("[\"'`>\\])]+$", "", generic)
      generic <- gsub("\\s+", " ", generic)
      generic <- gsub(
        "^(arquivo|file|attached|anexado|anexo|media|midia)\\s+",
        "",
        generic,
        ignore.case = TRUE
      )
    }

    # The explicit WhatsApp marker yields the complete Unicode basename. The
    # generic scanner is only a fallback; combining both can count a partial
    # ASCII suffix of the same Unicode filename as a second attachment.
    x <- if (length(marked)) marked else generic
    x <- x[nzchar(x)]
    if (length(x) <= 1) return(x)
    x[!duplicated(.clh_whatsapp_attachment_name_key(x))]
  }, marked_matches, generic_matches)

  placeholder <- wrapped_marker
  bare_rows <- which(
    !placeholder &
      !is.na(text) &
      nchar(text, type = "chars", allowNA = TRUE) <= 32L
  )
  if (length(bare_rows)) {
    bare_text <- tolower(trimws(.clh_whatsapp_strip_directionality_marks(text[bare_rows])))
    placeholder[bare_rows] <- bare_text %in% marker_labels
  }
  omitted_pattern <- "m(?:i|\\x{00ed})dia omitida|media omitted|<media omitted>"

  omitted <- grepl(omitted_pattern, text, ignore.case = TRUE, perl = TRUE)

  classify <- function(name) {
    ext <- tolower(sub(".*\\.", "", name))
    if (ext %in% audio_ext) return("audio")
    if (ext %in% image_ext) return("image")
    if (ext %in% video_ext) return("video")
    if (ext %in% contact_ext) return("contact")
    if (ext %in% doc_ext) return("file")
    "file"
  }

  types <- lapply(filenames, function(x) {
    if (length(x) == 0) return(character(0))
    vapply(x, classify, FUN.VALUE = character(1))
  })

  list(filenames = filenames, placeholder = placeholder, omitted = omitted, types = types)
}

.clh_whatsapp_attachment_status_message <- function(chat) {
  statuses <- unlist(chat$attachment_statuses, use.names = FALSE)
  if (length(statuses) == 0L) {
    return("Attachment check: no attachment references detected.")
  }

  statuses[is.na(statuses) | !nzchar(statuses)] <- "unknown"
  preferred_order <- c("present", "missing", "placeholder", "omitted", "unknown")
  status_order <- c(preferred_order, sort(setdiff(unique(statuses), preferred_order)))
  counts <- vapply(status_order, function(status) sum(statuses == status), FUN.VALUE = integer(1))
  counts <- counts[counts > 0L]
  total <- length(statuses)
  reference_label <- if (total == 1L) "attachment reference" else "attachment references"

  paste0(
    "Attachment check: ",
    total,
    " ",
    reference_label,
    " (",
    paste0(unname(counts), " ", names(counts), collapse = ", "),
    ")."
  )
}

.clh_whatsapp_normalize_system_text <- function(text) {
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

.clh_whatsapp_resolve_attachments <- function(chat, media_dir, zip_id = NA_character_) {
  if (nrow(chat) == 0) return(chat)
  files <- list.files(media_dir, recursive = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  file_map <- split(files, .clh_whatsapp_attachment_name_key(basename(files)))
  file_map <- lapply(file_map, sort)
  assignment_counts <- new.env(hash = TRUE, parent = emptyenv())

  select_attachment_path <- function(name) {
    if (is.na(name) || !nzchar(name)) return(list(path = NA_character_, ambiguous = FALSE))
    key <- .clh_whatsapp_attachment_name_key(name)
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
      keys_i[j] <- .clh_attachment_key(names_i[j], selected$path, zip_id = zip_id)
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
