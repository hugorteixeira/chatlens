# Assemble chat text for storage and analysis

.clh_format_messages <- function(chat, text_col = "text", rows = NULL) {
  if (is.null(rows)) rows <- seq_len(nrow(chat))
  sender <- ifelse(is.na(chat$sender[rows]), "SYSTEM", chat$sender[rows])
  ts <- format(chat$timestamp[rows], "%Y-%m-%d %H:%M:%S")
  text <- chat[[text_col]][rows]
  sprintf("%s - %s: %s", ts, sender, text)
}

.clh_format_chat_simple <- function(chat,
                                    text_col = "text_enriched",
                                    date_format = "%Y-%m-%d",
                                    time_format = "%H:%M",
                                    rows = NULL) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  chat <- .clh_chat_text_snapshot(chat, text_col = text_col)
  if (!text_col %in% names(chat)) {
    text_col <- "text"
  }

  if (is.null(rows)) rows <- seq_len(nrow(chat))
  if (length(rows) == 0L) return("")

  ts <- chat$timestamp[rows]
  date_key <- ifelse(is.na(ts), "unknown date", format(ts, date_format))
  time_key <- ifelse(is.na(ts), "unknown time", format(ts, time_format))
  sender_values <- chat$sender[rows]
  sender <- ifelse(is.na(sender_values) | !nzchar(sender_values), "SYSTEM", sender_values)
  text <- as.character(chat[[text_col]][rows])
  text[is.na(text)] <- ""

  n <- length(text)

  # A message can add at most five output lines: a separator, date, blank
  # line, sender header, and its text. Preallocating that upper bound preserves
  # the previous scalar `identical()` semantics exactly while avoiding an O(n²)
  # copy of the complete accumulated output on every iteration.
  lines <- character(5 * n)
  cursor <- 0L
  current_date <- NULL
  current_sender <- NULL

  for (i in seq_len(n)) {
    if (!identical(current_date, date_key[i])) {
      if (cursor > 0L) {
        cursor <- cursor + 1L
        lines[cursor] <- ""
      }
      cursor <- cursor + 1L
      lines[cursor] <- date_key[i]
      cursor <- cursor + 1L
      lines[cursor] <- ""
      current_date <- date_key[i]
      current_sender <- NULL
    }

    if (!identical(current_sender, sender[i])) {
      if (!is.null(current_sender)) {
        cursor <- cursor + 1L
        lines[cursor] <- ""
      }
      cursor <- cursor + 1L
      lines[cursor] <- sprintf("%s %s", time_key[i], sender[i])
      current_sender <- sender[i]
    }

    cursor <- cursor + 1L
    lines[cursor] <- text[i]
  }

  paste(lines[seq_len(cursor)], collapse = "\n")
}
