# Assemble chat text for storage and analysis

.clh_format_messages <- function(chat, text_col = "text") {
  sender <- ifelse(is.na(chat$sender), "SYSTEM", chat$sender)
  ts <- format(chat$timestamp, "%Y-%m-%d %H:%M:%S")
  text <- chat[[text_col]]
  sprintf("%s - %s: %s", ts, sender, text)
}

.clh_format_chat_simple <- function(chat,
                                        text_col = "text_enriched",
                                        date_format = "%Y-%m-%d",
                                        time_format = "%H:%M") {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  chat <- .clh_chat_text_snapshot(chat, text_col = text_col)
  if (!text_col %in% names(chat)) {
    text_col <- "text"
  }

  if (nrow(chat) == 0) return("")

  ts <- chat$timestamp
  date_key <- ifelse(is.na(ts), "unknown date", format(ts, date_format))
  time_key <- ifelse(is.na(ts), "unknown time", format(ts, time_format))
  sender <- ifelse(is.na(chat$sender) | !nzchar(chat$sender), "SYSTEM", chat$sender)
  text <- as.character(chat[[text_col]])
  text[is.na(text)] <- ""

  lines <- character(0)
  current_date <- NULL
  current_sender <- NULL

  for (i in seq_len(nrow(chat))) {
    if (!identical(current_date, date_key[i])) {
      if (length(lines) > 0) lines <- c(lines, "")
      lines <- c(lines, date_key[i], "")
      current_date <- date_key[i]
      current_sender <- NULL
    }

    if (!identical(current_sender, sender[i])) {
      if (!is.null(current_sender)) lines <- c(lines, "")
      lines <- c(lines, sprintf("%s %s", time_key[i], sender[i]))
      current_sender <- sender[i]
    }

    lines <- c(lines, text[i])
  }

  paste(lines, collapse = "\n")
}
