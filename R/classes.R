# Chat object helpers

new_chatlens_chat <- function(df, source = list(), participants = NULL, chat_key = NULL, zip_id = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  class(df) <- c("chatlens_chat", class(df))
  attr(df, "source") <- source
  attr(df, "participants") <- participants
  attr(df, "chat_key") <- chat_key
  attr(df, "zip_id") <- zip_id
  df
}

#' @export
print.chatlens_chat <- function(x, ...) {
  n <- nrow(x)
  participants <- attr(x, "participants")
  source <- attr(x, "source")
  chat_key <- attr(x, "chat_key")
  zip_id <- attr(x, "zip_id")
  ts <- x$timestamp
  ts_valid <- ts[!is.na(ts)]
  rng <- if (length(ts_valid)) range(ts_valid) else as.POSIXct(c(NA, NA), tz = "UTC")
  cat(sprintf("chatlens_chat: %d messages\n", n))
  if (!is.null(participants)) {
    cat(sprintf("Participants: %s\n", paste(participants, collapse = ", ")))
  }
  if (!is.null(chat_key)) {
    cat(sprintf("Chat key: %s\n", chat_key))
  }
  if (!is.null(zip_id)) {
    cat(sprintf("Zip id: %s\n", zip_id))
  }
  if (all(!is.na(rng))) {
    cat(sprintf("Date range: %s to %s\n", format(rng[1], "%Y-%m-%d %H:%M:%S"), format(rng[2], "%Y-%m-%d %H:%M:%S")))
  }
  if (!is.null(source$chat_file)) {
    cat(sprintf("Chat file: %s\n", basename(source$chat_file)))
  }
  invisible(x)
}
