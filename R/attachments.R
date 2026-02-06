# Attachment helpers

#' Expand chat attachments into a table
#' @param chat A `chatlens_chat` object
#' @export
cl_attachments <- function(chat) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  n <- nrow(chat)
  rows <- list()

  for (i in seq_len(n)) {
    att_names <- character(0)
    if ("attachments" %in% names(chat)) {
      att_names <- chat$attachments[[i]]
    } else if ("attachment" %in% names(chat) && !is.na(chat$attachment[i])) {
      att_names <- chat$attachment[i]
    }

    omitted_flag <- "attachment_omitted" %in% names(chat) && isTRUE(chat$attachment_omitted[i])
    placeholder_flag <- "attachment_placeholder" %in% names(chat) && isTRUE(chat$attachment_placeholder[i])
    if (length(att_names) == 0 && (omitted_flag || placeholder_flag)) {
      att_names <- NA_character_
    }

    if (length(att_names) == 0) next

    message_id_i <- if ("message_id" %in% names(chat)) chat$message_id[i] else i
    sender_i <- if ("sender" %in% names(chat)) chat$sender[i] else NA_character_
    timestamp_i <- if ("timestamp" %in% names(chat)) chat$timestamp[i] else as.POSIXct(NA, tz = "UTC")
    text_i <- if ("text" %in% names(chat)) chat$text[i] else NA_character_

    types <- if ("attachment_types" %in% names(chat)) chat$attachment_types[[i]] else rep(NA_character_, length(att_names))
    paths <- if ("attachment_paths" %in% names(chat)) chat$attachment_paths[[i]] else rep(NA_character_, length(att_names))
    statuses <- if ("attachment_statuses" %in% names(chat)) chat$attachment_statuses[[i]] else rep(NA_character_, length(att_names))
    keys <- if ("attachment_keys" %in% names(chat)) chat$attachment_keys[[i]] else rep(NA_character_, length(att_names))

    if (length(types) == 1 && length(att_names) > 1) types <- rep(types, length(att_names))
    if (length(paths) == 1 && length(att_names) > 1) paths <- rep(paths, length(att_names))
    if (length(statuses) == 1 && length(att_names) > 1) statuses <- rep(statuses, length(att_names))
    if (length(keys) == 1 && length(att_names) > 1) keys <- rep(keys, length(att_names))

    for (j in seq_along(att_names)) {
      status_j <- if (length(statuses) >= j) statuses[j] else NA_character_
      if (is.na(status_j) || !nzchar(status_j)) {
        if (omitted_flag) {
          status_j <- "omitted"
        } else if (placeholder_flag) {
          status_j <- "placeholder"
        }
      }
      rows[[length(rows) + 1]] <- data.frame(
        message_id = message_id_i,
        sender = sender_i,
        timestamp = timestamp_i,
        text = text_i,
        attachment = att_names[j],
        attachment_type = if (length(types) >= j) types[j] else NA_character_,
        attachment_path = if (length(paths) >= j) paths[j] else NA_character_,
        attachment_key = if (length(keys) >= j) keys[j] else NA_character_,
        attachment_status = status_j,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      message_id = integer(0),
      sender = character(0),
      timestamp = as.POSIXct(character(0)),
      text = character(0),
      attachment = character(0),
      attachment_type = character(0),
      attachment_path = character(0),
      attachment_key = character(0),
      attachment_status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}
