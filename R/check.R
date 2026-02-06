# WhatsApp import checks and summaries

cl_period_key <- function(ts, period) {
  if (length(ts) == 0) return(character(0))
  if (period == "year") return(format(ts, "%Y"))
  if (period == "month") return(format(ts, "%Y-%m"))
  if (period == "day") return(format(ts, "%Y-%m-%d"))
  if (period == "week") {
    key <- paste0(format(ts, "%G"), "-W", format(ts, "%V"))
    key[is.na(ts)] <- NA_character_
    return(key)
  }
  stop("Unsupported period: ", period)
}

chatlens_normalize_label <- function(x, unknown = "unknown") {
  x <- as.character(x)
  bad <- is.na(x) | x == "<NA>" | !nzchar(trimws(x))
  x[bad] <- unknown
  x
}

chatlens_count_df <- function(x, name_col) {
  tbl <- table(x, useNA = "ifany")
  if (length(tbl) == 0) {
    out <- data.frame(value = character(0), count = integer(0), stringsAsFactors = FALSE)
    names(out)[1] <- name_col
    return(out)
  }

  out <- data.frame(
    value = chatlens_normalize_label(names(tbl)),
    count = as.integer(tbl),
    stringsAsFactors = FALSE
  )
  names(out)[1] <- name_col
  out <- out[order(-out$count, out[[name_col]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

chatlens_print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

chatlens_print_count_rows <- function(df, name_col, total = NULL) {
  if (nrow(df) == 0) {
    cat("  - none: 0\n")
    return(invisible(NULL))
  }

  if (is.null(total)) total <- sum(df$count)

  for (i in seq_len(nrow(df))) {
    label <- df[[name_col]][i]
    count <- df$count[i]
    if (total > 0) {
      pct <- (count / total) * 100
      cat(sprintf("  - %s: %d (%.1f%%)\n", label, count, pct))
    } else {
      cat(sprintf("  - %s: %d\n", label, count))
    }
  }

  invisible(NULL)
}

#' Summarize an imported WhatsApp chat
#' @param chat A `chatlens_chat` object
#' @param path Optional ZIP path to import and then summarize
#' @param ask_confirmation Ask to confirm detected chat file
#' @param ... Additional arguments passed to [cl_whatsapp_import()]
#' @export
cl_whatsapp_summary <- function(chat = NULL,
                              path = NULL,
                              ask_confirmation = FALSE,
                              ...) {
  if (is.null(chat)) {
    if (is.null(path)) stop("Provide chat or path")
    chat <- cl_whatsapp_import(path = path, ask_confirmation = ask_confirmation, ...)
  }

  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  n <- nrow(chat)
  ts <- chat$timestamp
  ts_valid <- ts[!is.na(ts)]

  date_range <- if (length(ts_valid)) {
    rng <- range(ts_valid)
    sprintf("%s to %s", format(rng[1], "%Y-%m-%d"), format(rng[2], "%Y-%m-%d"))
  } else {
    NA_character_
  }

  participants <- attr(chat, "participants")
  if (is.null(participants)) participants <- unique(stats::na.omit(chat$sender))
  participants <- unique(participants[!is.na(participants) & nzchar(participants)])

  att_tbl <- cl_attachments(chat)
  total_attachments <- nrow(att_tbl)

  summary_df <- data.frame(
    metric = c("messages", "participants", "attachments", "date_range"),
    value = c(
      n,
      length(participants),
      total_attachments,
      if (is.na(date_range)) "not available (no valid timestamps)" else date_range
    ),
    stringsAsFactors = FALSE
  )

  msg_type_df <- chatlens_count_df(chat$message_type, "message_type")

  if (nrow(att_tbl) > 0) {
    att_type_df <- chatlens_count_df(att_tbl$attachment_type, "attachment_type")
    att_status_df <- chatlens_count_df(att_tbl$attachment_status, "attachment_status")
  } else {
    att_type_df <- data.frame(attachment_type = character(0), count = integer(0), stringsAsFactors = FALSE)
    att_status_df <- data.frame(attachment_status = character(0), count = integer(0), stringsAsFactors = FALSE)
  }

  periods <- c("year", "month", "week", "day")
  period_counts <- lapply(periods, function(p) {
    keys <- cl_period_key(ts_valid, p)
    data.frame(
      period = p,
      distinct = length(unique(keys)),
      stringsAsFactors = FALSE
    )
  })
  period_df <- do.call(rbind, period_counts)

  notes <- character(0)

  missing_ts <- sum(is.na(ts))
  if (missing_ts > 0) {
    notes <- c(notes, sprintf(
      "%d message(s) have missing timestamps; date range and period counts ignore them.",
      missing_ts
    ))
  }

  missing_sender <- sum(is.na(chat$sender) | !nzchar(chat$sender))
  if (missing_sender > 0) {
    notes <- c(notes, sprintf(
      "%d message(s) have no sender and are usually system events.",
      missing_sender
    ))
  }

  unknown_msg <- if (nrow(msg_type_df)) {
    sum(msg_type_df$count[msg_type_df$message_type == "unknown"])
  } else {
    0L
  }
  if (unknown_msg > 0) {
    notes <- c(notes, sprintf(
      "%d message(s) have unknown message_type (NA or empty).",
      unknown_msg
    ))
  }

  if (nrow(att_tbl) == 0) {
    notes <- c(notes, "No attachments were detected in this chat.")
  } else {
    unknown_att_type <- sum(att_type_df$count[att_type_df$attachment_type == "unknown"])
    if (unknown_att_type > 0) {
      notes <- c(notes, sprintf(
        "%d attachment(s) have unknown type (NA or empty), usually placeholders or unsupported extensions.",
        unknown_att_type
      ))
    }

    unknown_att_status <- sum(att_status_df$count[att_status_df$attachment_status == "unknown"])
    if (unknown_att_status > 0) {
      notes <- c(notes, sprintf(
        "%d attachment(s) have unknown status (NA or empty).",
        unknown_att_status
      ))
    }

    missing_att_name <- sum(is.na(att_tbl$attachment) | !nzchar(att_tbl$attachment))
    if (missing_att_name > 0) {
      notes <- c(notes, sprintf(
        "%d attachment row(s) have no filename (placeholder or omitted media).",
        missing_att_name
      ))
    }
  }

  cat("Chat Summary\n")
  cat("============\n")
  cat(sprintf("  - messages: %s\n", summary_df$value[summary_df$metric == "messages"]))
  cat(sprintf("  - participants: %s\n", summary_df$value[summary_df$metric == "participants"]))
  cat(sprintf("  - attachments: %s\n", summary_df$value[summary_df$metric == "attachments"]))
  cat(sprintf("  - date_range: %s\n", summary_df$value[summary_df$metric == "date_range"]))

  chatlens_print_section("Message Types")
  chatlens_print_count_rows(msg_type_df, "message_type", total = n)

  chatlens_print_section("Attachment Types")
  chatlens_print_count_rows(att_type_df, "attachment_type", total = total_attachments)

  chatlens_print_section("Attachment Status")
  chatlens_print_count_rows(att_status_df, "attachment_status", total = total_attachments)

  chatlens_print_section("Distinct Periods")
  for (i in seq_len(nrow(period_df))) {
    cat(sprintf("  - %s: %d\n", period_df$period[i], period_df$distinct[i]))
  }

  chatlens_print_section("Data Quality Notes")
  if (length(notes) == 0) {
    cat("  - no missing or unknown values detected.\n")
  } else {
    for (note in notes) {
      cat("  - ", note, "\n", sep = "")
    }
  }

  invisible(list(
    summary = summary_df,
    message_types = msg_type_df,
    attachment_types = att_type_df,
    attachment_status = att_status_df,
    periods = period_df,
    notes = notes
  ))
}

#' Filter chat by period key
#' @param chat A `chatlens_chat` object
#' @param period One of `"year"`, `"month"`, `"week"`, or `"day"`
#' @param key Period key value to keep
#' @export
cl_chat_filter_period <- function(chat, period, key) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  period <- match.arg(period, choices = c("year", "month", "week", "day"))
  keys <- cl_period_key(chat$timestamp, period)
  keep <- keys == key
  keep[is.na(keep)] <- FALSE
  chat[keep, , drop = FALSE]
}
