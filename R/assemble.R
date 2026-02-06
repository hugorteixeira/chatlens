# Assemble chat text into periods

chatlens_format_messages <- function(chat, text_col = "text") {
  sender <- ifelse(is.na(chat$sender), "SYSTEM", chat$sender)
  ts <- format(chat$timestamp, "%Y-%m-%d %H:%M:%S")
  text <- chat[[text_col]]
  sprintf("%s - %s: %s", ts, sender, text)
}

#' Slice chat text by time period
#' @param chat A `chatlens_chat`
#' @param period One or more of `"all"`, `"year"`, `"month"`, `"week"`, `"day"`
#' @param text_col Which text column to use
#' @export
cl_chat_split_periods <- function(chat,
                            period = c("all", "year", "month", "week", "day"),
                            text_col = "text_enriched") {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  period <- unique(match.arg(period, choices = c("all", "year", "month", "week", "day"), several.ok = TRUE))

  if (!text_col %in% names(chat)) {
    text_col <- "text"
  }

  lines <- chatlens_format_messages(chat, text_col = text_col)
  ts <- chat$timestamp

  out <- list()
  empty_df <- data.frame(
    period = character(0),
    key = character(0),
    text = character(0),
    stringsAsFactors = FALSE
  )

  make_period_df <- function(period_name, key) {
    valid <- !is.na(key)
    if (!any(valid)) return(empty_df)
    groups <- split(lines[valid], key[valid])
    data.frame(
      period = period_name,
      key = names(groups),
      text = vapply(groups, paste, collapse = "\n", FUN.VALUE = character(1)),
      stringsAsFactors = FALSE
    )
  }

  if ("all" %in% period) {
    out[[length(out) + 1]] <- data.frame(
      period = "all",
      key = "all",
      text = paste(lines, collapse = "\n"),
      stringsAsFactors = FALSE
    )
  }

  if ("year" %in% period) {
    key <- format(ts, "%Y")
    key[is.na(ts)] <- NA_character_
    out[[length(out) + 1]] <- make_period_df("year", key)
  }

  if ("month" %in% period) {
    key <- format(ts, "%Y-%m")
    key[is.na(ts)] <- NA_character_
    out[[length(out) + 1]] <- make_period_df("month", key)
  }

  if ("week" %in% period) {
    if (length(ts) == 0) {
      key <- character(0)
    } else {
      key <- paste0(format(ts, "%G"), "-W", format(ts, "%V"))
    }
    key[is.na(ts)] <- NA_character_
    out[[length(out) + 1]] <- make_period_df("week", key)
  }

  if ("day" %in% period) {
    key <- format(ts, "%Y-%m-%d")
    key[is.na(ts)] <- NA_character_
    out[[length(out) + 1]] <- make_period_df("day", key)
  }

  if (length(out) == 0) {
    df <- empty_df
  } else {
    out <- Filter(function(x) nrow(x) > 0, out)
    if (length(out) == 0) {
      df <- empty_df
    } else {
      df <- do.call(rbind, out)
    }
  }

  attr(df, "chat_key") <- attr(chat, "chat_key")
  store_dir <- attr(chat, "source")$store_dir %||% NULL
  attr(df, "store_dir") <- store_dir
  if (!is.null(store_dir) && nzchar(store_dir)) {
    attr(df, "analysis_dir") <- file.path(store_dir, "analysis")
  }
  df
}

#' Write period text to files
#' @param periods Data frame produced by [cl_chat_split_periods()]
#' @param output_dir Directory where period files are written
#' @param prefix Filename prefix for generated text files
#' @export
cl_periods_write <- function(periods, output_dir, prefix = "chat") {
  if (missing(output_dir) || is.null(output_dir)) {
    store_dir <- attr(periods, "store_dir")
    if (is.null(store_dir) || !nzchar(store_dir)) {
      stop("output_dir is NULL and periods has no store_dir attribute")
    }
    output_dir <- file.path(store_dir, "periods")
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- character(nrow(periods))
  for (i in seq_len(nrow(periods))) {
    p <- periods$period[i]
    key <- gsub("[^A-Za-z0-9_-]", "_", periods$key[i])
    if (p == "all") {
      fname <- paste0(prefix, "_all.txt")
    } else {
      fname <- paste0(prefix, "_", p, "_", key, ".txt")
    }
    path <- file.path(output_dir, fname)
    writeLines(periods$text[i], path)
    paths[i] <- path
  }

  periods$path <- paths
  periods
}

#' Filter chat by date range
#' @param chat A `chatlens_chat` object
#' @param start Optional inclusive start datetime
#' @param end Optional inclusive end datetime
#' @export
cl_filter_date <- function(chat, start = NULL, end = NULL) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  tz <- attr(chat, "source")$tz %||% "UTC"
  if (!is.null(start)) start <- as.POSIXct(start, tz = tz)
  if (!is.null(end)) end <- as.POSIXct(end, tz = tz)

  keep <- rep(TRUE, nrow(chat))
  if (!is.null(start)) keep <- keep & chat$timestamp >= start
  if (!is.null(end)) keep <- keep & chat$timestamp <= end
  keep[is.na(keep)] <- FALSE

  chat[keep, , drop = FALSE]
}
