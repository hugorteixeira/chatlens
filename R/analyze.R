# LLM analysis helpers

#' Analyze text with genflow
#' @param text Input text to analyze
#' @param prompt Prompt or instruction for the model
#' @param service Optional model provider passed to `genflow`
#' @param model Optional model identifier
#' @param directory Optional directory where analysis output may be saved
#' @param label Optional label used for saved analysis files. If omitted (or
#'   when it starts with `"analysis"`), saved filenames use the first 10 chars
#'   of the model response as prefix.
#' @param return Either `"genflow"` (raw provider response) or `"standard"`
#' @param save_meta Whether to save raw provider metadata as JSON
#' @param meta_suffix Suffix used when saving metadata JSON
#' @param ... Additional arguments passed to [genflow::gen_txt()]
#' @export
cl_analyze_text <- function(text,
                            prompt,
                            service = NULL,
                            model = NULL,
                            directory = NULL,
                            label = NULL,
                            return = c("genflow", "standard"),
                            save_meta = TRUE,
                            meta_suffix = "_meta.json",
                            ...) {
  if (!requireNamespace("genflow", quietly = TRUE)) stop("genflow is required for analysis")
  if (missing(prompt) || is.null(prompt)) stop("prompt is required")

  return <- match.arg(return)

  use_response_prefix <- !is.null(directory) &&
    (is.null(label) || !nzchar(label) || isTRUE(grepl("^analysis(?:_|$)", label)))

  args <- list(prompt)
  args$add <- text
  if (!is.null(service)) args$service <- service
  if (!is.null(model)) args$model <- model
  if (!use_response_prefix) {
    if (!is.null(directory)) args$directory <- directory
    if (!is.null(label)) args$label <- label
  }
  extra <- list(...)
  if (length(extra)) args <- c(args, extra)

  raw <- do.call(genflow::gen_txt, args)
  response_text <- chatlens_coerce_text(raw)
  if (chatlens_is_error_response(raw, response_text)) {
    response_text <- NA_character_
  }

  saved_file <- NULL
  meta_file <- NULL
  if (is.list(raw) && !is.null(raw$saved_file)) {
    saved_file <- raw$saved_file
  }

  if (is.null(saved_file) && !is.null(directory) && !is.na(response_text)) {
    if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base <- if (!is.null(label) && nzchar(label)) label else "analysis"
    if (isTRUE(use_response_prefix)) {
      prefix <- cl_response_label(response_text, n = 10)
      if (!is.null(label) && nzchar(label) && isTRUE(grepl("^analysis(?:_|$)", label))) {
        suffix <- sub("^analysis", "", label)
        base <- paste0(prefix, suffix)
      } else {
        base <- prefix
      }
    }
    saved_file <- file.path(directory, paste0(base, "_", ts, ".txt"))
    writeLines(response_text, saved_file)
  }

  if (save_meta) {
    if (!is.null(saved_file) && nzchar(saved_file)) {
      meta_file <- sub("\\.txt$", meta_suffix, saved_file)
      jsonlite::write_json(raw, meta_file, auto_unbox = TRUE, pretty = TRUE)
    }
  }

  if (return == "genflow") {
    return(raw)
  }

  list(
    text = response_text,
    meta = raw,
    saved_file = saved_file,
    meta_file = meta_file
  )
}

cl_period_from_key <- function(key) {
  if (grepl("^\\d{4}$", key)) return("year")
  if (grepl("^\\d{4}-\\d{2}$", key)) return("month")
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", key)) return("day")
  if (grepl("^\\d{4}-W\\d{2}$", key)) return("week")
  NULL
}

#' Select period rows by key, period, or range selectors
#' @param periods Data frame produced by [cl_chat_split_periods()] or a `chatlens_chat`
#' @param select Optional selectors. Accepts numeric row indexes, period names,
#'   exact keys (for example `"2025-01"`), `period:key` selectors, or ranges
#'   such as `"2025-01:2025-03"`.
#' @param period Alias for `select`. If `periods` is a chat object, this
#'   controls which periods are generated before selection.
#' @param text_col Text column used when `periods` is a chat object
#' @param data_type Output type:
#'   `"default"` returns selected period rows as a data frame,
#'   `"aggregate"` returns selected rows without `period`/`key`,
#'   `"simple"` returns one-row data frame with merged `text` and without
#'   `period`/`key`.
#' @export
cl_periods_select <- function(periods,
                              select = NULL,
                              period = NULL,
                              text_col = "text_enriched",
                              data_type = c("default", "simple", "aggregate")) {
  if (!is.null(period) && is.null(select)) {
    select <- period
  }
  data_type <- match.arg(data_type)

  if (inherits(periods, "chatlens_chat")) {
    # Always generate standard period rows first, then apply selector logic
    # uniformly below (numeric, key, period, ranges, etc.).
    periods <- cl_chat_split_periods(
      periods,
      period = c("all", "year", "month", "week", "day"),
      text_col = text_col
    )
  }

  if (is.null(select)) {
    selected <- periods
  } else if (is.numeric(select)) {
    selected <- periods[select, , drop = FALSE]
  } else {
    if (!is.character(select)) stop("select must be character or numeric")

    select <- trimws(select)
    select <- select[nzchar(select)]
    if (length(select) == 0) {
      selected <- periods[0, , drop = FALSE]
    } else {
      keep <- rep(FALSE, nrow(periods))

      for (sel in select) {
        if (sel %in% c("all", "year", "month", "week", "day")) {
          keep <- keep | periods$period == sel
          next
        }

        if (grepl("^[a-z]+:", sel)) {
          parts <- strsplit(sel, ":", fixed = TRUE)[[1]]
          if (length(parts) == 2) {
            p <- parts[1]
            k <- parts[2]
            keep <- keep | (periods$period == p & periods$key == k)
            next
          }
        }

        if (grepl(":", sel, fixed = TRUE)) {
          parts <- strsplit(sel, ":", fixed = TRUE)[[1]]
          if (length(parts) == 2) {
            start <- parts[1]
            end <- parts[2]
            p <- cl_period_from_key(start)
            if (is.null(p)) p <- cl_period_from_key(end)
            if (is.null(p)) stop("Cannot infer period from selector: ", sel)
            keep <- keep | (periods$period == p & periods$key >= start & periods$key <= end)
            next
          }
        }

        p <- cl_period_from_key(sel)
        if (!is.null(p)) {
          keep <- keep | (periods$period == p & periods$key == sel)
        } else {
          keep <- keep | (periods$key == sel)
        }
      }
      selected <- periods[keep, , drop = FALSE]
    }
  }
  copy_df_attrs <- function(from, to) {
    attrs <- attributes(from)
    keep_attrs <- setdiff(names(attrs), c("names", "row.names", "class"))
    for (nm in keep_attrs) attr(to, nm) <- attrs[[nm]]
    to
  }

  strip_period_key <- function(df) {
    keep_cols <- setdiff(names(df), c("period", "key"))
    out <- df[, keep_cols, drop = FALSE]
    copy_df_attrs(df, out)
  }

  if (identical(data_type, "default")) {
    return(selected)
  }

  if (identical(data_type, "aggregate")) {
    return(strip_period_key(selected))
  }

  simple_df <- strip_period_key(selected)
  if (!"text" %in% names(simple_df)) {
    stop("periods must include a text column when data_type = 'simple'")
  }
  if (nrow(simple_df) == 0) return(simple_df)

  out <- simple_df[1, , drop = FALSE]
  out$text <- paste(as.character(simple_df$text), collapse = "\n")
  copy_df_attrs(simple_df, out)
}

# Build a safe label for saved files
cl_analysis_label <- function(period, key) {
  label <- paste0("analysis_", period, "_", key)
  label <- gsub("[^A-Za-z0-9_-]+", "_", label)
  label <- gsub("^_+|_+$", "", label)
  if (!nzchar(label)) "analysis" else label
}

cl_response_label <- function(text, n = 10) {
  if (is.na(text) || !nzchar(text)) return("analysis")
  out <- tolower(trimws(text))
  out <- gsub("\\s+", "_", out)
  out <- gsub("[^A-Za-z0-9_-]+", "_", out)
  out <- substr(out, 1, n)
  out <- gsub("^_+|_+$", "", out)
  if (!nzchar(out)) "analysis" else out
}

# Ensure analysis directory exists
cl_analysis_dir <- function(dir) {
  if (is.null(dir) || !nzchar(dir)) return(NULL)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Analyze period text
#' @param periods Data frame from [cl_chat_split_periods()], a `chatlens_chat`,
#'   or a character string with already-aggregated text.
#' @param prompt Prompt or instruction for the model
#' @param select Optional selector passed to [cl_periods_select()]
#' @param service Optional model provider passed to `genflow`
#' @param model Optional model identifier
#' @param analysis_dir Optional directory where analysis files are saved
#' @param return Either `"genflow"` (raw provider response) or `"standard"`
#' @param ... Additional arguments passed to [cl_analyze_text()]
#' @export
cl_periods_analyze <- function(periods,
                               prompt,
                               select = NULL,
                               service = NULL,
                               model = NULL,
                               analysis_dir = NULL,
                               return = c("genflow", "standard"),
                               ...) {
  if (missing(prompt) || is.null(prompt)) stop("prompt is required")

  return <- match.arg(return)

  if (is.character(periods) && is.null(select)) {
    has_parent_periods <- FALSE
    if (exists("periods", envir = parent.frame(), inherits = TRUE)) {
      parent_periods <- get("periods", envir = parent.frame(), inherits = TRUE)
      if (inherits(parent_periods, "chatlens_chat") || is.data.frame(parent_periods)) {
        has_parent_periods <- TRUE
        select <- periods
        periods <- parent_periods
      }
    }

    if (!has_parent_periods) {
      text_value <- paste(periods, collapse = "\n")
      analysis_dir <- cl_analysis_dir(analysis_dir)
      single <- cl_analyze_text(
        text = text_value,
        prompt = prompt,
        service = service,
        model = model,
        directory = analysis_dir,
        label = "analysis_text",
        return = return,
        ...
      )

      if (return == "genflow") {
        return(single)
      }

      out <- data.frame(
        period = "text",
        key = "input",
        text = text_value,
        analysis_text = single$text,
        analysis_file = single$saved_file %||% NA_character_,
        analysis_meta_file = single$meta_file %||% NA_character_,
        stringsAsFactors = FALSE
      )
      out$analysis_meta <- list(single$meta)
      return(out)
    }
  }

  if (inherits(periods, "chatlens_chat")) {
    periods <- cl_chat_split_periods(periods, period = c("all", "year", "month", "week", "day"))
  }

  if (!is.data.frame(periods)) stop("periods must be a data frame or chatlens_chat")
  if (!"text" %in% names(periods)) stop("periods must include a text column")

  periods <- cl_periods_select(periods, select = select, data_type = "default")
  if (nrow(periods) == 0) stop("No periods selected")

  default_periods <- rep("text", nrow(periods))
  default_keys <- if (nrow(periods) == 1L) "input" else sprintf("row%04d", seq_len(nrow(periods)))
  period_vals <- default_periods
  key_vals <- default_keys

  if ("period" %in% names(periods)) {
    period_vals <- as.character(periods$period)
    bad <- is.na(period_vals) | !nzchar(period_vals)
    period_vals[bad] <- default_periods[bad]
  }
  if ("key" %in% names(periods)) {
    key_vals <- as.character(periods$key)
    bad <- is.na(key_vals) | !nzchar(key_vals)
    key_vals[bad] <- default_keys[bad]
  }

  if (is.null(analysis_dir)) {
    analysis_dir <- attr(periods, "analysis_dir") %||% NULL
  }
  analysis_dir <- cl_analysis_dir(analysis_dir)

  if (return == "genflow") {
    results <- vector("list", nrow(periods))
    for (i in seq_len(nrow(periods))) {
      res <- tryCatch(
        cl_analyze_text(
          text = periods$text[i],
          prompt = prompt,
          service = service,
          model = model,
          directory = analysis_dir,
          label = cl_analysis_label(period_vals[i], key_vals[i]),
          return = "genflow",
          ...
        ),
        error = function(e) {
          warning("Analysis failed for ", period_vals[i], ": ", key_vals[i], " - ", e$message)
          list(error = e$message)
        }
      )
      results[[i]] <- res
    }
    if (length(results) == 1) return(results[[1]])
    names(results) <- paste(period_vals, key_vals, sep = ":")
    return(results)
  }

  results <- vector("list", nrow(periods))
  for (i in seq_len(nrow(periods))) {
    res <- tryCatch(
      cl_analyze_text(
        text = periods$text[i],
        prompt = prompt,
        service = service,
        model = model,
        directory = analysis_dir,
        label = cl_analysis_label(period_vals[i], key_vals[i]),
        return = "standard",
        ...
      ),
      error = function(e) {
        warning("Analysis failed for ", period_vals[i], ": ", key_vals[i], " - ", e$message)
        list(text = NA_character_, meta = list(error = e$message), saved_file = NULL, meta_file = NULL)
      }
    )
    results[[i]] <- res
  }

  periods$analysis_text <- vapply(results, function(x) x$text, FUN.VALUE = character(1))
  periods$analysis_file <- vapply(results, function(x) x$saved_file %||% NA_character_, FUN.VALUE = character(1))
  periods$analysis_meta_file <- vapply(results, function(x) x$meta_file %||% NA_character_, FUN.VALUE = character(1))
  periods$analysis_meta <- lapply(results, function(x) x$meta)
  periods
}
