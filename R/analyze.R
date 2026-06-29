# LLM analysis helpers

.clh_analysis_base_dir <- function(chat, cache_dir = NULL) {
  store_dir <- .clh_chat_store_dir_from_chat(chat, cache_dir = cache_dir)
  if (is.null(store_dir)) {
    stop("chat must have a chat_key to derive the analysis directory")
  }

  .clh_ensure_dir(file.path(store_dir, "analysis"))
}

.clh_subset_chat <- function(chat, rows) {
  out <- chat[rows, , drop = FALSE]
  attr(out, "source") <- attr(chat, "source")
  attr(out, "participants") <- attr(chat, "participants")
  attr(out, "chat_key") <- attr(chat, "chat_key")
  attr(out, "zip_id") <- attr(chat, "zip_id")
  class(out) <- class(chat)
  out
}

.clh_period_keys <- function(chat, period = c("all", "year", "month", "week", "day")) {
  period <- match.arg(period)
  ts <- chat$timestamp
  if (identical(period, "all")) return(rep("all", nrow(chat)))
  if (identical(period, "year")) return(format(ts, "%Y"))
  if (identical(period, "month")) return(format(ts, "%Y-%m"))
  if (identical(period, "week")) {
    key <- paste0(format(ts, "%G"), "-W", format(ts, "%V"))
    key[is.na(ts)] <- NA_character_
    return(key)
  }
  if (identical(period, "day")) return(format(ts, "%Y-%m-%d"))
}

.clh_select_period_keys <- function(keys, period, select = NULL) {
  if (length(keys) == 0) return(logical(0))
  if (is.null(select)) return(rep(TRUE, length(keys)))
  if (is.numeric(select)) {
    keep <- rep(FALSE, length(keys))
    keep[select[select >= 1 & select <= length(keys)]] <- TRUE
    return(keep)
  }
  if (!is.character(select)) stop("select must be NULL, numeric, or character")

  select <- trimws(select)
  select <- select[nzchar(select)]
  if (length(select) == 0) return(rep(FALSE, length(keys)))

  keep <- rep(FALSE, length(keys))
  for (sel in select) {
    if (sel %in% c("all", "year", "month", "week", "day")) {
      keep <- keep | period == sel
      next
    }

    if (grepl(":", sel, fixed = TRUE)) {
      parts <- strsplit(sel, ":", fixed = TRUE)[[1]]
      if (length(parts) == 2) {
        keep <- keep | (keys >= parts[1] & keys <= parts[2])
        next
      }
    }

    if (identical(period, "day") && grepl("^\\d{4}-\\d{2}$", sel)) {
      keep <- keep | startsWith(keys, paste0(sel, "-"))
    } else if (identical(period, "day") && grepl("^\\d{4}$", sel)) {
      keep <- keep | startsWith(keys, paste0(sel, "-"))
    } else if (identical(period, "month") && grepl("^\\d{4}$", sel)) {
      keep <- keep | startsWith(keys, paste0(sel, "-"))
    } else if (identical(period, "week") && grepl("^\\d{4}$", sel)) {
      keep <- keep | startsWith(keys, paste0(sel, "-"))
    } else {
      keep <- keep | keys == sel
    }
  }

  keep
}

.clh_analysis_item_dir <- function(base_dir, period, key) {
  if (identical(period, "all")) {
    return(file.path(base_dir, "all"))
  }

  if (identical(period, "year")) {
    return(file.path(base_dir, "by_year", .clh_path_slug(key, "unknown_year")))
  }

  if (identical(period, "month")) {
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]
    year <- .clh_or(parts[1], "unknown_year")
    month <- .clh_or(parts[2], "unknown_month")
    return(file.path(base_dir, "by_month", year, month))
  }

  if (identical(period, "day")) {
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]
    year <- .clh_or(parts[1], "unknown_year")
    month <- .clh_or(parts[2], "unknown_month")
    day <- .clh_or(parts[3], "unknown_day")
    return(file.path(base_dir, "by_day", year, month, day))
  }

  if (identical(period, "week")) {
    parts <- strsplit(key, "-W", fixed = TRUE)[[1]]
    year <- .clh_or(parts[1], "unknown_year")
    week <- .clh_or(parts[2], "unknown_week")
    return(file.path(base_dir, "by_week", year, week))
  }

  file.path(base_dir, .clh_path_slug(period, "period"), .clh_path_slug(key, "key"))
}

.clh_format_analysis_text <- function(chat, formatting, text_col = "text_enriched") {
  formatting <- match.arg(formatting, choices = c("simple", "raw"))
  if (identical(formatting, "simple")) {
    return(.clh_format_chat_simple(chat, text_col = text_col))
  }

  chat <- .clh_chat_text_snapshot(chat, text_col = text_col)
  if (!text_col %in% names(chat)) text_col <- "text"
  paste(.clh_format_messages(chat, text_col = text_col), collapse = "\n")
}

#' Prepare compact chat inputs for LLM analysis
#'
#' `cl_prepare_analysis()` is the last step before `cl_analyze_chat()`. It can
#' keep the whole chat as one input or split it into year, month, week, or day
#' inputs. With `save = TRUE`, each input is written as `input.txt` and
#' `input.rds` under the chat analysis cache.
#'
#' Default `"simple"` formatting groups repeated dates and consecutive messages
#' from the same sender. `"raw"` preserves one formatted chat line per message.
#'
#' @param chat A `chatlens_chat` object
#' @param period One of `"all"`, `"year"`, `"month"`, `"week"`, or `"day"`
#' @param select Optional period selector. Character selectors can be exact keys
#'   such as `"2020-10"` or `"2020-10-15"`, ranges such as
#'   `"2020-01:2020-03"`, or broader prefixes where supported. For example,
#'   with `period = "day"`, `"2020-10"` selects each day in October 2020.
#' @param formatting Text formatting. `"simple"` is the default; `"raw"` keeps
#'   one line per message.
#' @param save Whether to save prepared inputs
#' @param cache_dir Optional cache directory. When `NULL`, uses the chat's
#'   stored cache location or `~/.chatlens`.
#' @param text_col Text column used as transcript content
#' @return A `chatlens_analysis_input` data frame with one row per prepared
#'   input and columns for the period key, prepared text, and saved input paths.
#' @export
cl_prepare_analysis <- function(chat,
                                period = c("all", "year", "month", "week", "day"),
                                select = NULL,
                                formatting = c("simple", "raw"),
                                save = TRUE,
                                cache_dir = NULL,
                                text_col = "text_enriched") {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  period <- match.arg(period)
  formatting <- match.arg(formatting)

  base_dir <- .clh_analysis_base_dir(chat, cache_dir = cache_dir)
  keys <- .clh_period_keys(chat, period = period)
  valid <- if (identical(period, "all")) rep(TRUE, length(keys)) else !is.na(keys)
  groups <- split(seq_len(nrow(chat))[valid], keys[valid])

  if (length(groups) == 0) {
    out <- data.frame(
      period = character(0),
      key = character(0),
      text = character(0),
      input_dir = character(0),
      input_file = character(0),
      input_rds = character(0),
      stringsAsFactors = FALSE
    )
    class(out) <- c("chatlens_analysis_input", class(out))
    return(out)
  }

  group_keys <- names(groups)
  keep <- .clh_select_period_keys(group_keys, period = period, select = select)
  groups <- groups[keep]
  group_keys <- group_keys[keep]
  if (length(groups) == 0) stop("No analysis periods selected")

  rows <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    key <- group_keys[i]
    slice <- .clh_subset_chat(chat, groups[[i]])
    text <- .clh_format_analysis_text(slice, formatting = formatting, text_col = text_col)
    input_dir <- .clh_analysis_item_dir(base_dir, period = period, key = key)
    input_file <- if (save) file.path(input_dir, "input.txt") else NA_character_
    input_rds <- if (save) file.path(input_dir, "input.rds") else NA_character_

    row <- data.frame(
      period = period,
      key = key,
      text = text,
      input_dir = input_dir,
      input_file = input_file,
      input_rds = input_rds,
      stringsAsFactors = FALSE
    )

    if (save) {
      .clh_ensure_dir(input_dir)
      writeLines(text, input_file, useBytes = TRUE)
      saveRDS(row, input_rds)
    }

    rows[[i]] <- row
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("chatlens_analysis_input", class(out))
  attr(out, "analysis_dir") <- base_dir
  attr(out, "chat_key") <- attr(chat, "chat_key")
  attr(out, "period") <- period
  attr(out, "select") <- select
  attr(out, "formatting") <- formatting
  out
}

.clh_analysis_run_id <- function(service = NULL, model = NULL) {
  paste(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    .clh_path_slug(.clh_or(service, "default_service"), "default_service"),
    .clh_path_slug(.clh_or(model, "default_model"), "default_model"),
    sep = "_"
  )
}

.clh_unique_analysis_run_id <- function(dir, service = NULL, model = NULL) {
  base <- .clh_analysis_run_id(service = service, model = model)
  candidate <- base
  idx <- 2L
  while (file.exists(file.path(dir, paste0("result_", candidate, ".txt")))) {
    candidate <- paste0(base, "_", idx)
    idx <- idx + 1L
  }
  candidate
}

.clh_write_json_safe <- function(x, path) {
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' Analyze prepared chat inputs
#'
#' `cl_analyze_chat()` sends each prepared input to `genflow::gen_txt()` using
#' `prompt` plus the prepared chat text. If `prepared` is a `chatlens_chat`, it
#' is first passed through `cl_prepare_analysis()` with default settings.
#'
#' With `save = TRUE`, each prepared input directory receives timestamped
#' `prompt_*`, `result_*`, `meta_*`, and `run_*` files. The run id includes the
#' creation time, service, and model.
#'
#' @param prepared A `chatlens_analysis_input` from [cl_prepare_analysis()], or
#'   a `chatlens_chat` to prepare with default settings.
#' @param prompt Prompt or instruction for the model
#' @param service Optional model provider passed to `genflow`
#' @param model Optional model identifier
#' @param save Whether to save prompt, result, metadata, and run files
#' @param return Either `"standard"` or `"genflow"`
#' @param ... Additional arguments passed to [genflow::gen_txt()]
#' @return With `return = "standard"`, a `chatlens_analysis_result` data frame.
#'   With `return = "genflow"`, the raw `genflow` response for one input or a
#'   list of raw responses for multiple inputs.
#' @export
cl_analyze_chat <- function(prepared,
                            prompt,
                            service = NULL,
                            model = NULL,
                            save = TRUE,
                            return = c("standard", "genflow"),
                            ...) {
  if (inherits(prepared, "chatlens_chat")) {
    prepared <- cl_prepare_analysis(prepared)
  }
  if (!inherits(prepared, "chatlens_analysis_input")) {
    stop("prepared must be produced by cl_prepare_analysis()")
  }
  if (missing(prompt) || is.null(prompt)) stop("prompt is required")
  if (!requireNamespace("genflow", quietly = TRUE)) stop("genflow is required for analysis")

  return <- match.arg(return)
  raw_results <- vector("list", nrow(prepared))
  rows <- vector("list", nrow(prepared))

  for (i in seq_len(nrow(prepared))) {
    input_dir <- prepared$input_dir[i]
    if (save) .clh_ensure_dir(input_dir)
    run_id <- if (save) .clh_unique_analysis_run_id(input_dir, service = service, model = model) else NA_character_

    prompt_file <- if (save) file.path(input_dir, paste0("prompt_", run_id, ".txt")) else NA_character_
    result_file <- if (save) file.path(input_dir, paste0("result_", run_id, ".txt")) else NA_character_
    result_rds <- if (save) file.path(input_dir, paste0("result_", run_id, ".rds")) else NA_character_
    meta_file <- if (save) file.path(input_dir, paste0("meta_", run_id, ".json")) else NA_character_
    run_file <- if (save) file.path(input_dir, paste0("run_", run_id, ".json")) else NA_character_

    if (save) writeLines(prompt, prompt_file, useBytes = TRUE)

    args <- list(prompt)
    args$add <- prepared$text[i]
    if (!is.null(service)) args$service <- service
    if (!is.null(model)) args$model <- model
    extra <- list(...)
    if (length(extra)) args <- c(args, extra)

    raw <- tryCatch(
      do.call(genflow::gen_txt, args),
      error = function(e) {
        warning("Analysis failed for ", prepared$period[i], ": ", prepared$key[i], " - ", e$message, call. = FALSE)
        list(error = conditionMessage(e))
      }
    )
    raw_results[[i]] <- raw

    response_text <- .clh_coerce_text(raw)
    if (.clh_is_error_response(raw, response_text)) response_text <- NA_character_

    row <- data.frame(
      period = prepared$period[i],
      key = prepared$key[i],
      text = prepared$text[i],
      analysis_text = response_text,
      input_file = prepared$input_file[i],
      input_rds = prepared$input_rds[i],
      prompt_file = prompt_file,
      result_file = result_file,
      result_rds = result_rds,
      meta_file = meta_file,
      run_file = run_file,
      stringsAsFactors = FALSE
    )
    row$analysis_meta <- list(raw)

    if (save) {
      result_text <- response_text
      if (length(result_text) == 0 || is.na(result_text[1])) result_text <- ""
      writeLines(result_text, result_file, useBytes = TRUE)
      saveRDS(row, result_rds)
      .clh_write_json_safe(raw, meta_file)
      run <- list(
        period = prepared$period[i],
        key = prepared$key[i],
        service = service,
        model = model,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        run_id = run_id,
        formatting = .clh_or(attr(prepared, "formatting"), NA_character_),
        select = .clh_or(attr(prepared, "select"), NA_character_),
        input_file = prepared$input_file[i],
        input_rds = prepared$input_rds[i],
        prompt_file = prompt_file,
        result_file = result_file,
        result_rds = result_rds,
        meta_file = meta_file
      )
      .clh_write_json_safe(run, run_file)
    }

    rows[[i]] <- row
  }

  if (identical(return, "genflow")) {
    if (length(raw_results) == 1L) return(raw_results[[1]])
    names(raw_results) <- paste(prepared$period, prepared$key, sep = ":")
    return(raw_results)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("chatlens_analysis_result", class(out))
  out
}
