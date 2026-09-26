# Media processing: audio transcription and image description

.clh_ffmpeg_path <- function() {
  path <- Sys.which("ffmpeg")
  if (path == "") NA_character_ else path
}

.clh_convert_audio_mp3 <- function(input_path, output_path, quiet = TRUE) {
  ffmpeg <- .clh_ffmpeg_path()
  if (is.na(ffmpeg)) stop("ffmpeg not found on PATH.")
  input_path <- normalizePath(input_path, winslash = "/", mustWork = TRUE)
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  cmd <- sprintf('"%s" -y -i "%s" "%s"', ffmpeg, input_path, output_path)
  res <- system(cmd, ignore.stdout = quiet, ignore.stderr = quiet)
  if (res != 0) stop("ffmpeg conversion failed for: ", basename(input_path))
  output_path
}

.clh_manifest_sources <- function(existing, zip_id) {
  if (is.null(zip_id) || is.na(zip_id)) return(.clh_or(existing, character(0)))
  unique(c(.clh_or(existing, character(0)), zip_id))
}

.clh_attachment_id <- function(att_name, att_path, att_key = NA_character_, zip_id = NULL) {
  if (!is.na(att_key) && nzchar(att_key)) return(att_key)
  .clh_attachment_key(att_name, att_path, zip_id = zip_id)
}

.clh_resolve_chat_key <- function(chat) {
  key <- .clh_or(attr(chat, "chat_key"), attr(chat, "source")$chat_key, NULL)
  if (is.null(key) || !nzchar(key)) "chat_unknown" else key
}

.clh_resolve_store_dir <- function(chat, cache_dir = NULL) {
  chat_key <- .clh_resolve_chat_key(chat)
  if (!is.null(cache_dir)) {
    return(.clh_chat_store_dir(chat_key, cache_dir))
  }

  store_dir <- .clh_or(attr(chat, "source")$store_dir, NULL)
  if (!is.null(store_dir) && nzchar(store_dir)) {
    return(.clh_ensure_dir(store_dir))
  }
  .clh_chat_store_dir(chat_key, cache_dir)
}

.clh_format_duration <- function(seconds) {
  if (is.null(seconds) || !is.finite(seconds) || is.na(seconds)) return("calculating")
  total <- as.integer(round(max(0, as.numeric(seconds))))
  hh <- total %/% 3600L
  mm <- (total %% 3600L) %/% 60L
  ss <- total %% 60L
  if (hh > 0L) sprintf("%02d:%02d:%02d", hh, mm, ss) else sprintf("%02d:%02d", mm, ss)
}

.clh_progress_with_eta <- function(index, total, durations) {
  if (length(durations) == 0) {
    return(sprintf("(%d of %d, ETA calculating)", index, total))
  }
  remaining <- total - index + 1L
  eta <- mean(durations) * remaining
  sprintf("(%d of %d, ETA %s)", index, total, .clh_format_duration(eta))
}

.clh_estimated_remaining <- function(durations, remaining_items) {
  if (length(durations) == 0) return("calculating")
  .clh_format_duration(mean(durations) * max(0L, remaining_items))
}

.clh_resolve_image_workers <- function(workers, task_count) {
  if (task_count <= 0L) return(1L)
  if (is.null(workers)) return(min(4L, as.integer(task_count)))
  if (!is.numeric(workers) || length(workers) != 1L || is.na(workers) ||
    !is.finite(workers) || workers < 1 || workers != as.integer(workers)) {
    stop("workers must be NULL or a positive integer")
  }
  min(as.integer(workers), as.integer(task_count))
}

.clh_parallel_eta <- function(started_at, completed, total) {
  if (completed <= 0L) return("calculating")
  elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  if (!is.finite(elapsed) || elapsed <= 0) return("calculating")
  rate <- completed / elapsed
  if (!is.finite(rate) || rate <= 0) return("calculating")
  .clh_format_duration((total - completed) / rate)
}

.clh_image_batch_window <- function(workers, task_count) {
  if (task_count <= 0L) return(1L)
  if (as.integer(workers) == 1L) return(1L)
  min(as.integer(task_count), max(as.integer(workers), as.integer(workers) * 4L))
}

.clh_image_chunks <- function(task_ids, workers) {
  if (!length(task_ids)) return(list())
  workers <- min(as.integer(workers), length(task_ids))
  first <- task_ids[seq_len(workers)]
  remaining <- task_ids[-seq_len(workers)]
  if (!length(remaining)) return(list(first))
  window <- .clh_image_batch_window(workers, length(remaining))
  c(
    list(first),
    unname(split(remaining, ceiling(seq_along(remaining) / window)))
  )
}

.clh_genflow_image_batch_contract <- function() {
  list(
    get_provider = "id",
    set_agent = c("name", "setup", "content", "save", "assign"),
    gen_batch_agent = c(
      "agent", "qty", "workers", "backend", "add_img_each", "checkpoint_each",
      "persist", "verbose", "log", "always_fix_errors"
    ),
    gen_batch = c(
      "qty", "agent", "workers", "backend", "add_img_each", "checkpoint_each",
      "persist", "verbose", "log", "always_fix_errors"
    )
  )
}

.clh_genflow_image_batch_exports <- function() {
  contract <- .clh_genflow_image_batch_contract()
  out <- lapply(names(contract), function(name) {
    tryCatch(
      getExportedValue("genflow", name),
      error = function(...) NULL
    )
  })
  names(out) <- names(contract)
  out
}

.clh_genflow_image_batch_issues <- function(exports) {
  contract <- .clh_genflow_image_batch_contract()
  issues <- character(0)
  for (name in names(contract)) {
    fn <- exports[[name]]
    if (!is.function(fn)) {
      issues <- c(issues, paste0("missing exported function `", name, "()`"))
      next
    }
    missing_args <- setdiff(contract[[name]], names(formals(fn)))
    if (length(missing_args)) {
      issues <- c(
        issues,
        paste0(
          "`", name, "()` lacks explicit argument(s): ",
          paste(missing_args, collapse = ", ")
        )
      )
    }
  }
  issues
}

.clh_genflow_runtime_info <- function() {
  package_path <- tryCatch(
    find.package("genflow", quiet = TRUE),
    error = function(...) ""
  )
  if (length(package_path) != 1L || is.na(package_path)) package_path <- ""
  installed_version <- if (nzchar(package_path)) {
    tryCatch(
      as.character(read.dcf(file.path(package_path, "DESCRIPTION"), fields = "Version")[1, 1]),
      error = function(...) "unknown"
    )
  } else {
    "unknown"
  }
  list(
    loaded_version = tryCatch(
      as.character(getNamespaceVersion("genflow")),
      error = function(...) "unknown"
    ),
    installed_version = installed_version,
    package_path = if (nzchar(package_path)) package_path else "unknown"
  )
}

.clh_assert_genflow_image_batch_compatibility <- function() {
  minimum_version <- "0.0.5"
  info <- .clh_genflow_runtime_info()
  issues <- .clh_genflow_image_batch_issues(.clh_genflow_image_batch_exports())
  loaded_too_old <- tryCatch(
    utils::compareVersion(info$loaded_version, minimum_version) < 0L,
    error = function(...) TRUE
  )
  if (isTRUE(loaded_too_old)) {
    issues <- c(
      issues,
      paste0("loaded namespace version is older than ", minimum_version)
    )
  }
  if (!length(issues)) return(invisible(TRUE))

  installed_is_supported <- tryCatch(
    utils::compareVersion(info$installed_version, minimum_version) >= 0L,
    error = function(...) FALSE
  )
  restart_only <- isTRUE(installed_is_supported) &&
    !identical(info$loaded_version, "unknown") &&
    !identical(info$installed_version, "unknown") &&
    !identical(info$loaded_version, info$installed_version)
  recovery <- if (restart_only) {
    paste0(
      "The compatible genflow package on disk differs from the namespace ",
      "loaded in this R session. ",
      "Restart the R session completely and run the call again."
    )
  } else {
    paste0(
      "Install or reinstall genflow >= ", minimum_version,
      ", restart the R session completely, ",
      "and run the call again."
    )
  }

  stop(
    "The genflow namespace loaded in this R session is incompatible with ",
    "Chatlens parallel image batches. Loaded namespace version: ",
    info$loaded_version, "; installed package version: ",
    info$installed_version, "; package path: ", info$package_path, ". ",
    "Compatibility issue(s): ", paste(unique(issues), collapse = "; "), ". ",
    recovery, " Existing image descriptions are safe; this check stopped ",
    "before Chatlens read or changed the image cache.",
    call. = FALSE
  )
}

.clh_image_provider <- function(service) {
  tryCatch(
    genflow::get_provider(service),
    error = function(e) {
      stop("Unsupported image-description service: ", conditionMessage(e))
    }
  )
}

.clh_validate_image_extra <- function(extra, service = NULL) {
  provider <- .clh_image_provider(service)
  service_id <- as.character(provider$id)[1]
  is_custom <- identical(.clh_or(provider$kind, ""), "openai_compat")
  if (is_custom && !isTRUE(provider$supports_vision)) {
    stop(
      "Custom provider ", sQuote(provider$id),
      " is configured with supports_vision = FALSE and cannot describe images."
    )
  }
  if (!length(extra)) return(invisible(extra))
  extra_names <- names(extra)
  if (is.null(extra_names) || any(is.na(extra_names) | !nzchar(extra_names))) {
    stop("All additional image-description arguments in `...` must be named")
  }
  if (anyDuplicated(extra_names)) {
    stop("Additional image-description arguments in `...` must have unique names")
  }

  managed <- c(
    "context", "add_img", "directory", "label", "res_context", "persist",
    "service", "model", "type"
  )
  reserved <- intersect(extra_names, managed)
  if (length(reserved)) {
    stop(
      "These image-description arguments are managed by chatlens and cannot be supplied in `...`: ",
      paste(reserved, collapse = ", ")
    )
  }

  supported <- c(
    "add", "temp", "reasoning", "tools", "plugins", "my_tools",
    "timeout_api", "null_repeat"
  )
  unknown <- setdiff(extra_names, supported)
  if (length(unknown)) {
    stop(
      "Unsupported image-description argument(s) in `...`: ",
      paste(unknown, collapse = ", "),
      ". Supported arguments are: ", paste(supported, collapse = ", "), "."
    )
  }

  invalid <- function(argument, detail) {
    stop("Invalid image-description argument `", argument, "`: ", detail, ".")
  }
  if (!is.null(extra$temp) &&
    (!is.numeric(extra$temp) || length(extra$temp) != 1L || is.na(extra$temp) ||
      !is.finite(extra$temp))) {
    invalid("temp", "use one finite numeric value")
  }
  if (!is.null(extra$reasoning)) {
    valid_reasoning <- is.character(extra$reasoning) && length(extra$reasoning) == 1L &&
      !is.na(extra$reasoning) &&
      tolower(trimws(extra$reasoning)) %in% c("minimal", "low", "medium", "high", "xhigh")
    if (!valid_reasoning) {
      invalid("reasoning", "choose minimal, low, medium, high, or xhigh")
    }
    service_id <- tolower(trimws(as.character(.clh_or(service, ""))[1]))
    if (identical(service_id, "openai")) {
      stop(
        "`reasoning` cannot be used with `service = \"openai\"` for image description: ",
        "the current genflow OpenAI reasoning path ignores image attachments."
      )
    }
  }
  is_json_list <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
      is.list(tryCatch(
        jsonlite::fromJSON(x, simplifyVector = FALSE),
        error = function(...) NULL
      ))
  }
  if (!is.null(extra$tools)) {
    valid_tools <- (is.logical(extra$tools) && length(extra$tools) == 1L && !is.na(extra$tools)) ||
      is.list(extra$tools) || is_json_list(extra$tools)
    if (!valid_tools) invalid("tools", "use TRUE, FALSE, a list, or a JSON object")
  }
  if (!is.null(extra$plugins) && !is.list(extra$plugins) && !is_json_list(extra$plugins)) {
    invalid("plugins", "use a list or a JSON object")
  }
  if (!is.null(extra$my_tools) && !is.function(extra$my_tools) && !is.list(extra$my_tools)) {
    invalid("my_tools", "use a function or list")
  }
  if (!is.null(extra$timeout_api) &&
    (!is.numeric(extra$timeout_api) || length(extra$timeout_api) != 1L ||
      is.na(extra$timeout_api) || !is.finite(extra$timeout_api) || extra$timeout_api <= 0)) {
    invalid("timeout_api", "use one positive finite numeric value")
  }
  if (!is.null(extra$null_repeat) &&
    (!is.logical(extra$null_repeat) || length(extra$null_repeat) != 1L ||
      is.na(extra$null_repeat))) {
    invalid("null_repeat", "use TRUE or FALSE")
  }

  supports_reasoning <- if (is_custom) {
    isTRUE(provider$supports_reasoning) && !is.null(provider$reasoning_field) &&
      nzchar(as.character(provider$reasoning_field)[1])
  } else {
    service_id %in% c("openrouter", "hf")
  }
  if (!is.null(extra$reasoning) && !supports_reasoning) {
    invalid(
      "reasoning",
      paste0("service = ", sQuote(service_id), " does not apply it to image requests")
    )
  }
  supports_plugins <- if (is_custom) {
    isTRUE(provider$supports_plugins) && !is.null(provider$plugins_field) &&
      nzchar(as.character(provider$plugins_field)[1])
  } else {
    identical(service_id, "openrouter")
  }
  if (!is.null(extra$plugins) && !supports_plugins) {
    invalid(
      "plugins",
      paste0("service = ", sQuote(service_id), " does not apply them to image requests")
    )
  }
  tools_active <- !is.null(extra$tools) &&
    (!is.logical(extra$tools) || isTRUE(extra$tools))
  if (tools_active) {
    tools_services <- c(
      "openai", "openrouter", "anthropic", "nebius", "deepseek",
      "perplexity", "fireworks", "deepinfra", "hyperbolic", "cerebras",
      "together", "sambanova", "groq", "hf", "llamacpp"
    )
    supports_tools <- if (is_custom) isTRUE(provider$supports_tools) else service_id %in% tools_services
    if (!supports_tools) {
      invalid(
        "tools",
        paste0("service = ", sQuote(service_id), " does not apply them to image requests")
      )
    }
  }
  if (isTRUE(extra$tools) && is.null(extra$my_tools)) {
    invalid("tools", "provide definitions in `my_tools`, or pass the definitions as a list/JSON in `tools`")
  }
  if (!is.null(extra$my_tools) && !isTRUE(extra$tools)) {
    invalid("my_tools", "set `tools = TRUE` so the definitions are applied")
  }
  invisible(extra)
}

.clh_batch_errors_by_id <- function(stats, task_ids) {
  errors <- if (is.list(stats)) stats$detailed_errors else NULL
  if (is.null(errors)) return(stats::setNames(vector("list", length(task_ids)), task_ids))
  if (!is.list(errors)) errors <- as.list(errors)
  error_names <- names(errors)
  if (!is.null(error_names) && all(!is.na(error_names) & nzchar(error_names))) {
    if (anyDuplicated(error_names) || !all(task_ids %in% error_names)) {
      stop("Image batch returned inconsistent error identifiers; cache was left recoverable.")
    }
    return(errors[task_ids])
  }
  if (length(errors) != length(task_ids)) {
    stop("Image batch returned an inconsistent error count; cache was left recoverable.")
  }
  names(errors) <- task_ids
  errors
}

.clh_assert_image_task_id <- function(task_id, expected, source) {
  if (is.null(task_id)) return(invisible(TRUE))
  valid <- length(task_id) == 1L && !is.na(task_id) && nzchar(as.character(task_id)) &&
    identical(as.character(task_id), as.character(expected))
  if (!valid) {
    stop(
      "Image batch ", source, " task identifier does not match ", expected,
      "; cache was left recoverable."
    )
  }
  invisible(TRUE)
}

.clh_is_rate_limit_error <- function(x) {
  text <- .clh_compact_error_message(x)
  if (is.na(text)) return(FALSE)
  grepl(
    "(^|[^0-9])429([^0-9]|$)|rate[ _-]*limit|too many requests|quota[^.]{0,30}exceed",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.clh_is_fatal_provider_error <- function(x) {
  text <- .clh_compact_error_message(x)
  if (is.na(text)) return(FALSE)
  grepl(
    paste(
      c(
        "SERVICE_NOT_IMPLEMENTED", "invalid model", "not a valid model",
        "model[^.]{0,30}(not found|does not exist|unsupported)",
        "unauthori[sz]ed", "authentication", "invalid api key",
        "environment variable[^.]{0,50}not set",
        "(^|[^0-9])40[13]([^0-9]|$)", "permission denied"
      ),
      collapse = "|"
    ),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.clh_is_parallel_backend_failure <- function(error_message,
                                             raw = NULL,
                                             checkpoint = NULL) {
  text <- .clh_compact_error_message(error_message)
  if (!is.na(text) && grepl(
    paste(
      c(
        "Index\\s+[0-9]+:\\s*No (valid )?result received( or mapped)? from worker",
        "Worker(\\s+[0-9]+)? returned NULL",
        "parallel (jobs|function calls) did not deliver results",
        "No raw results received from workers"
      ),
      collapse = "|"
    ),
    text,
    ignore.case = TRUE,
    perl = TRUE
  )) {
    return(TRUE)
  }

  # A normal provider error still reaches the worker checkpoint. A NULL result
  # with no checkpoint means the parallel worker exited before it could report
  # its task outcome, even if the scheduler only supplied an index-specific
  # fallback message.
  is.null(raw) && is.null(checkpoint)
}

.clh_image_failure_signature <- function(error_message,
                                         raw = NULL,
                                         checkpoint = NULL) {
  if (.clh_is_parallel_backend_failure(error_message, raw, checkpoint)) {
    return("parallel_backend_no_task_result")
  }
  .clh_compact_error_message(error_message)
}

.clh_image_agent <- function(prompt, service, model, extra, configuration_id) {
  .clh_validate_image_extra(extra, service = service)
  setup <- c(
    list(service = service, model = model, type = "Vision"),
    extra
  )
  genflow::set_agent(
    name = paste0("chatlens_image_", substr(sub("^cfg_", "", configuration_id), 1L, 16L)),
    setup = setup,
    content = list(context = prompt),
    save = FALSE,
    assign = FALSE
  )
}

.clh_run_image_batch <- function(agent,
                                 image_paths,
                                 workers,
                                 checkpoint_paths) {
  genflow::gen_batch_agent(
    agent = agent,
    qty = length(image_paths),
    add_img_each = image_paths,
    workers = workers,
    backend = "psock",
    checkpoint_each = checkpoint_paths,
    persist = FALSE,
    verbose = FALSE,
    log = FALSE,
    always_fix_errors = FALSE
  )
}

.clh_default_image_prompt <- function() {
  "Describe the image in detail for context."
}

.clh_legacy_image_description_path <- function(desc_dir, attachment, attachment_path) {
  source <- if (!is.na(attachment_path) && nzchar(attachment_path)) {
    basename(attachment_path)
  } else {
    basename(attachment)
  }
  base <- tools::file_path_sans_ext(source)
  file.path(desc_dir, paste0(base, ".txt"))
}

.clh_normalize_prompt <- function(prompt, default_prompt) {
  if (is.null(prompt) || (is.character(prompt) && length(prompt) == 0L)) return(default_prompt)
  if (!is.character(prompt) || length(prompt) != 1L) stop("prompt must be a length-1 character string")
  prompt <- trimws(prompt)
  if (!nzchar(prompt)) stop("prompt must be a non-empty character string")
  prompt
}

.clh_unique_attachment_rows <- function(att, rows, zip_id = NULL) {
  if (length(rows) == 0) return(integer(0))
  out <- integer(0)
  seen <- character(0)
  for (r in rows) {
    att_name <- att$attachment[r]
    if (is.na(att_name) || !nzchar(att_name)) next
    att_key <- .clh_attachment_id(att_name, att$attachment_path[r], att$attachment_key[r], zip_id = zip_id)
    if (att_key %in% seen) next
    seen <- c(seen, att_key)
    out <- c(out, r)
  }
  out
}

.clh_image_targets <- function(att,
                               image_rows,
                               index,
                               image_root,
                               chat_key,
                               zip_id) {
  unique_rows <- .clh_unique_attachment_rows(att, image_rows, zip_id = zip_id)
  targets <- list()
  key_to_image <- list()

  for (r in unique_rows) {
    attachment_key <- .clh_attachment_id(
      att$attachment[r],
      att$attachment_path[r],
      att$attachment_key[r],
      zip_id = zip_id
    )
    image_id <- index$attachment_index[[attachment_key]]
    if (is.null(image_id) || !nzchar(image_id) || is.null(index$images[[image_id]])) {
      image_id <- .clh_image_id(attachment_key, att$attachment_path[r])
    }
    key_to_image[[attachment_key]] <- image_id
    index$attachment_index[[attachment_key]] <- image_id

    target <- targets[[image_id]]
    if (is.null(target)) {
      target <- list(
        image_id = image_id,
        primary_row = r,
        key_rows = list(),
        attachment_keys = character(0),
        original_names = character(0),
        source_paths = character(0),
        occurrences = list()
      )
    }
    target$attachment_keys <- .clh_image_unique_strings(c(
      target$attachment_keys,
      attachment_key
    ))
    target$key_rows[[attachment_key]] <- r
    target$original_names <- .clh_image_unique_strings(c(
      target$original_names,
      att$attachment[r]
    ))
    target$source_paths <- .clh_image_unique_strings(c(
      target$source_paths,
      att$attachment_path[r]
    ))
    targets[[image_id]] <- target
  }

  for (r in image_rows) {
    attachment_key <- .clh_attachment_id(
      att$attachment[r],
      att$attachment_path[r],
      att$attachment_key[r],
      zip_id = zip_id
    )
    image_id <- key_to_image[[attachment_key]]
    if (is.null(image_id)) next
    occurrence <- .clh_image_occurrence(
      chat_key = chat_key,
      zip_id = zip_id,
      message_id = att$message_id[r],
      timestamp = att$timestamp[r],
      attachment_key = attachment_key,
      attachment = att$attachment[r],
      occurrence_index = r
    )
    targets[[image_id]]$occurrences <- c(targets[[image_id]]$occurrences, occurrence)
  }

  changed_records <- list()
  for (image_id in names(targets)) {
    target <- targets[[image_id]]
    summary <- index$images[[image_id]]
    existing_keys <- .clh_image_unique_strings(.clh_or(summary$attachment_keys, character(0)))
    existing_names <- .clh_image_unique_strings(.clh_or(summary$original_names, character(0)))
    existing_paths <- .clh_image_unique_strings(.clh_or(summary$source_paths, character(0)))
    existing_sources <- .clh_image_unique_strings(.clh_or(summary$sources, character(0)))
    needs_update <- is.null(summary) ||
      !all(target$attachment_keys %in% existing_keys) ||
      !all(target$original_names %in% existing_names) ||
      !all(target$source_paths %in% existing_paths) ||
      (!is.null(zip_id) && !is.na(zip_id) && nzchar(zip_id) && !zip_id %in% existing_sources)

    if (needs_update) {
      record <- .clh_image_record_load(image_root, image_id, fallback = summary)
      record <- .clh_image_record_add_context(
        record,
        attachment_keys = target$attachment_keys,
        original_names = target$original_names,
        source_paths = target$source_paths,
        sources = zip_id,
        occurrences = target$occurrences
      )
      changed_records[[image_id]] <- record
    }
  }

  list(targets = targets, index = index, changed_records = changed_records)
}

.clh_image_flat_already_migrated <- function(image, flat_path) {
  if (is.null(image) || !file.exists(flat_path) || length(image$descriptions) == 0L) {
    return(FALSE)
  }
  flat_md5 <- unname(tools::md5sum(flat_path))
  any(vapply(image$descriptions, function(description) {
    .clh_or(description$recovered_from, "") %in% c("legacy_flat_file", "legacy_manifest") &&
      identical(description$legacy_file, flat_path) &&
      identical(description$legacy_md5, flat_md5)
  }, FUN.VALUE = logical(1)))
}

#' Transcribe audio attachments
#' @param chat A `chatlens_chat` object
#' @param service Provider name for speech-to-text
#' @param model Model id for speech-to-text
#' @param cache_dir Optional cache directory. When `NULL`, uses the chat's
#'   stored cache location or `~/.chatlens`.
#' @param overwrite Reprocess items even if cached output exists
#' @param verbose Whether to emit progress messages
#' @param save_chat Save the current chat state in cache as `chat.rds` plus a
#'   mirrored `chat.txt` transcript
#' @param ... Additional arguments passed to [genflow::gen_stt()]
#' @details
#' The progress total includes only unique audio files that are available after
#' import, plus reusable cached transcripts. Audio references already marked as
#' missing by [cl_whatsapp_import()] are recorded in the manifest and run log,
#' but are not sent to the transcription provider. A defensive check still
#' warns if a file marked as present disappears after import.
#' @export
cl_chat_transcribe_audio <- function(chat,
                                      service = "replicate",
                                      model = "openai/whisper",
                                      cache_dir = NULL,
                                      overwrite = FALSE,
                                      verbose = TRUE,
                                      save_chat = TRUE,
                                      ...) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  if (!requireNamespace("genflow", quietly = TRUE)) stop("genflow is required for transcription")

  chat_key <- .clh_resolve_chat_key(chat)
  zip_id <- attr(chat, "zip_id")
  cache_base <- .clh_resolve_store_dir(chat, cache_dir)
  attr(chat, "chat_key") <- chat_key
  audio_dir <- .clh_ensure_dir(file.path(cache_base, "audio"))
  transcript_dir <- .clh_ensure_dir(file.path(cache_base, "audio_transcripts"))
  manifest_path <- file.path(cache_base, "audio_manifest.json")
  manifest <- .clh_manifest_load(manifest_path)
  if (is.null(manifest$items)) manifest$items <- list()
  run_items <- list()
  save_current_chat <- function(chat) {
    if (save_chat) .clh_save_current_chat(chat, cache_dir = cache_dir) else chat
  }

  att <- .clh_attachments(chat)
  if (nrow(att) == 0) {
    warning("No attachments found.")
    chat$audio_transcripts <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$audio_transcript <- rep(NA_character_, nrow(chat))
    return(save_current_chat(chat))
  }

  if (any(is.na(att$attachment_type))) {
    idx <- which(is.na(att$attachment_type))
    att$attachment_type[idx] <- vapply(att$attachment[idx], .clh_attachment_type_from_name, FUN.VALUE = character(1))
  }

  audio_rows <- which(att$attachment_type == "audio")
  audio_rows <- .clh_unique_attachment_rows(att, audio_rows, zip_id = zip_id)
  if (length(audio_rows) == 0) {
    warning("No audio attachments found.")
    chat$audio_transcripts <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$audio_transcript <- rep(NA_character_, nrow(chat))
    return(save_current_chat(chat))
  }

  detected_audio_rows <- audio_rows
  audio_keys <- vapply(detected_audio_rows, function(r) {
    .clh_attachment_id(
      att$attachment[r],
      att$attachment_path[r],
      att$attachment_key[r],
      zip_id = zip_id
    )
  }, FUN.VALUE = character(1))
  file_available <- vapply(detected_audio_rows, function(r) {
    path <- att$attachment_path[r]
    !is.na(path) && nzchar(path) && file.exists(path)
  }, FUN.VALUE = logical(1))
  cache_reusable <- vapply(seq_along(detected_audio_rows), function(i) {
    if (overwrite) return(FALSE)
    existing <- manifest$items[[audio_keys[i]]]
    !is.null(existing) &&
      identical(existing$status, "processed") &&
      identical(existing$service, service) &&
      identical(existing$model, model) &&
      !is.null(existing$transcript) &&
      length(existing$transcript) == 1L &&
      !is.na(existing$transcript) &&
      nzchar(existing$transcript)
  }, FUN.VALUE = logical(1))

  unavailable <- !file_available & !cache_reusable
  unavailable_rows <- detected_audio_rows[unavailable]
  audio_rows <- detected_audio_rows[!unavailable]

  if (verbose) {
    total <- length(detected_audio_rows)
    available_count <- sum(file_available)
    missing_count <- sum(unavailable)
    cached_only_count <- sum(!file_available & cache_reusable)
    message(
      "Audio preflight: ",
      total,
      if (total == 1L) " unique audio reference; " else " unique audio references; ",
      available_count,
      if (available_count == 1L) " file available" else " files available",
      if (cached_only_count > 0L) {
        paste0(
          "; ", cached_only_count,
          if (cached_only_count == 1L) " cached transcript reusable" else " cached transcripts reusable"
        )
      } else {
        ""
      },
      if (missing_count > 0L) {
        paste0(
          "; ", missing_count,
          if (missing_count == 1L) " missing reference skipped" else " missing references skipped"
        )
      } else {
        ""
      },
      "."
    )
  }

  transcript_map <- list()
  audio_durations <- numeric(0)

  for (r in unavailable_rows) {
    att_name <- att$attachment[r]
    att_path <- att$attachment_path[r]
    att_key <- .clh_attachment_id(att_name, att_path, att$attachment_key[r], zip_id = zip_id)
    existing <- manifest$items[[att_key]]
    import_status <- att$attachment_status[r]
    known_at_import <- !is.na(import_status) && import_status %in% c("missing", "placeholder", "omitted")
    missing_reason <- if (known_at_import) "missing_at_import" else "missing_after_import"

    if (!known_at_import) {
      warning("Audio file became unavailable after import: ", att_name, call. = FALSE)
    }
    if (verbose) {
      message(
        if (known_at_import) "Skipping audio reference marked unavailable during import: " else "Skipping audio file unavailable after import: ",
        att_name
      )
    }

    entry <- list(
      attachment_key = att_key,
      attachment = att_name,
      status = "missing",
      missing_reason = missing_reason,
      attachment_status = import_status,
      service = service,
      model = model,
      processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      transcript = NA_character_,
      sources = .clh_manifest_sources(existing$sources, zip_id)
    )
    manifest$items[[att_key]] <- entry
    run_items[[att_key]] <- entry
    manifest <- .clh_manifest_checkpoint(manifest, manifest_path)
  }

  for (idx in seq_along(audio_rows)) {
    r <- audio_rows[idx]
    att_name <- att$attachment[r]
    att_path <- att$attachment_path[r]
    att_key <- .clh_attachment_id(att_name, att_path, att$attachment_key[r], zip_id = zip_id)
    progress <- .clh_progress_with_eta(idx, length(audio_rows), audio_durations)

    existing <- manifest$items[[att_key]]
    if (!overwrite && !is.null(existing) &&
      identical(existing$status, "processed") &&
      identical(existing$service, service) &&
      identical(existing$model, model) &&
      !is.null(existing$transcript) && nzchar(existing$transcript)) {
      transcript_map[[att_key]] <- existing$transcript
      if (verbose) message("Skipping (already processed) ", progress, ": ", att_name)
      run_items[[att_key]] <- c(existing, list(reused = TRUE))
      next
    }

    if (is.na(att_path) || !nzchar(att_path) || !file.exists(att_path)) {
      warning("Audio file became unavailable after preflight: ", att_name, call. = FALSE)
      if (verbose) message("Skipping (audio file became unavailable) ", progress, ": ", att_name)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "missing",
        missing_reason = "missing_after_preflight",
        attachment_status = att$attachment_status[r],
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        transcript = NA_character_,
        sources = .clh_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
      manifest <- .clh_manifest_checkpoint(manifest, manifest_path)
      next
    }

    base <- tools::file_path_sans_ext(basename(att_path))
    transcript_path <- file.path(transcript_dir, paste0(base, ".txt"))

    if (!overwrite && file.exists(transcript_path)) {
      cached_text <- paste(readLines(transcript_path, warn = FALSE), collapse = "\n")
      if (!.clh_is_error_response(NULL, cached_text)) {
        if (verbose) message("Using cached transcript ", progress, ": ", basename(transcript_path))
        transcript_map[[att_key]] <- cached_text
        entry <- list(
          attachment_key = att_key,
          attachment = att_name,
          status = "processed",
          service = service,
          model = model,
          processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          transcript = cached_text,
          saved_file = transcript_path,
          sources = .clh_manifest_sources(existing$sources, zip_id)
        )
        manifest$items[[att_key]] <- entry
        run_items[[att_key]] <- entry
        manifest <- .clh_manifest_checkpoint(manifest, manifest_path)
        next
      } else if (verbose) {
        message("Cached transcript looks like an error; reprocessing ", progress, ": ", basename(transcript_path))
      }
    }

    ext <- tolower(tools::file_ext(att_path))
    audio_path <- att_path

    if (ext != "mp3") {
      audio_path <- file.path(audio_dir, paste0(base, ".mp3"))
      if (!file.exists(audio_path) || overwrite) {
        if (verbose) message("Converting audio ", progress, ": ", basename(att_path), " -> ", basename(audio_path))
        audio_path <- .clh_convert_audio_mp3(att_path, audio_path)
      }
    }

    if (verbose) message("Transcribing audio ", progress, ": ", basename(audio_path))

    args <- list(audio_path)
    if (!is.null(service)) args$service <- service
    if (!is.null(model)) args$model <- model
    extra <- list(...)
    if (length(extra)) args <- c(args, extra)

    started_at <- Sys.time()
    error_message <- NA_character_
    raw <- tryCatch(
      do.call(genflow::gen_stt, args),
      error = function(e) {
        error_message <<- conditionMessage(e)
        warning("Transcription failed for ", basename(att_path), ": ", error_message, call. = FALSE)
        NA_character_
      }
    )
    elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    audio_durations <- c(audio_durations, elapsed)

    transcript <- .clh_coerce_text(raw)

    if (!.clh_is_error_response(raw, transcript)) {
      transcript_map[[att_key]] <- transcript
      writeLines(transcript, transcript_path)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "processed",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        transcript = transcript,
        response = raw,
        saved_file = transcript_path,
        sources = .clh_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
    } else {
      if (is.na(error_message) || !nzchar(error_message)) {
        error_message <- .clh_error_response_message(raw, transcript)
      }
      if (verbose) {
        message(
          "Transcription error recorded ",
          progress,
          ": ",
          att_name,
          " [",
          .clh_or(service, "default-service"),
          "/",
          .clh_or(model, "default-model"),
          "] - ",
          error_message
        )
      }
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "error",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        transcript = NA_character_,
        error_message = error_message,
        response = raw,
        sources = .clh_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
    }

    manifest <- .clh_manifest_checkpoint(manifest, manifest_path)

    if (verbose) {
      eta_left <- .clh_estimated_remaining(audio_durations, length(audio_rows) - idx)
      if (length(audio_durations) == 1L) {
        message(
          "First audio transcription took ",
          .clh_format_duration(elapsed),
          ". Estimated remaining: ",
          eta_left,
          "."
        )
      } else {
        message(
          "Audio ",
          idx,
          "/",
          length(audio_rows),
          " completed in ",
          .clh_format_duration(elapsed),
          ". Estimated remaining: ",
          eta_left,
          "."
        )
      }
    }
  }

  manifest <- .clh_manifest_checkpoint(manifest, manifest_path)

  zip_name <- .clh_or(attr(chat, "source")$path, NULL)
  run_path <- .clh_run_log_path(chat_key, zip_id, kind = "audio", zip_name = zip_name, cache_dir = cache_dir)
  if (!is.null(run_path)) {
    run_log <- list(
      chat_key = chat_key,
      zip_id = zip_id,
      run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      items = run_items
    )
    jsonlite::write_json(run_log, run_path, auto_unbox = TRUE, pretty = TRUE)
    if (verbose) message("Saved audio run log: ", run_path)
  }

  # Build per-message transcripts
  if (!"attachments" %in% names(chat)) {
    chat$attachments <- lapply(chat$attachment, function(x) if (is.na(x)) character(0) else x)
  }
  if (!"attachment_keys" %in% names(chat)) {
    if ("attachment_paths" %in% names(chat)) {
      path_list <- chat$attachment_paths
    } else {
      path_list <- lapply(chat$attachments, function(x) rep(NA_character_, length(x)))
    }
    chat$attachment_keys <- mapply(function(att_names, att_paths) {
      if (length(att_names) == 0) return(character(0))
      if (length(att_paths) != length(att_names)) att_paths <- rep(NA_character_, length(att_names))
      vapply(seq_along(att_names), function(i) {
        .clh_attachment_key(att_names[i], att_paths[i], zip_id = zip_id)
      }, FUN.VALUE = character(1))
    }, chat$attachments, path_list, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  chat$audio_transcripts <- mapply(function(att_names, att_keys) {
    if (length(att_names) == 0) return(character(0))
    if (length(att_keys) != length(att_names)) att_keys <- rep(NA_character_, length(att_names))
    vals <- vapply(seq_along(att_names), function(i) {
      att_key <- .clh_attachment_id(att_names[i], NA_character_, att_keys[i], zip_id = zip_id)
      if (!is.null(transcript_map[[att_key]])) return(transcript_map[[att_key]])
      existing <- manifest$items[[att_key]]
      if (!is.null(existing) && !is.null(existing$transcript)) return(existing$transcript)
      NA_character_
    }, FUN.VALUE = character(1))
    names(vals) <- att_names
    vals <- vals[!is.na(vals)]
    vals
  }, chat$attachments, chat$attachment_keys, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  chat$audio_transcript <- vapply(chat$audio_transcripts, function(x) {
    if (length(x) == 0) return(NA_character_)
    if (length(x) == 1) return(unname(x[1]))
    paste(paste0("[", names(x), "] ", x), collapse = "\n")
  }, FUN.VALUE = character(1))

  save_current_chat(chat)
}

#' Describe image attachments
#' @param chat A `chatlens_chat` object
#' @param prompt Prompt used to describe each image
#' @param service Provider name for image description
#' @param model Model id for image description
#' @param cache_dir Optional cache directory. When `NULL`, uses the chat's
#'   stored cache location or `~/.chatlens`.
#' @param overwrite Create a new description for every image even when a valid
#'   cached description exists. Previous descriptions are preserved as separate
#'   versioned artifacts. This is a compatibility shortcut for
#'   `cache_mode = "force"`; `FALSE` maps to `cache_mode = "missing"`.
#' @param cache_mode Optional cache policy. `"missing"` reuses any active valid
#'   description and processes only undescribed images. `"configuration"`
#'   reuses descriptions made with the requested prompt, service, model, and
#'   additional arguments, processing only images missing that exact
#'   configuration. `"force"` creates a new version for every image. When
#'   `NULL`, the policy is derived from `overwrite`.
#' @param workers Maximum number of image requests to run simultaneously.
#'   `NULL` uses up to four workers. Explicit values are capped by the number
#'   of pending images; higher values may trigger provider rate limits. Parallel
#'   provider calls run in independent PSOCK processes rather than forked copies
#'   of the current R session.
#' @param verbose Whether to emit progress messages
#' @param save_chat Save the current chat state in cache as `chat.rds` plus a
#'   mirrored `chat.txt` transcript
#' @param ... Supported [genflow::gen_txt()] runtime arguments: `add`, `temp`,
#'   `reasoning`, `tools`, `plugins`, `my_tools`, `timeout_api`, and
#'   `null_repeat`. Unknown names fail before cache metadata is created so a
#'   configuration never claims to have used an ignored option. Provider-bound
#'   options are rejected when the selected service is known to ignore them;
#'   for example, `plugins` are currently limited to OpenRouter among built-in
#'   providers. Custom-provider capability flags are honored; reasoning and
#'   plugin support also require their configured payload field. `tools = TRUE`
#'   requires definitions in `my_tools`; definitions can instead be supplied
#'   directly as a list/JSON value in `tools`.
#' @details
#' Before reading or changing the image cache, the function verifies that the
#' live `genflow` namespace supports the required parallel batch arguments. If
#' `genflow` was updated while the current R session was open, the error reports
#' both the loaded and installed versions and asks for a complete session
#' restart instead of creating failed image attempts.
#'
#' Parallel requests explicitly use genflow's PSOCK backend. This avoids
#' inheriting initialized `curl`/`httr` native state from RStudio, which can make
#' forked workers crash before returning a result or writing a checkpoint.
#'
#' Each image has its own `image.json` metadata file and a `descriptions/`
#' directory containing one `.json` provenance file per attempt plus a `.txt`
#' result for each successful attempt. A global `image_manifest.json` indexes
#' those records. All artifacts are written atomically and checkpointed after
#' each attempt. The first concurrent group contains at most one task per
#' worker, providing an early circuit breaker for uniform provider or
#' configuration failures. When enough tasks remain, later images are processed
#' in bounded queue windows with more queued tasks than workers, so fast workers
#' do not wait for the slowest request after every small group. Serial runs
#' commit metadata after every image. Only the main R process writes image
#' metadata and the global manifest. Workers also write a unique per-attempt
#' checkpoint so completed calls can be recovered if a batch is interrupted.
#' The global index can be rebuilt from the per-image metadata after an
#' interruption. Flat `.txt`
#' files and the legacy root image manifest are migrated automatically without
#' deleting the originals. The returned chat stores the selected text in
#' `image_description`/`image_descriptions` and its artifact id in
#' `image_description_id`/`image_description_ids`.
#' @export
cl_chat_describe_images <- function(chat,
                                     prompt = "Describe the image in detail for context.",
                                     service = "openrouter",
                                     model = "google/gemini-3-flash-preview",
                                     cache_dir = NULL,
                                     overwrite = FALSE,
                                     verbose = TRUE,
                                     save_chat = TRUE,
                                     cache_mode = NULL,
                                     workers = NULL,
                                     ...) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  if (!requireNamespace("genflow", quietly = TRUE)) stop("genflow is required for image description")
  .clh_assert_genflow_image_batch_compatibility()
  if (!is.character(service) || length(service) != 1L || is.na(service) || !nzchar(trimws(service))) {
    stop("service must be one non-empty character value")
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(trimws(model))) {
    stop("model must be one non-empty character value")
  }
  service <- as.character(.clh_image_provider(service)$id)[1]
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE")
  }
  if (is.null(cache_mode)) {
    cache_mode <- if (overwrite) "force" else "missing"
  } else {
    cache_mode <- match.arg(cache_mode, c("missing", "configuration", "force"))
    if (overwrite && !identical(cache_mode, "force")) {
      stop("overwrite = TRUE conflicts with cache_mode = ", sQuote(cache_mode))
    }
  }
  prompt <- .clh_normalize_prompt(prompt, .clh_default_image_prompt())
  extra <- list(...)
  .clh_validate_image_extra(extra, service = service)

  chat_key <- .clh_resolve_chat_key(chat)
  zip_id <- attr(chat, "zip_id")
  cache_base <- .clh_resolve_store_dir(chat, cache_dir)
  attr(chat, "chat_key") <- chat_key
  image_root <- .clh_image_cache_root(cache_base)
  index <- .clh_image_index_load(image_root)
  run_items <- list()
  save_current_chat <- function(chat) {
    if (save_chat) .clh_save_current_chat(chat, cache_dir = cache_dir) else chat
  }

  att <- .clh_attachments(chat)
  if (nrow(att) == 0) {
    warning("No attachments found.")
    chat$image_descriptions <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$image_description <- rep(NA_character_, nrow(chat))
    chat$image_description_ids <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$image_description_id <- rep(NA_character_, nrow(chat))
    return(save_current_chat(chat))
  }

  if (any(is.na(att$attachment_type))) {
    idx <- which(is.na(att$attachment_type))
    att$attachment_type[idx] <- vapply(att$attachment[idx], .clh_attachment_type_from_name, FUN.VALUE = character(1))
  }

  image_rows <- which(att$attachment_type == "image")
  if (length(image_rows) == 0) {
    warning("No image attachments found.")
    chat$image_descriptions <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$image_description <- rep(NA_character_, nrow(chat))
    chat$image_description_ids <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$image_description_id <- rep(NA_character_, nrow(chat))
    return(save_current_chat(chat))
  }

  prepared_targets <- .clh_image_targets(
    att = att,
    image_rows = image_rows,
    index = index,
    image_root = image_root,
    chat_key = chat_key,
    zip_id = zip_id
  )
  targets <- prepared_targets$targets
  index <- prepared_targets$index
  changed_records <- prepared_targets$changed_records

  processing_ids <- names(targets)[vapply(names(targets), function(image_id) {
    descriptions <- index$images[[image_id]]$descriptions
    length(descriptions) > 0L && any(vapply(descriptions, function(description) {
      identical(description$status, "processing")
    }, FUN.VALUE = logical(1)))
  }, FUN.VALUE = logical(1))]
  if (length(processing_ids)) {
    .clh_image_mark_dirty(image_root)
    for (image_id in processing_ids) {
      record <- changed_records[[image_id]]
      if (is.null(record)) {
        record <- .clh_image_record_load(
          image_root,
          image_id,
          fallback = index$images[[image_id]]
        )
      }
      changed_records[[image_id]] <- .clh_image_reconcile_record(
        record,
        image_root
      )$record
    }
  }

  configuration <- .clh_image_configuration(prompt, service, model, extra = extra)
  index <- .clh_image_index_register_configuration(index, configuration)

  legacy_manifest_path <- file.path(cache_base, "image_manifest.json")
  legacy_manifest_md5 <- if (file.exists(legacy_manifest_path)) {
    unname(tools::md5sum(legacy_manifest_path))
  } else {
    NA_character_
  }
  migrated_manifest_md5 <- .clh_or(index$migrations$legacy_image_manifest$md5, NA_character_)
  legacy_manifest <- if (!is.na(legacy_manifest_md5) &&
    !identical(legacy_manifest_md5, migrated_manifest_md5)) {
    .clh_manifest_load(legacy_manifest_path)
  } else {
    list(items = list())
  }
  legacy_items <- .clh_or(legacy_manifest$items, list())

  flat_owners <- list()
  for (image_id in names(targets)) {
    target <- targets[[image_id]]
    for (attachment_key in target$attachment_keys) {
      r <- target$key_rows[[attachment_key]]
      flat_path <- .clh_legacy_image_description_path(
        image_root,
        att$attachment[r],
        att$attachment_path[r]
      )
      owners <- .clh_or(flat_owners[[flat_path]], character(0))
      flat_owners[[flat_path]] <- unique(c(owners, image_id))
    }
  }

  flat_candidates <- vapply(names(flat_owners), function(flat_path) {
    owners <- flat_owners[[flat_path]]
    file.exists(flat_path) && length(owners) == 1L &&
      !.clh_image_flat_already_migrated(index$images[[owners]], flat_path)
  }, FUN.VALUE = logical(1))
  legacy_candidates <- length(legacy_items) > 0L || any(flat_candidates)
  if (legacy_candidates) .clh_image_mark_dirty(image_root)
  migrated_count <- 0L

  for (image_id in names(targets)) {
    target <- targets[[image_id]]
    record <- changed_records[[image_id]]
    record_changed <- !is.null(changed_records[[image_id]])
    consumed_flat_paths <- character(0)

    migration_keys <- target$attachment_keys[vapply(target$attachment_keys, function(attachment_key) {
      r <- target$key_rows[[attachment_key]]
      flat_path <- .clh_legacy_image_description_path(
        image_root,
        att$attachment[r],
        att$attachment_path[r]
      )
      flat_already_migrated <- .clh_image_flat_already_migrated(
        index$images[[image_id]],
        flat_path
      )
      !is.null(legacy_items[[attachment_key]]) ||
        (file.exists(flat_path) &&
          length(.clh_or(flat_owners[[flat_path]], character(0))) == 1L &&
          !flat_already_migrated)
    }, FUN.VALUE = logical(1))]

    if (!record_changed && length(migration_keys) == 0L) next
    if (is.null(record)) {
      record <- .clh_image_record_load(
        image_root,
        image_id,
        fallback = index$images[[image_id]]
      )
    }

    for (attachment_key in migration_keys) {
      r <- target$key_rows[[attachment_key]]
      flat_path <- .clh_legacy_image_description_path(
        image_root,
        att$attachment[r],
        att$attachment_path[r]
      )
      legacy_entry <- legacy_items[[attachment_key]]
      flat_is_unambiguous <- length(.clh_or(flat_owners[[flat_path]], character(0))) == 1L
      recover_flat <- is.null(legacy_entry) && file.exists(flat_path) &&
        flat_is_unambiguous && !flat_path %in% consumed_flat_paths

      if (!is.null(legacy_entry) || recover_flat) {
        migrated <- .clh_image_migrate_legacy_item(
          record = record,
          image_root = image_root,
          attachment_key = attachment_key,
          attachment = att$attachment[r],
          zip_id = zip_id,
          entry = legacy_entry,
          flat_path = if (file.exists(flat_path)) flat_path else NA_character_
        )
        record <- migrated$record
        if (!is.null(migrated$configuration)) {
          index <- .clh_image_index_register_configuration(index, migrated$configuration)
        }
        if (isTRUE(migrated$migrated)) {
          migrated_count <- migrated_count + 1L
          record_changed <- TRUE
        }
        consumed_flat_paths <- unique(c(consumed_flat_paths, flat_path))
      }
    }

    if (record_changed) changed_records[[image_id]] <- record
  }

  if (!is.na(legacy_manifest_md5) &&
    !identical(legacy_manifest_md5, migrated_manifest_md5)) {
    index$migrations$legacy_image_manifest <- list(
      md5 = legacy_manifest_md5,
      migrated_at = .clh_image_now()
    )
  }

  if (length(changed_records)) {
    index <- .clh_image_commit_records(changed_records, index, image_root)
  } else {
    .clh_image_mark_dirty(image_root)
    index$updated_at <- .clh_image_now()
    .clh_manifest_save(index, .clh_image_index_path(image_root))
    dirty_path <- .clh_image_dirty_path(image_root)
    if (file.exists(dirty_path)) unlink(dirty_path)
  }

  if (identical(cache_mode, "configuration")) {
    activation_records <- list()
    for (image_id in names(targets)) {
      configured <- .clh_image_index_configuration(
        index,
        image_id,
        configuration$configuration_id,
        image_root
      )
      if (is.null(configured) ||
        identical(index$images[[image_id]]$active_description_id, configured$id)) {
        next
      }
      record <- .clh_image_record_load(
        image_root,
        image_id,
        fallback = index$images[[image_id]]
      )
      record$active_description_id <- configured$id
      activation_records[[image_id]] <- record
    }
    if (length(activation_records)) {
      index <- .clh_image_commit_records(activation_records, index, image_root)
    }
  }

  if (verbose && migrated_count > 0L) {
    message(
      "Migrated ",
      migrated_count,
      if (migrated_count == 1L) " legacy image description" else " legacy image descriptions",
      " into the versioned cache."
    )
  }

  active_at_start <- lapply(names(targets), function(image_id) {
    .clh_image_index_active(index, image_id, image_root)
  })
  names(active_at_start) <- names(targets)
  cached_descriptions <- if (identical(cache_mode, "configuration")) {
    out <- lapply(names(targets), function(image_id) {
      .clh_image_index_configuration(
        index,
        image_id,
        configuration$configuration_id,
        image_root
      )
    })
    names(out) <- names(targets)
    out
  } else if (identical(cache_mode, "missing")) {
    active_at_start
  } else {
    stats::setNames(vector("list", length(targets)), names(targets))
  }
  cached_images <- !vapply(cached_descriptions, is.null, FUN.VALUE = logical(1))
  existing_count <- sum(!vapply(active_at_start, is.null, FUN.VALUE = logical(1)))
  if (verbose) {
    cached_count <- sum(cached_images)
    if (identical(cache_mode, "force")) {
      message(
        "Image cache check: cache_mode = 'force'; ",
        length(targets),
        if (length(targets) == 1L) " image will receive a new description" else " images will receive new descriptions",
        if (existing_count > 0L) paste0("; ", existing_count, " existing active description(s) will be preserved") else "",
        "."
      )
    } else if (identical(cache_mode, "configuration")) {
      remaining <- length(targets) - cached_count
      message(
        "Image configuration check: ",
        cached_count,
        if (cached_count == 1L) " matching description will be reused; " else " matching descriptions will be reused; ",
        remaining,
        if (remaining == 1L) " image remains to process." else " images remain to process."
      )
    } else {
      remaining <- length(targets) - cached_count
      message(
        "Image resume check: ",
        cached_count,
        if (cached_count == 1L) " cached description will be reused; " else " cached descriptions will be reused; ",
        remaining,
        if (remaining == 1L) " image remains to process." else " images remain to process."
      )
    }
  }

  description_map <- list()
  selected_active <- active_at_start
  pending <- list()
  workers_used <- 0L
  batch_count <- 0L

  target_ids <- names(targets)
  for (idx in seq_along(target_ids)) {
    image_id <- target_ids[idx]
    target <- targets[[image_id]]
    r <- target$primary_row
    att_name <- att$attachment[r]
    att_path <- att$attachment_path[r]
    att_key <- .clh_attachment_id(att_name, att_path, att$attachment_key[r], zip_id = zip_id)
    progress <- sprintf("(%d of %d)", idx, length(targets))
    active <- active_at_start[[image_id]]
    cached <- cached_descriptions[[image_id]]

    if (!is.null(cached)) {
      selected_active[[image_id]] <- cached
      for (key in target$attachment_keys) description_map[[key]] <- cached$text
      if (verbose) message("Reusing cached image description ", progress, ": ", att_name)
      run_entry <- cached$description
      run_entry$image_id <- image_id
      run_entry$description <- cached$text
      run_entry$reused <- TRUE
      run_entry$requested_service <- service
      run_entry$requested_model <- model
      run_entry$requested_prompt <- prompt
      for (key in target$attachment_keys) run_items[[key]] <- run_entry
      next
    }

    if (is.na(att_path) || !nzchar(att_path) || !file.exists(att_path)) {
      warning("Image file missing: ", att_name, call. = FALSE)
      if (verbose) message("Skipping (missing image file) ", progress, ": ", att_name)
      record <- .clh_image_record_load(
        image_root,
        image_id,
        fallback = index$images[[image_id]]
      )
      metadata <- .clh_image_attempt_metadata(
        record,
        configuration,
        image_root,
        attachment_key = att_key,
        attachment = att_name,
        zip_id = zip_id,
        status = "missing"
      )
      metadata$processed_at <- .clh_image_now()
      metadata$error_message <- "Image file is missing from the imported export."
      committed <- .clh_image_commit_attempt(
        record,
        metadata,
        index,
        image_root
      )
      index <- committed$index
      if (!is.null(active)) {
        for (key in target$attachment_keys) description_map[[key]] <- active$text
      }
      entry <- metadata
      entry$image_id <- image_id
      for (key in target$attachment_keys) run_items[[key]] <- entry
      next
    }

    pending[[image_id]] <- list(
      image_id = image_id,
      target = target,
      attachment = att_name,
      attachment_path = att_path,
      attachment_key = att_key,
      active = active
    )
  }

  if (length(pending)) {
    workers_used <- .clh_resolve_image_workers(workers, length(pending))
    pending_ids <- names(pending)
    chunks <- .clh_image_chunks(pending_ids, workers_used)
    batch_count <- length(chunks)
    agent <- .clh_image_agent(
      prompt = prompt,
      service = service,
      model = model,
      extra = extra,
      configuration_id = configuration$configuration_id
    )
    provider_started <- Sys.time()
    completed_count <- 0L

    if (verbose) {
      message(
        "Image provider queue: ",
        length(pending),
        if (length(pending) == 1L) " pending image, " else " pending images, ",
        workers_used,
        if (workers_used == 1L) " worker, " else " workers, ",
        batch_count,
        if (batch_count == 1L) " batch." else " batches."
      )
    }

    for (batch_index in seq_along(chunks)) {
      chunk_ids <- chunks[[batch_index]]
      staged_records <- list()
      staged_items <- list()
      .clh_image_mark_dirty(image_root)

      for (image_id in chunk_ids) {
        item <- pending[[image_id]]
        record <- .clh_image_record_load(
          image_root,
          image_id,
          fallback = index$images[[image_id]]
        )
        metadata <- .clh_image_attempt_metadata(
          record,
          configuration,
          image_root,
          attachment_key = item$attachment_key,
          attachment = item$attachment,
          zip_id = zip_id,
          status = "processing"
        )
        checkpoint_path <- file.path(
          .clh_image_description_dir(image_root, image_id),
          paste0(metadata$description_id, ".result.rds")
        )
        metadata$checkpoint_file <- .clh_image_relative_path(image_root, checkpoint_path)
        record <- .clh_image_stage_attempt(record, metadata, image_root)
        staged_records[[image_id]] <- record
        staged_items[[image_id]] <- c(
          item,
          list(
            record = record,
            metadata = metadata,
            checkpoint_path = checkpoint_path
          )
        )
      }
      index <- .clh_image_commit_records(staged_records, index, image_root)

      image_paths <- lapply(staged_items, `[[`, "attachment_path")
      checkpoint_paths <- lapply(staged_items, `[[`, "checkpoint_path")
      names(image_paths) <- chunk_ids
      names(checkpoint_paths) <- chunk_ids

      if (verbose) {
        first_position <- completed_count + 1L
        last_position <- completed_count + length(chunk_ids)
        message(
          "Describing image batch ", batch_index, "/", batch_count,
          " (", first_position, "-", last_position, " of ", length(pending), ")..."
        )
      }

      batch_error <- NULL
      batch_results <- tryCatch(
        .clh_run_image_batch(
          agent = agent,
          image_paths = image_paths,
          workers = min(workers_used, length(chunk_ids)),
          checkpoint_paths = checkpoint_paths
        ),
        interrupt = function(e) stop(e),
        error = function(e) {
          batch_error <<- conditionMessage(e)
          NULL
        }
      )

      if (is.null(batch_results)) {
        recovered_records <- list()
        for (image_id in chunk_ids) {
          reconciled <- .clh_image_reconcile_record(
            staged_items[[image_id]]$record,
            image_root
          )
          record <- reconciled$record
          metadata <- reconciled$metadata[[staged_items[[image_id]]$metadata$description_id]]
          if (!is.null(metadata) && identical(metadata$status, "interrupted")) {
            metadata$status <- "error"
            metadata$processed_at <- .clh_image_now()
            metadata$updated_at <- metadata$processed_at
            metadata$error_message <- paste0(
              "Image batch failed: ",
              .clh_or(batch_error, "unknown batch error")
            )
            metadata$checkpoint_file <- NULL
            record <- .clh_image_stage_attempt(record, metadata, image_root)
          }
          recovered_records[[image_id]] <- record
        }
        index <- .clh_image_commit_records(recovered_records, index, image_root)
        stop("Image batch failed: ", .clh_or(batch_error, "unknown batch error"), call. = FALSE)
      }

      task_results <- batch_results[names(batch_results) != "combined_stats"]
      task_names <- names(task_results)
      if (is.null(task_names) || length(task_results) != length(chunk_ids) ||
        anyDuplicated(task_names) || !setequal(task_names, chunk_ids)) {
        stop(
          "Image batch returned results without the expected image identifiers; cache was left recoverable.",
          call. = FALSE
        )
      }
      task_errors <- .clh_batch_errors_by_id(batch_results$combined_stats, chunk_ids)
      task_checkpoints <- list()
      for (image_id in chunk_ids) {
        raw <- task_results[[image_id]]
        if (is.list(raw)) {
          .clh_assert_image_task_id(raw$task_id, image_id, "result")
        }
        checkpoint <- .clh_image_checkpoint_load(staged_items[[image_id]]$metadata, image_root)
        checkpoint_problem <- .clh_image_checkpoint_problem(checkpoint, image_id)
        if (!is.null(checkpoint_problem)) {
          stop(
            "Invalid image checkpoint for ", image_id, ": ", checkpoint_problem,
            "; cache was left recoverable.",
            call. = FALSE
          )
        }
        task_checkpoints[[image_id]] <- checkpoint
      }

      final_records <- list()
      checkpoint_metadata <- list()
      batch_failures <- stats::setNames(rep(FALSE, length(chunk_ids)), chunk_ids)
      batch_error_messages <- stats::setNames(rep(NA_character_, length(chunk_ids)), chunk_ids)
      batch_failure_signatures <- stats::setNames(rep(NA_character_, length(chunk_ids)), chunk_ids)
      fatal_provider_failures <- stats::setNames(rep(FALSE, length(chunk_ids)), chunk_ids)
      rate_limit_failures <- stats::setNames(rep(FALSE, length(chunk_ids)), chunk_ids)
      .clh_image_mark_dirty(image_root)
      for (image_id in chunk_ids) {
        item <- staged_items[[image_id]]
        target <- item$target
        raw <- task_results[[image_id]]
        checkpoint <- task_checkpoints[[image_id]]
        description <- .clh_coerce_text(raw)
        elapsed <- if (is.list(raw) && !is.null(raw$duration)) {
          as.numeric(raw$duration)
        } else if (is.list(checkpoint) && !is.null(checkpoint$duration_seconds)) {
          as.numeric(checkpoint$duration_seconds)
        } else {
          NA_real_
        }
        checkpoint_error <- if (is.list(checkpoint)) {
          .clh_compact_error_message(checkpoint$error)
        } else {
          NA_character_
        }
        stats_error <- .clh_compact_error_message(task_errors[[image_id]])
        worker_error <- if (!is.na(checkpoint_error)) checkpoint_error else stats_error
        metadata <- item$metadata
        checkpoint_metadata[[image_id]] <- metadata
        metadata$checkpoint_file <- NULL
        metadata$processed_at <- .clh_image_now()
        metadata$updated_at <- metadata$processed_at
        metadata$elapsed_seconds <- elapsed
        metadata$response_class <- if (is.null(raw)) "NULL" else paste(class(raw), collapse = "/")
        record <- item$record

        if (is.na(worker_error) && !.clh_is_error_response(raw, description)) {
          metadata$status <- "processed"
          record <- .clh_image_stage_attempt(
            record,
            metadata,
            image_root,
            text = description,
            activate = TRUE
          )
          selected_active[[image_id]] <- list(
            id = metadata$description_id,
            description = .clh_image_description_summary(metadata),
            text = description
          )
          for (key in target$attachment_keys) description_map[[key]] <- description
          entry <- metadata
          entry$description <- description
          for (key in target$attachment_keys) run_items[[key]] <- entry
        } else {
          error_message <- if (!is.na(worker_error)) {
            worker_error
          } else {
            .clh_error_response_message(raw, description)
          }
          metadata$status <- "error"
          metadata$error_message <- error_message
          batch_failures[[image_id]] <- TRUE
          batch_error_messages[[image_id]] <- error_message
          batch_failure_signatures[[image_id]] <- .clh_image_failure_signature(
            error_message,
            raw = raw,
            checkpoint = checkpoint
          )
          fatal_provider_failures[[image_id]] <- .clh_is_fatal_provider_error(error_message)
          rate_limit_failures[[image_id]] <- .clh_is_rate_limit_error(error_message)
          record <- .clh_image_stage_attempt(record, metadata, image_root)
          if (!is.null(item$active)) {
            for (key in target$attachment_keys) description_map[[key]] <- item$active$text
          }
          entry <- metadata
          for (key in target$attachment_keys) run_items[[key]] <- entry
          if (verbose) {
            message(
              "Image description error recorded: ", item$attachment,
              " [", .clh_or(service, "default-service"), "/",
              .clh_or(model, "default-model"), "] - ", error_message
            )
          }
        }
        final_records[[image_id]] <- record
      }
      index <- .clh_image_commit_records(final_records, index, image_root)
      for (image_id in chunk_ids) {
        .clh_image_checkpoint_remove(checkpoint_metadata[[image_id]], image_root)
      }

      completed_count <- completed_count + length(chunk_ids)
      if (verbose) {
        message(
          "Image batch ", batch_index, "/", batch_count, " completed; ",
          completed_count, "/", length(pending),
          " provider calls finished. Estimated remaining: ",
          .clh_parallel_eta(provider_started, completed_count, length(pending)),
          "."
        )
      }
      queue_remaining <- completed_count < length(pending)
      failure_messages <- unique(stats::na.omit(batch_error_messages))
      failure_signatures <- unique(stats::na.omit(
        batch_failure_signatures[batch_failures]
      ))
      uniform_failure <- length(failure_signatures) == 1L
      stop_failed_queue <- all(rate_limit_failures) || uniform_failure || all(fatal_provider_failures)
      if (queue_remaining && length(batch_failures) && all(batch_failures) && stop_failed_queue) {
        if (all(rate_limit_failures)) {
          stop(
            "Every request in the latest image batch hit a provider rate limit. ",
            "The completed errors were cached; retry later or reduce `workers`.",
            call. = FALSE
          )
        }
        first_error <- unique(stats::na.omit(batch_error_messages))[1]
        stop(
          "Every request in the latest image batch failed. The completed errors ",
          "were cached and the remaining queue was stopped. First error: ",
          .clh_or(first_error, "unknown provider error"),
          call. = FALSE
        )
      }
    }
  }

  zip_name <- .clh_or(attr(chat, "source")$path, NULL)
  run_path <- .clh_run_log_path(chat_key, zip_id, kind = "image", zip_name = zip_name, cache_dir = cache_dir)
  if (!is.null(run_path)) {
    run_log <- list(
      chat_key = chat_key,
      zip_id = zip_id,
      run_at = .clh_image_now(),
      configuration_id = configuration$configuration_id,
      cache_mode = cache_mode,
      workers_requested = if (is.null(workers)) NA_integer_ else as.integer(workers),
      workers_used = workers_used,
      provider_batches = batch_count,
      provider_calls = length(pending),
      global_manifest = .clh_image_index_path(image_root),
      items = run_items
    )
    jsonlite::write_json(run_log, run_path, auto_unbox = TRUE, pretty = TRUE)
    if (verbose) message("Saved image run log: ", run_path)
  }

  if (!"attachments" %in% names(chat)) {
    chat$attachments <- lapply(chat$attachment, function(x) if (is.na(x)) character(0) else x)
  }
  if (!"attachment_keys" %in% names(chat)) {
    if ("attachment_paths" %in% names(chat)) {
      path_list <- chat$attachment_paths
    } else {
      path_list <- lapply(chat$attachments, function(x) rep(NA_character_, length(x)))
    }
    chat$attachment_keys <- mapply(function(att_names, att_paths) {
      if (length(att_names) == 0) return(character(0))
      if (length(att_paths) != length(att_names)) att_paths <- rep(NA_character_, length(att_names))
      vapply(seq_along(att_names), function(i) {
        .clh_attachment_key(att_names[i], att_paths[i], zip_id = zip_id)
      }, FUN.VALUE = character(1))
    }, chat$attachments, path_list, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  active_description_ids <- list()
  for (image_id in names(targets)) {
    active <- selected_active[[image_id]]
    if (is.null(active)) next
    for (attachment_key in targets[[image_id]]$attachment_keys) {
      description_map[[attachment_key]] <- active$text
      active_description_ids[[attachment_key]] <- active$id
    }
  }

  chat$image_descriptions <- mapply(function(att_names, att_keys) {
    if (length(att_names) == 0) return(character(0))
    if (length(att_keys) != length(att_names)) att_keys <- rep(NA_character_, length(att_names))
    vals <- vapply(seq_along(att_names), function(i) {
      att_key <- .clh_attachment_id(att_names[i], NA_character_, att_keys[i], zip_id = zip_id)
      if (!is.null(description_map[[att_key]])) return(description_map[[att_key]])
      NA_character_
    }, FUN.VALUE = character(1))
    names(vals) <- att_names
    vals <- vals[!is.na(vals)]
    vals
  }, chat$attachments, chat$attachment_keys, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  chat$image_description <- vapply(chat$image_descriptions, function(x) {
    if (length(x) == 0) return(NA_character_)
    if (length(x) == 1) return(unname(x[1]))
    paste(paste0("[", names(x), "] ", x), collapse = "\n")
  }, FUN.VALUE = character(1))

  chat$image_description_ids <- mapply(function(att_names, att_keys) {
    if (length(att_names) == 0) return(character(0))
    if (length(att_keys) != length(att_names)) att_keys <- rep(NA_character_, length(att_names))
    vals <- vapply(seq_along(att_names), function(i) {
      att_key <- .clh_attachment_id(att_names[i], NA_character_, att_keys[i], zip_id = zip_id)
      .clh_or(active_description_ids[[att_key]], NA_character_)
    }, FUN.VALUE = character(1))
    names(vals) <- att_names
    vals <- vals[!is.na(vals)]
    vals
  }, chat$attachments, chat$attachment_keys, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  chat$image_description_id <- vapply(chat$image_description_ids, function(x) {
    if (length(x) == 0) return(NA_character_)
    if (length(x) == 1) return(unname(x[1]))
    paste(paste0("[", names(x), "] ", x), collapse = "\n")
  }, FUN.VALUE = character(1))

  save_current_chat(chat)
}

#' Enrich text with media annotations
#' @param chat A `chatlens_chat` object
#' @param include_audio Include audio transcript annotations
#' @param include_images Include image description annotations
#' @param audio_tag Tag label used for audio annotations
#' @param image_tag Tag label used for image annotations
#' @param cache_dir Optional cache directory. When `NULL`, uses the chat's
#'   stored cache location or `~/.chatlens`.
#' @param save_chat Save the current chat state in cache as `chat.rds` plus a
#'   mirrored `chat.txt` transcript
#' @export
cl_chat_process_media <- function(chat,
                                 include_audio = TRUE,
                                 include_images = TRUE,
                                 audio_tag = "AUDIO TRANSCRIPT",
                                 image_tag = "IMAGE DESCRIPTION",
                                 cache_dir = NULL,
                                 save_chat = TRUE) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  text <- chat$text

  for (i in seq_len(nrow(chat))) {
    text_i <- text[i]

    if (include_audio) {
      if ("audio_transcripts" %in% names(chat)) {
        audio_map <- chat$audio_transcripts[[i]]
        if (length(audio_map) > 0) {
          for (name in names(audio_map)) {
            text_i <- .clh_insert_annotation(text_i, name, audio_map[[name]], audio_tag)
          }
        }
      } else if ("audio_transcript" %in% names(chat) && !is.na(chat$audio_transcript[i])) {
        text_i <- .clh_insert_annotation(text_i, chat$attachment[i], chat$audio_transcript[i], audio_tag)
      }
    }

    if (include_images) {
      if ("image_descriptions" %in% names(chat)) {
        image_map <- chat$image_descriptions[[i]]
        if (length(image_map) > 0) {
          for (name in names(image_map)) {
            text_i <- .clh_insert_annotation(text_i, name, image_map[[name]], image_tag)
          }
        }
      } else if ("image_description" %in% names(chat) && !is.na(chat$image_description[i])) {
        text_i <- .clh_insert_annotation(text_i, chat$attachment[i], chat$image_description[i], image_tag)
      }
    }

    text[i] <- text_i
  }

  chat$text_enriched <- text
  if (save_chat) .clh_save_current_chat(chat, cache_dir = cache_dir) else chat
}

.clh_insert_annotation <- function(text, attachment, annotation, tag) {
  if (is.na(annotation) || !nzchar(annotation)) return(text)
  replacement <- sprintf("[%s] %s [/%s]", tag, annotation, tag)
  if (!is.na(attachment) && nzchar(attachment) && !is.na(text)) {
    att_re <- .clh_escape_regex(attachment)
    pattern <- paste0(att_re, "(\\s*\\((arquivo anexado|file attached|attached|anexado)\\))?")
    if (isTRUE(grepl(pattern, text, ignore.case = TRUE))) {
      return(sub(pattern, replacement, text, ignore.case = TRUE))
    }
  }
  if (is.na(text) || !nzchar(text)) return(replacement)
  paste(text, replacement, sep = "\n")
}
