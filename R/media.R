# Media processing: audio transcription and image description

chatlens_ffmpeg_path <- function() {
  path <- Sys.which("ffmpeg")
  if (path == "") NA_character_ else path
}

chatlens_convert_audio_mp3 <- function(input_path, output_path, quiet = TRUE) {
  ffmpeg <- chatlens_ffmpeg_path()
  if (is.na(ffmpeg)) stop("ffmpeg not found on PATH.")
  input_path <- normalizePath(input_path, winslash = "/", mustWork = TRUE)
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  cmd <- sprintf('"%s" -y -i "%s" "%s"', ffmpeg, input_path, output_path)
  res <- system(cmd, ignore.stdout = quiet, ignore.stderr = quiet)
  if (res != 0) stop("ffmpeg conversion failed for: ", basename(input_path))
  output_path
}

chatlens_manifest_sources <- function(existing, zip_id) {
  if (is.null(zip_id) || is.na(zip_id)) return(existing %||% character(0))
  unique(c(existing %||% character(0), zip_id))
}

chatlens_build_named_map <- function(values, names_vec) {
  if (length(values) == 0 || length(names_vec) == 0) return(character(0))
  out <- values
  names(out) <- names_vec
  out
}

chatlens_attachment_id <- function(att_name, att_path, att_key = NA_character_, zip_id = NULL) {
  if (!is.na(att_key) && nzchar(att_key)) return(att_key)
  chatlens_attachment_key(att_name, att_path, zip_id = zip_id)
}

chatlens_resolve_chat_key <- function(chat) {
  key <- attr(chat, "chat_key") %||% attr(chat, "source")$chat_key %||% NULL
  if (is.null(key) || !nzchar(key)) "chat_unknown" else key
}

chatlens_resolve_store_dir <- function(chat, cache_dir = cl_cache_dir()) {
  store_dir <- attr(chat, "source")$store_dir %||% NULL
  if (!is.null(store_dir) && nzchar(store_dir)) {
    return(chatlens_ensure_dir(store_dir))
  }
  chatlens_chat_store_dir(chatlens_resolve_chat_key(chat), cache_dir)
}

chatlens_format_duration <- function(seconds) {
  if (is.null(seconds) || !is.finite(seconds) || is.na(seconds)) return("calculating")
  total <- as.integer(round(max(0, as.numeric(seconds))))
  hh <- total %/% 3600L
  mm <- (total %% 3600L) %/% 60L
  ss <- total %% 60L
  if (hh > 0L) sprintf("%02d:%02d:%02d", hh, mm, ss) else sprintf("%02d:%02d", mm, ss)
}

chatlens_progress_with_eta <- function(index, total, durations) {
  if (length(durations) == 0) {
    return(sprintf("(%d of %d, ETA calculating)", index, total))
  }
  remaining <- total - index + 1L
  eta <- mean(durations) * remaining
  sprintf("(%d of %d, ETA %s)", index, total, chatlens_format_duration(eta))
}

chatlens_estimated_remaining <- function(durations, remaining_items) {
  if (length(durations) == 0) return("calculating")
  chatlens_format_duration(mean(durations) * max(0L, remaining_items))
}

chatlens_default_image_prompt <- function() {
  "Describe the image in detail for context."
}

chatlens_normalize_prompt <- function(prompt, default_prompt) {
  if (is.null(prompt) || (is.character(prompt) && length(prompt) == 0L)) return(default_prompt)
  if (!is.character(prompt) || length(prompt) != 1L) stop("prompt must be a length-1 character string")
  prompt <- trimws(prompt)
  if (!nzchar(prompt)) stop("prompt must be a non-empty character string")
  prompt
}

chatlens_manifest_prompt <- function(entry, default_prompt) {
  if (is.null(entry) || is.null(entry$prompt)) return(default_prompt)
  prompt <- as.character(entry$prompt[1])
  if (!nzchar(prompt)) return(default_prompt)
  prompt
}

chatlens_unique_attachment_rows <- function(att, rows, zip_id = NULL) {
  if (length(rows) == 0) return(integer(0))
  out <- integer(0)
  seen <- character(0)
  for (r in rows) {
    att_name <- att$attachment[r]
    if (is.na(att_name) || !nzchar(att_name)) next
    att_key <- chatlens_attachment_id(att_name, att$attachment_path[r], att$attachment_key[r], zip_id = zip_id)
    if (att_key %in% seen) next
    seen <- c(seen, att_key)
    out <- c(out, r)
  }
  out
}

#' Transcribe audio attachments
#' @param chat A `chatlens_chat` object
#' @param service Provider name for speech-to-text
#' @param model Model id for speech-to-text
#' @param cache_dir Cache directory
#' @param overwrite Reprocess items even if cached output exists
#' @param verbose Whether to emit progress messages
#' @param ... Additional arguments passed to [genflow::gen_stt()]
#' @export
cl_chat_transcribe_audio <- function(chat,
                                      service = "replicate",
                                      model = "openai/whisper",
                                      cache_dir = cl_cache_dir(),
                                      overwrite = FALSE,
                                      verbose = TRUE,
                                      ...) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  if (!requireNamespace("genflow", quietly = TRUE)) stop("genflow is required for transcription")

  chat_key <- chatlens_resolve_chat_key(chat)
  zip_id <- attr(chat, "zip_id")
  cache_base <- chatlens_resolve_store_dir(chat, cache_dir)
  attr(chat, "chat_key") <- chat_key
  audio_dir <- chatlens_ensure_dir(file.path(cache_base, "audio"))
  transcript_dir <- chatlens_ensure_dir(file.path(cache_base, "audio_transcripts"))
  manifest_path <- file.path(cache_base, "audio_manifest.json")
  manifest <- chatlens_manifest_load(manifest_path)
  if (is.null(manifest$items)) manifest$items <- list()
  run_items <- list()

  att <- cl_attachments(chat)
  if (nrow(att) == 0) {
    warning("No attachments found.")
    chat$audio_transcripts <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$audio_transcript <- rep(NA_character_, nrow(chat))
    return(chat)
  }

  if (any(is.na(att$attachment_type))) {
    idx <- which(is.na(att$attachment_type))
    att$attachment_type[idx] <- vapply(att$attachment[idx], chatlens_attachment_type_from_name, FUN.VALUE = character(1))
  }

  audio_rows <- which(att$attachment_type == "audio")
  audio_rows <- chatlens_unique_attachment_rows(att, audio_rows, zip_id = zip_id)
  if (length(audio_rows) == 0) {
    warning("No audio attachments found.")
    chat$audio_transcripts <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$audio_transcript <- rep(NA_character_, nrow(chat))
    return(chat)
  }

  transcript_map <- list()
  audio_durations <- numeric(0)

  for (idx in seq_along(audio_rows)) {
    r <- audio_rows[idx]
    att_name <- att$attachment[r]
    att_path <- att$attachment_path[r]
    att_key <- chatlens_attachment_id(att_name, att_path, att$attachment_key[r], zip_id = zip_id)
    progress <- chatlens_progress_with_eta(idx, length(audio_rows), audio_durations)

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

    if (is.na(att_path) || !file.exists(att_path)) {
      warning("Audio file missing: ", att_name)
      if (verbose) message("Skipping (missing audio file) ", progress, ": ", att_name)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "missing",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        transcript = NA_character_,
        sources = chatlens_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
      next
    }

    base <- tools::file_path_sans_ext(basename(att_path))
    transcript_path <- file.path(transcript_dir, paste0(base, ".txt"))

    if (!overwrite && file.exists(transcript_path)) {
      cached_text <- paste(readLines(transcript_path, warn = FALSE), collapse = "\n")
      if (!chatlens_is_error_response(NULL, cached_text)) {
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
          sources = chatlens_manifest_sources(existing$sources, zip_id)
        )
        manifest$items[[att_key]] <- entry
        run_items[[att_key]] <- entry
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
        audio_path <- chatlens_convert_audio_mp3(att_path, audio_path)
      }
    }

    if (verbose) message("Transcribing audio ", progress, ": ", basename(audio_path))

    args <- list(audio_path)
    if (!is.null(service)) args$service <- service
    if (!is.null(model)) args$model <- model
    extra <- list(...)
    if (length(extra)) args <- c(args, extra)

    started_at <- Sys.time()
    raw <- tryCatch(
      do.call(genflow::gen_stt, args),
      error = function(e) {
        warning("Transcription failed for ", basename(att_path), ": ", e$message)
        NA_character_
      }
    )
    elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    audio_durations <- c(audio_durations, elapsed)

    transcript <- chatlens_coerce_text(raw)

    if (!chatlens_is_error_response(raw, transcript)) {
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
        sources = chatlens_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
    } else {
      if (verbose) message("Transcription error recorded ", progress, ": ", att_name)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "error",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        transcript = NA_character_,
        response = raw,
        sources = chatlens_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
    }

    if (verbose) {
      eta_left <- chatlens_estimated_remaining(audio_durations, length(audio_rows) - idx)
      if (length(audio_durations) == 1L) {
        message(
          "First audio transcription took ",
          chatlens_format_duration(elapsed),
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
          chatlens_format_duration(elapsed),
          ". Estimated remaining: ",
          eta_left,
          "."
        )
      }
    }
  }

  manifest$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  chatlens_manifest_save(manifest, manifest_path)

  zip_name <- attr(chat, "source")$path %||% NULL
  run_path <- chatlens_run_log_path(chat_key, zip_id, kind = "audio", zip_name = zip_name, cache_dir = cache_dir)
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
        chatlens_attachment_key(att_names[i], att_paths[i], zip_id = zip_id)
      }, FUN.VALUE = character(1))
    }, chat$attachments, path_list, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  chat$audio_transcripts <- mapply(function(att_names, att_keys) {
    if (length(att_names) == 0) return(character(0))
    if (length(att_keys) != length(att_names)) att_keys <- rep(NA_character_, length(att_names))
    vals <- vapply(seq_along(att_names), function(i) {
      att_key <- chatlens_attachment_id(att_names[i], NA_character_, att_keys[i], zip_id = zip_id)
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

  chat
}

#' Describe image attachments
#' @param chat A `chatlens_chat` object
#' @param prompt Prompt used to describe each image
#' @param service Provider name for image description
#' @param model Model id for image description
#' @param cache_dir Cache directory
#' @param overwrite Reprocess items even if cached output exists
#' @param verbose Whether to emit progress messages
#' @param ... Additional arguments passed to [genflow::gen_txt()]
#' @export
cl_chat_describe_images <- function(chat,
                                     prompt = "Describe the image in detail for context.",
                                     service = "openrouter",
                                     model = "google/gemini-3-flash-preview",
                                     cache_dir = cl_cache_dir(),
                                     overwrite = FALSE,
                                     verbose = TRUE,
                                     ...) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")
  if (!requireNamespace("genflow", quietly = TRUE)) stop("genflow is required for image description")
  prompt <- chatlens_normalize_prompt(prompt, chatlens_default_image_prompt())

  chat_key <- chatlens_resolve_chat_key(chat)
  zip_id <- attr(chat, "zip_id")
  cache_base <- chatlens_resolve_store_dir(chat, cache_dir)
  attr(chat, "chat_key") <- chat_key
  desc_dir <- chatlens_ensure_dir(file.path(cache_base, "image_descriptions"))
  manifest_path <- file.path(cache_base, "image_manifest.json")
  manifest <- chatlens_manifest_load(manifest_path)
  if (is.null(manifest$items)) manifest$items <- list()
  run_items <- list()

  att <- cl_attachments(chat)
  if (nrow(att) == 0) {
    warning("No attachments found.")
    chat$image_descriptions <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$image_description <- rep(NA_character_, nrow(chat))
    return(chat)
  }

  if (any(is.na(att$attachment_type))) {
    idx <- which(is.na(att$attachment_type))
    att$attachment_type[idx] <- vapply(att$attachment[idx], chatlens_attachment_type_from_name, FUN.VALUE = character(1))
  }

  image_rows <- which(att$attachment_type == "image")
  image_rows <- chatlens_unique_attachment_rows(att, image_rows, zip_id = zip_id)
  if (length(image_rows) == 0) {
    warning("No image attachments found.")
    chat$image_descriptions <- lapply(seq_len(nrow(chat)), function(...) character(0))
    chat$image_description <- rep(NA_character_, nrow(chat))
    return(chat)
  }

  description_map <- list()
  image_durations <- numeric(0)

  for (idx in seq_along(image_rows)) {
    r <- image_rows[idx]
    att_name <- att$attachment[r]
    att_path <- att$attachment_path[r]
    att_key <- chatlens_attachment_id(att_name, att_path, att$attachment_key[r], zip_id = zip_id)
    progress <- chatlens_progress_with_eta(idx, length(image_rows), image_durations)

    existing <- manifest$items[[att_key]]
    if (!overwrite && !is.null(existing) &&
      identical(existing$status, "processed") &&
      identical(existing$service, service) &&
      identical(existing$model, model) &&
      identical(chatlens_manifest_prompt(existing, chatlens_default_image_prompt()), prompt) &&
      !is.null(existing$description) && nzchar(existing$description)) {
      description_map[[att_key]] <- existing$description
      if (verbose) message("Skipping (already processed) ", progress, ": ", att_name)
      run_items[[att_key]] <- c(existing, list(prompt = chatlens_manifest_prompt(existing, chatlens_default_image_prompt()), reused = TRUE))
      next
    }

    if (is.na(att_path) || !file.exists(att_path)) {
      warning("Image file missing: ", att_name)
      if (verbose) message("Skipping (missing image file) ", progress, ": ", att_name)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "missing",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        prompt = prompt,
        description = NA_character_,
        sources = chatlens_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
      next
    }

    base <- tools::file_path_sans_ext(basename(att_path))
    desc_path <- file.path(desc_dir, paste0(base, ".txt"))

    if (!overwrite && file.exists(desc_path)) {
      prompt_matches_cache <- FALSE
      if (!is.null(existing)) {
        prompt_matches_cache <- identical(chatlens_manifest_prompt(existing, chatlens_default_image_prompt()), prompt)
      } else if (identical(prompt, chatlens_default_image_prompt())) {
        # Backward-compatible fallback for old manifests that do not track prompt.
        prompt_matches_cache <- TRUE
      }

      if (!prompt_matches_cache) {
        if (verbose) message("Cached image description prompt mismatch; reprocessing ", progress, ": ", basename(desc_path))
      } else {
        cached_text <- paste(readLines(desc_path, warn = FALSE), collapse = "\n")
        if (!chatlens_is_error_response(NULL, cached_text)) {
          if (verbose) message("Using cached image description ", progress, ": ", basename(desc_path))
          description_map[[att_key]] <- cached_text
          entry <- list(
            attachment_key = att_key,
            attachment = att_name,
            status = "processed",
            service = service,
            model = model,
            processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            prompt = prompt,
            description = cached_text,
            saved_file = desc_path,
            sources = chatlens_manifest_sources(existing$sources, zip_id)
          )
          manifest$items[[att_key]] <- entry
          run_items[[att_key]] <- entry
          next
        } else if (verbose) {
          message("Cached description looks like an error; reprocessing ", progress, ": ", basename(desc_path))
        }
      }
    }

    args <- list(prompt)
    args$add_img <- att_path
    if (!is.null(service)) args$service <- service
    if (!is.null(model)) args$model <- model
    extra <- list(...)
    if (length(extra)) args <- c(args, extra)

    if (verbose) message("Describing image ", progress, ": ", basename(att_path))

    started_at <- Sys.time()
    raw <- tryCatch(
      do.call(genflow::gen_txt, args),
      error = function(e) {
        warning("Image description failed for ", basename(att_path), ": ", e$message)
        NA_character_
      }
    )
    elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    image_durations <- c(image_durations, elapsed)

    description <- chatlens_coerce_text(raw)

    if (!chatlens_is_error_response(raw, description)) {
      description_map[[att_key]] <- description
      writeLines(description, desc_path)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "processed",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        prompt = prompt,
        description = description,
        response = raw,
        saved_file = desc_path,
        sources = chatlens_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
    } else {
      if (verbose) message("Image description error recorded ", progress, ": ", att_name)
      if (file.exists(desc_path)) unlink(desc_path)
      entry <- list(
        attachment_key = att_key,
        attachment = att_name,
        status = "error",
        service = service,
        model = model,
        processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        prompt = prompt,
        description = NA_character_,
        response = raw,
        sources = chatlens_manifest_sources(existing$sources, zip_id)
      )
      manifest$items[[att_key]] <- entry
      run_items[[att_key]] <- entry
    }

    if (verbose) {
      eta_left <- chatlens_estimated_remaining(image_durations, length(image_rows) - idx)
      if (length(image_durations) == 1L) {
        message(
          "First image description took ",
          chatlens_format_duration(elapsed),
          ". Estimated remaining: ",
          eta_left,
          "."
        )
      } else {
        message(
          "Image ",
          idx,
          "/",
          length(image_rows),
          " completed in ",
          chatlens_format_duration(elapsed),
          ". Estimated remaining: ",
          eta_left,
          "."
        )
      }
    }
  }

  manifest$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  chatlens_manifest_save(manifest, manifest_path)

  zip_name <- attr(chat, "source")$path %||% NULL
  run_path <- chatlens_run_log_path(chat_key, zip_id, kind = "image", zip_name = zip_name, cache_dir = cache_dir)
  if (!is.null(run_path)) {
    run_log <- list(
      chat_key = chat_key,
      zip_id = zip_id,
      run_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
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
        chatlens_attachment_key(att_names[i], att_paths[i], zip_id = zip_id)
      }, FUN.VALUE = character(1))
    }, chat$attachments, path_list, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  chat$image_descriptions <- mapply(function(att_names, att_keys) {
    if (length(att_names) == 0) return(character(0))
    if (length(att_keys) != length(att_names)) att_keys <- rep(NA_character_, length(att_names))
    vals <- vapply(seq_along(att_names), function(i) {
      att_key <- chatlens_attachment_id(att_names[i], NA_character_, att_keys[i], zip_id = zip_id)
      if (!is.null(description_map[[att_key]])) return(description_map[[att_key]])
      existing <- manifest$items[[att_key]]
      if (!is.null(existing) && !is.null(existing$description)) return(existing$description)
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

  chat
}

#' Enrich text with media annotations
#' @param chat A `chatlens_chat` object
#' @param include_audio Include audio transcript annotations
#' @param include_images Include image description annotations
#' @param audio_tag Tag label used for audio annotations
#' @param image_tag Tag label used for image annotations
#' @export
cl_chat_process_media <- function(chat,
                                 include_audio = TRUE,
                                 include_images = TRUE,
                                 audio_tag = "AUDIO TRANSCRIPT",
                                 image_tag = "IMAGE DESCRIPTION") {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  text <- chat$text

  for (i in seq_len(nrow(chat))) {
    text_i <- text[i]

    if (include_audio) {
      if ("audio_transcripts" %in% names(chat)) {
        audio_map <- chat$audio_transcripts[[i]]
        if (length(audio_map) > 0) {
          for (name in names(audio_map)) {
            text_i <- chatlens_insert_annotation(text_i, name, audio_map[[name]], audio_tag)
          }
        }
      } else if ("audio_transcript" %in% names(chat) && !is.na(chat$audio_transcript[i])) {
        text_i <- chatlens_insert_annotation(text_i, chat$attachment[i], chat$audio_transcript[i], audio_tag)
      }
    }

    if (include_images) {
      if ("image_descriptions" %in% names(chat)) {
        image_map <- chat$image_descriptions[[i]]
        if (length(image_map) > 0) {
          for (name in names(image_map)) {
            text_i <- chatlens_insert_annotation(text_i, name, image_map[[name]], image_tag)
          }
        }
      } else if ("image_description" %in% names(chat) && !is.na(chat$image_description[i])) {
        text_i <- chatlens_insert_annotation(text_i, chat$attachment[i], chat$image_description[i], image_tag)
      }
    }

    text[i] <- text_i
  }

  chat$text_enriched <- text
  chat
}

chatlens_insert_annotation <- function(text, attachment, annotation, tag) {
  if (is.na(annotation) || !nzchar(annotation)) return(text)
  replacement <- sprintf("[%s] %s [/%s]", tag, annotation, tag)
  if (!is.na(attachment) && nzchar(attachment) && !is.na(text)) {
    att_re <- chatlens_escape_regex(attachment)
    pattern <- paste0(att_re, "(\\s*\\((arquivo anexado|file attached|attached|anexado)\\))?")
    if (isTRUE(grepl(pattern, text, ignore.case = TRUE))) {
      return(sub(pattern, replacement, text, ignore.case = TRUE))
    }
  }
  if (is.na(text) || !nzchar(text)) return(replacement)
  paste(text, replacement, sep = "\n")
}
