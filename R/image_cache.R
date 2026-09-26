# Versioned image-description cache helpers

.clh_image_schema_version <- function() 2L

.clh_image_now <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%S%z")
}

.clh_object_md5 <- function(x) {
  path <- tempfile("chatlens_hash_", fileext = ".rds")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  saveRDS(x, path, version = 2)
  unname(tools::md5sum(path))
}

.clh_atomic_write_lines <- function(text, path) {
  .clh_ensure_dir(dirname(path))
  tmp <- tempfile(paste0(".", basename(path), "_"), tmpdir = dirname(path))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  writeLines(text, tmp, useBytes = TRUE)
  replaced <- file.rename(tmp, path)
  if (!isTRUE(replaced)) {
    copied <- file.copy(tmp, path, overwrite = TRUE)
    if (!isTRUE(copied)) stop("Could not save text artifact: ", path)
    unlink(tmp)
  }
  invisible(path)
}

.clh_image_cache_root <- function(cache_base) {
  .clh_ensure_dir(file.path(cache_base, "image_descriptions"))
}

.clh_image_index_path <- function(image_root) {
  file.path(image_root, "image_manifest.json")
}

.clh_image_dirty_path <- function(image_root) {
  file.path(image_root, ".index_dirty")
}

.clh_image_record_path <- function(image_root, image_id) {
  file.path(image_root, image_id, "image.json")
}

.clh_image_description_dir <- function(image_root, image_id) {
  .clh_ensure_dir(file.path(image_root, image_id, "descriptions"))
}

.clh_image_relative_path <- function(image_root, path) {
  root <- normalizePath(image_root, winslash = "/", mustWork = FALSE)
  value <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(value, prefix)) substring(value, nchar(prefix) + 1L) else value
}

.clh_image_absolute_path <- function(image_root, path) {
  if (is.null(path) || length(path) == 0L || is.na(path[1]) || !nzchar(path[1])) {
    return(NA_character_)
  }
  path <- as.character(path[1])
  if (grepl("^/|^[A-Za-z]:[/\\\\]", path)) return(path)
  file.path(image_root, path)
}

.clh_image_id <- function(attachment_key, attachment_path = NA_character_) {
  content_md5 <- NA_character_
  if (!is.na(attachment_path) && nzchar(attachment_path) && file.exists(attachment_path)) {
    content_md5 <- unname(tools::md5sum(attachment_path))
  }
  if (is.na(content_md5) || !nzchar(content_md5)) {
    content_md5 <- .clh_object_md5(list(attachment_key = attachment_key))
  }
  paste0("img_", content_md5)
}

.clh_image_empty_index <- function() {
  list(
    schema_version = .clh_image_schema_version(),
    kind = "image_description_index",
    updated_at = NULL,
    migrations = list(),
    configurations = list(),
    attachment_index = list(),
    images = list()
  )
}

.clh_image_empty_record <- function(image_id) {
  now <- .clh_image_now()
  list(
    schema_version = .clh_image_schema_version(),
    image_id = image_id,
    created_at = now,
    updated_at = now,
    content_md5 = sub("^img_", "", image_id),
    attachment_keys = character(0),
    original_names = character(0),
    source_paths = character(0),
    sources = character(0),
    occurrences = list(),
    active_description_id = NA_character_,
    descriptions = list()
  )
}

.clh_image_normalize_index <- function(index) {
  if (is.null(index$migrations)) index$migrations <- list()
  if (is.null(index$configurations)) index$configurations <- list()
  if (is.null(index$attachment_index)) index$attachment_index <- list()
  if (is.null(index$images)) index$images <- list()
  index$schema_version <- .clh_image_schema_version()
  index$kind <- "image_description_index"
  index
}

.clh_image_normalize_record <- function(record, image_id) {
  if (is.null(record) || !is.list(record)) record <- .clh_image_empty_record(image_id)
  if (is.null(record$image_id) || !nzchar(record$image_id)) record$image_id <- image_id
  if (is.null(record$created_at)) record$created_at <- .clh_image_now()
  if (is.null(record$updated_at)) record$updated_at <- record$created_at
  if (is.null(record$content_md5)) record$content_md5 <- sub("^img_", "", image_id)
  if (is.null(record$attachment_keys)) record$attachment_keys <- character(0)
  if (is.null(record$original_names)) record$original_names <- character(0)
  if (is.null(record$source_paths)) record$source_paths <- character(0)
  if (is.null(record$sources)) record$sources <- character(0)
  record$attachment_keys <- .clh_image_unique_strings(record$attachment_keys)
  record$original_names <- .clh_image_unique_strings(record$original_names)
  record$source_paths <- .clh_image_unique_strings(record$source_paths)
  record$sources <- .clh_image_unique_strings(record$sources)
  if (is.null(record$occurrences)) record$occurrences <- list()
  if (is.null(record$active_description_id)) record$active_description_id <- NA_character_
  if (is.null(record$descriptions)) record$descriptions <- list()
  record$schema_version <- .clh_image_schema_version()
  record
}

.clh_image_unique_strings <- function(x) {
  x <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
  x <- x[!is.na(x) & nzchar(x)]
  unique(x)
}

.clh_image_record_add_context <- function(record,
                                          attachment_keys = character(0),
                                          original_names = character(0),
                                          source_paths = character(0),
                                          sources = character(0),
                                          occurrences = list()) {
  record$attachment_keys <- .clh_image_unique_strings(c(
    record$attachment_keys,
    attachment_keys
  ))
  record$original_names <- .clh_image_unique_strings(c(
    record$original_names,
    original_names
  ))
  record$source_paths <- .clh_image_unique_strings(c(
    record$source_paths,
    source_paths
  ))
  record$sources <- .clh_image_unique_strings(c(record$sources, sources))
  if (length(occurrences)) {
    for (id in names(occurrences)) record$occurrences[[id]] <- occurrences[[id]]
  }
  record$updated_at <- .clh_image_now()
  record
}

.clh_image_occurrence <- function(chat_key,
                                  zip_id,
                                  message_id,
                                  timestamp,
                                  attachment_key,
                                  attachment,
                                  occurrence_index) {
  timestamp_text <- if (length(timestamp) && !is.na(timestamp[1])) {
    format(timestamp[1], "%Y-%m-%dT%H:%M:%S%z")
  } else {
    NA_character_
  }
  occurrence <- list(
    chat_key = chat_key,
    zip_id = if (is.null(zip_id)) NA_character_ else zip_id,
    message_id = message_id,
    timestamp = timestamp_text,
    attachment_key = attachment_key,
    attachment = attachment
  )
  id <- paste(
    "occ",
    substr(.clh_path_slug(chat_key, "chat"), 1L, 24L),
    substr(.clh_path_slug(zip_id, "source"), 1L, 12L),
    .clh_path_slug(message_id, "message"),
    as.integer(occurrence_index),
    sep = "_"
  )
  stats::setNames(list(occurrence), id)
}

.clh_image_record_load <- function(image_root, image_id, fallback = NULL) {
  path <- .clh_image_record_path(image_root, image_id)
  record <- NULL
  if (file.exists(path)) {
    record <- .clh_manifest_load(path)
    if (is.null(record$image_id)) record <- NULL
  }
  if (is.null(record) && !is.null(fallback)) {
    record <- fallback
    record$active_description <- NULL
    record$record_file <- NULL
    record$status <- NULL
    record$description_count <- NULL
    record$attempt_count <- NULL
    record$occurrence_count <- NULL
  }
  .clh_image_normalize_record(record, image_id)
}

.clh_image_valid_text <- function(text) {
  is.character(text) && length(text) == 1L && !is.na(text) && nzchar(text) &&
    !.clh_is_error_response(NULL, text)
}

.clh_image_read_description <- function(description, image_root, fallback = NULL) {
  text_path <- .clh_image_absolute_path(image_root, description$text_file)
  if (!is.na(text_path) && file.exists(text_path)) {
    text <- paste(readLines(text_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (.clh_image_valid_text(text)) return(text)
  }
  if (.clh_image_valid_text(fallback)) return(as.character(fallback))
  NA_character_
}

.clh_image_description_summary <- function(metadata) {
  fields <- c(
    "description_id", "image_id", "status", "created_at", "processed_at",
    "updated_at", "service", "model", "prompt_hash", "configuration_id",
    "text_file", "metadata_file", "elapsed_seconds", "error_message",
    "checkpoint_file", "recovered_from", "legacy_file", "legacy_md5",
    "source_zip_id"
  )
  metadata[intersect(fields, names(metadata))]
}

.clh_image_checkpoint_load <- function(metadata, image_root) {
  path <- .clh_image_absolute_path(image_root, metadata$checkpoint_file)
  if (is.na(path) || !file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) {
    structure(list(error = conditionMessage(e)), class = "chatlens_checkpoint_error")
  })
}

.clh_image_checkpoint_problem <- function(checkpoint, image_id) {
  if (is.null(checkpoint)) return(NULL)
  if (inherits(checkpoint, "chatlens_checkpoint_error")) {
    return(paste0("checkpoint could not be read: ", checkpoint$error))
  }
  if (!is.list(checkpoint)) return("checkpoint is not a list")
  required <- c(
    "schema_version", "task_id", "completed_at", "response", "error",
    "duration_seconds"
  )
  missing_fields <- setdiff(required, names(checkpoint))
  if (length(missing_fields)) {
    return(paste0("checkpoint is missing field(s): ", paste(missing_fields, collapse = ", ")))
  }
  schema_version <- suppressWarnings(as.integer(checkpoint$schema_version))
  if (length(schema_version) != 1L || is.na(schema_version) || schema_version != 1L) {
    return("checkpoint schema_version is unsupported")
  }
  task_id <- checkpoint$task_id
  if (length(task_id) != 1L || is.na(task_id) || !nzchar(as.character(task_id)) ||
    !identical(as.character(task_id), as.character(image_id))) {
    return(paste0("checkpoint task_id does not match ", image_id))
  }
  NULL
}

.clh_image_checkpoint_remove <- function(metadata, image_root) {
  path <- .clh_image_absolute_path(image_root, metadata$checkpoint_file)
  if (!is.na(path) && file.exists(path)) unlink(path)
  invisible(path)
}

.clh_image_successful_ids <- function(record, image_root) {
  if (length(record$descriptions) == 0L) return(character(0))
  ids <- names(record$descriptions)
  if (is.null(ids)) return(character(0))
  ids[vapply(ids, function(id) {
    description <- record$descriptions[[id]]
    identical(description$status, "processed") &&
      .clh_image_valid_text(.clh_image_read_description(description, image_root))
  }, FUN.VALUE = logical(1))]
}

.clh_image_active_from_record <- function(record, image_root, fallback = NULL) {
  active_id <- .clh_or(record$active_description_id, NA_character_)
  if (length(active_id) == 0L || is.na(active_id[1]) || !nzchar(active_id[1])) {
    active_id <- NA_character_
  } else {
    active_id <- as.character(active_id[1])
  }

  if (!is.na(active_id) && !is.null(record$descriptions[[active_id]])) {
    description <- record$descriptions[[active_id]]
    text <- .clh_image_read_description(description, image_root, fallback = fallback)
    if (identical(description$status, "processed") && .clh_image_valid_text(text)) {
      return(list(id = active_id, description = description, text = text))
    }
  }

  ids <- .clh_image_successful_ids(record, image_root)
  if (length(ids) == 0L) return(NULL)
  id <- ids[length(ids)]
  description <- record$descriptions[[id]]
  list(
    id = id,
    description = description,
    text = .clh_image_read_description(description, image_root)
  )
}

.clh_image_index_image_summary <- function(record, image_root) {
  active <- .clh_image_active_from_record(record, image_root)
  statuses <- if (length(record$descriptions)) {
    vapply(record$descriptions, function(x) .clh_or(x$status, "unknown"), FUN.VALUE = character(1))
  } else {
    character(0)
  }
  status <- if (!is.null(active)) {
    "processed"
  } else if (length(statuses)) {
    statuses[length(statuses)]
  } else {
    "pending"
  }

  list(
    image_id = record$image_id,
    status = status,
    record_file = .clh_image_relative_path(
      image_root,
      .clh_image_record_path(image_root, record$image_id)
    ),
    content_md5 = record$content_md5,
    attachment_keys = record$attachment_keys,
    original_names = record$original_names,
    source_paths = record$source_paths,
    sources = record$sources,
    occurrence_count = length(record$occurrences),
    active_description_id = if (is.null(active)) NA_character_ else active$id,
    description_count = sum(statuses == "processed"),
    attempt_count = length(statuses),
    descriptions = record$descriptions,
    updated_at = record$updated_at
  )
}

.clh_image_mark_dirty <- function(image_root) {
  .clh_atomic_write_lines(.clh_image_now(), .clh_image_dirty_path(image_root))
}

.clh_image_index_register_configuration <- function(index, configuration) {
  index$configurations[[configuration$configuration_id]] <- configuration
  index
}

.clh_image_index_add_record <- function(index, record, image_root) {
  index$images[[record$image_id]] <- .clh_image_index_image_summary(record, image_root)
  if (length(record$attachment_keys)) {
    for (attachment_key in record$attachment_keys) {
      if (!is.na(attachment_key) && nzchar(attachment_key)) {
        index$attachment_index[[attachment_key]] <- record$image_id
      }
    }
  }
  index
}

.clh_image_commit_records <- function(records, index, image_root) {
  if (length(records) == 0L) return(.clh_image_normalize_index(index))
  .clh_image_mark_dirty(image_root)
  for (record in records) {
    record$updated_at <- .clh_image_now()
    .clh_manifest_save(record, .clh_image_record_path(image_root, record$image_id))
    index <- .clh_image_index_add_record(index, record, image_root)
  }
  index$updated_at <- .clh_image_now()
  .clh_manifest_save(index, .clh_image_index_path(image_root))
  dirty <- .clh_image_dirty_path(image_root)
  if (file.exists(dirty)) unlink(dirty)
  index
}

.clh_image_reconcile_record <- function(record, image_root) {
  image_id <- record$image_id
  desc_dir <- file.path(image_root, image_id, "descriptions")
  metadata_files <- if (dir.exists(desc_dir)) {
    list.files(desc_dir, pattern = "\\.json$", full.names = TRUE)
  } else {
    character(0)
  }

  metadata_items <- list()
  for (path in metadata_files) {
    metadata <- .clh_manifest_load(path)
    id <- .clh_or(metadata$description_id, tools::file_path_sans_ext(basename(path)))
    metadata$description_id <- id
    metadata$image_id <- image_id
    metadata$metadata_file <- .clh_image_relative_path(image_root, path)
    text <- .clh_image_read_description(metadata, image_root)

    cleanup_checkpoint <- FALSE
    was_processing <- identical(metadata$status, "processing")
    if (was_processing) {
      checkpoint <- .clh_image_checkpoint_load(metadata, image_root)
      checkpoint_problem <- .clh_image_checkpoint_problem(checkpoint, image_id)
      if (!is.null(checkpoint) && is.null(checkpoint_problem)) {
        raw <- checkpoint$response
        checkpoint_text <- .clh_coerce_text(raw)
        checkpoint_error <- .clh_or(checkpoint$error, NULL)
        if (is.null(checkpoint_error) && !.clh_is_error_response(raw, checkpoint_text)) {
          .clh_atomic_write_lines(
            checkpoint_text,
            .clh_image_absolute_path(image_root, metadata$text_file)
          )
          metadata$status <- "processed"
          metadata$processed_at <- .clh_or(checkpoint$completed_at, .clh_image_now())
        } else {
          metadata$status <- "error"
          metadata$error_message <- .clh_error_response_message(
            raw,
            checkpoint_text,
            fallback = .clh_or(checkpoint_error, "unknown provider error")
          )
          metadata$processed_at <- .clh_or(checkpoint$completed_at, .clh_image_now())
        }
        metadata$elapsed_seconds <- .clh_or(checkpoint$duration_seconds, NA_real_)
        metadata$response_class <- paste(class(raw), collapse = "/")
        metadata$recovered_from <- "batch_checkpoint"
        cleanup_checkpoint <- TRUE
      } else if (!is.null(checkpoint_problem)) {
        metadata$status <- "interrupted"
        metadata$error_message <- paste0("The previous batch checkpoint is invalid: ", checkpoint_problem)
        cleanup_checkpoint <- TRUE
      } else if (.clh_image_valid_text(text)) {
        metadata$status <- "processed"
        metadata$processed_at <- .clh_image_now(file.info(
          .clh_image_absolute_path(image_root, metadata$text_file)
        )$mtime)
        metadata$recovered_from <- "interrupted_write"
      } else {
        metadata$status <- "interrupted"
        metadata$error_message <- "The previous run stopped before saving a valid description."
      }
      metadata$updated_at <- .clh_image_now()
      checkpoint_metadata <- metadata
      if (cleanup_checkpoint) metadata$checkpoint_file <- NULL
      .clh_manifest_save(metadata, path)
      if (cleanup_checkpoint) .clh_image_checkpoint_remove(checkpoint_metadata, image_root)
    } else if (identical(metadata$status, "processed") && !.clh_image_valid_text(text)) {
      metadata$status <- "missing_output"
      metadata$error_message <- "Description metadata exists, but its text artifact is missing or invalid."
      metadata$updated_at <- .clh_image_now()
      .clh_manifest_save(metadata, path)
    } else if (!is.null(metadata$checkpoint_file)) {
      checkpoint_metadata <- metadata
      metadata$checkpoint_file <- NULL
      metadata$updated_at <- .clh_image_now()
      .clh_manifest_save(metadata, path)
      .clh_image_checkpoint_remove(checkpoint_metadata, image_root)
    }
    if (was_processing && identical(metadata$status, "processed")) {
      record$active_description_id <- id
    }
    metadata_items[[id]] <- metadata
    record <- .clh_image_record_add_context(
      record,
      attachment_keys = .clh_or(metadata$attachment_key, character(0)),
      original_names = .clh_or(metadata$attachment, character(0)),
      sources = .clh_or(metadata$source_zip_id, character(0))
    )
  }

  if (length(metadata_items)) {
    for (id in names(metadata_items)) {
      record$descriptions[[id]] <- .clh_image_description_summary(metadata_items[[id]])
    }
  }

  active <- .clh_image_active_from_record(record, image_root)
  record$active_description_id <- if (is.null(active)) NA_character_ else active$id
  record$updated_at <- .clh_image_now()
  list(record = record, metadata = metadata_items)
}

.clh_image_index_rebuild <- function(image_root) {
  index <- .clh_image_empty_index()
  entries <- list.files(image_root, full.names = TRUE, no.. = TRUE)
  dirs <- entries[file.info(entries)$isdir %in% TRUE]

  .clh_image_mark_dirty(image_root)
  for (dir in dirs) {
    image_id <- basename(dir)
    if (!startsWith(image_id, "img_")) next
    record_path <- .clh_image_record_path(image_root, image_id)
    record <- if (file.exists(record_path)) {
      .clh_image_record_load(image_root, image_id)
    } else {
      .clh_image_empty_record(image_id)
    }
    reconciled <- .clh_image_reconcile_record(record, image_root)
    record <- reconciled$record
    .clh_manifest_save(record, record_path)
    index <- .clh_image_index_add_record(index, record, image_root)

    if (length(reconciled$metadata)) {
      for (metadata in reconciled$metadata) {
        configuration_id <- .clh_or(metadata$configuration_id, NULL)
        if (is.null(configuration_id) || !nzchar(configuration_id)) next
        index$configurations[[configuration_id]] <- list(
          configuration_id = configuration_id,
          service = .clh_or(metadata$service, NA_character_),
          model = .clh_or(metadata$model, NA_character_),
          prompt = .clh_or(metadata$prompt, NA_character_),
          prompt_hash = .clh_or(metadata$prompt_hash, NA_character_),
          argument_names = .clh_or(metadata$argument_names, character(0))
        )
      }
    }
  }

  index$updated_at <- .clh_image_now()
  .clh_manifest_save(index, .clh_image_index_path(image_root))
  dirty <- .clh_image_dirty_path(image_root)
  if (file.exists(dirty)) unlink(dirty)
  index
}

.clh_image_index_load <- function(image_root) {
  path <- .clh_image_index_path(image_root)
  dirty <- .clh_image_dirty_path(image_root)
  if (!file.exists(path) || file.exists(dirty)) {
    return(.clh_image_index_rebuild(image_root))
  }

  index <- .clh_manifest_load(path)
  valid <- identical(as.integer(.clh_or(index$schema_version, 0L)), .clh_image_schema_version()) &&
    identical(index$kind, "image_description_index") && is.list(index$images)
  if (!valid) return(.clh_image_index_rebuild(image_root))
  .clh_image_normalize_index(index)
}

.clh_image_configuration <- function(prompt, service, model, extra = list()) {
  prompt_hash <- .clh_object_md5(list(prompt = prompt))
  hash_extra <- extra
  extra_names <- names(hash_extra)
  if (length(hash_extra) > 1L && !is.null(extra_names) &&
    all(nzchar(extra_names)) && !anyDuplicated(extra_names)) {
    hash_extra <- hash_extra[order(extra_names)]
  }
  configuration_id <- paste0(
    "cfg_",
    .clh_object_md5(list(prompt = prompt, service = service, model = model, extra = hash_extra))
  )
  argument_names <- names(extra)
  if (is.null(argument_names)) argument_names <- rep("<unnamed>", length(extra))
  argument_names[!nzchar(argument_names)] <- "<unnamed>"
  list(
    configuration_id = configuration_id,
    service = if (is.null(service)) NA_character_ else as.character(service),
    model = if (is.null(model)) NA_character_ else as.character(model),
    prompt = if (is.null(prompt)) NA_character_ else as.character(prompt),
    prompt_hash = prompt_hash,
    argument_names = argument_names
  )
}

.clh_image_description_id <- function(record, configuration, image_root, time = Sys.time(), prefix = NULL) {
  stamp <- format(time, "%Y%m%dT%H%M%OS3", tz = "UTC")
  stamp <- paste0(gsub("[^0-9T]", "", stamp), "Z")
  service <- substr(.clh_path_slug(configuration$service, "default_service"), 1L, 20L)
  model <- substr(.clh_path_slug(configuration$model, "default_model"), 1L, 32L)
  prompt_hash <- substr(configuration$prompt_hash, 1L, 8L)
  base <- paste(c(prefix, stamp, service, model, prompt_hash), collapse = "_")
  id <- base
  suffix <- 1L
  while (!is.null(record$descriptions[[id]]) ||
    file.exists(file.path(.clh_image_description_dir(image_root, record$image_id), paste0(id, ".json"))) ||
    file.exists(file.path(.clh_image_description_dir(image_root, record$image_id), paste0(id, ".txt")))) {
    suffix <- suffix + 1L
    id <- paste0(base, "_", sprintf("%02d", suffix))
  }
  id
}

.clh_image_attempt_metadata <- function(record,
                                        configuration,
                                        image_root,
                                        attachment_key,
                                        attachment,
                                        zip_id,
                                        status = "processing",
                                        time = Sys.time(),
                                        prefix = NULL,
                                        description_id = NULL) {
  if (is.null(description_id)) {
    description_id <- .clh_image_description_id(
      record,
      configuration,
      image_root,
      time = time,
      prefix = prefix
    )
  }
  desc_dir <- .clh_image_description_dir(image_root, record$image_id)
  text_path <- file.path(desc_dir, paste0(description_id, ".txt"))
  metadata_path <- file.path(desc_dir, paste0(description_id, ".json"))
  list(
    schema_version = 1L,
    description_id = description_id,
    image_id = record$image_id,
    status = status,
    created_at = .clh_image_now(time),
    updated_at = .clh_image_now(time),
    service = configuration$service,
    model = configuration$model,
    prompt = configuration$prompt,
    prompt_hash = configuration$prompt_hash,
    configuration_id = configuration$configuration_id,
    argument_names = configuration$argument_names,
    attachment_key = attachment_key,
    attachment = attachment,
    source_zip_id = if (is.null(zip_id)) NA_character_ else zip_id,
    text_file = .clh_image_relative_path(image_root, text_path),
    metadata_file = .clh_image_relative_path(image_root, metadata_path)
  )
}

.clh_image_stage_attempt <- function(record,
                                     metadata,
                                     image_root,
                                     text = NULL,
                                     activate = FALSE) {
  if (!is.null(text)) {
    .clh_atomic_write_lines(
      text,
      .clh_image_absolute_path(image_root, metadata$text_file)
    )
  }
  .clh_manifest_save(
    metadata,
    .clh_image_absolute_path(image_root, metadata$metadata_file)
  )
  record$descriptions[[metadata$description_id]] <- .clh_image_description_summary(metadata)
  if (activate) record$active_description_id <- metadata$description_id
  record$updated_at <- .clh_image_now()
  record
}

.clh_image_commit_attempt <- function(record,
                                      metadata,
                                      index,
                                      image_root,
                                      text = NULL,
                                      activate = FALSE) {
  .clh_image_mark_dirty(image_root)
  record <- .clh_image_stage_attempt(
    record,
    metadata,
    image_root,
    text = text,
    activate = activate
  )
  .clh_manifest_save(record, .clh_image_record_path(image_root, record$image_id))
  index <- .clh_image_index_add_record(index, record, image_root)
  index$updated_at <- .clh_image_now()
  .clh_manifest_save(index, .clh_image_index_path(image_root))
  dirty <- .clh_image_dirty_path(image_root)
  if (file.exists(dirty)) unlink(dirty)
  list(record = record, index = index)
}

.clh_image_migrate_legacy_item <- function(record,
                                           image_root,
                                           attachment_key,
                                           attachment,
                                           zip_id,
                                           entry = NULL,
                                           flat_path = NA_character_) {
  text <- NA_character_
  if (!is.null(entry) && .clh_image_valid_text(.clh_or(entry$description, NA_character_))) {
    text <- as.character(entry$description)
  } else if (!is.na(flat_path) && file.exists(flat_path)) {
    candidate <- paste(readLines(flat_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (.clh_image_valid_text(candidate)) text <- candidate
  }

  legacy_status <- .clh_or(entry$status, NA_character_)
  status <- if (.clh_image_valid_text(text)) {
    "processed"
  } else if (!is.na(legacy_status) && legacy_status %in% c("error", "missing", "interrupted")) {
    legacy_status
  } else {
    return(list(record = record, configuration = NULL, migrated = FALSE))
  }

  prompt <- .clh_or(entry$prompt, NA_character_)
  service <- .clh_or(entry$service, NA_character_)
  model <- .clh_or(entry$model, NA_character_)
  configuration <- .clh_image_configuration(
    prompt = prompt,
    service = service,
    model = model,
    extra = list(cache_format = "legacy")
  )
  description_id <- paste0(
    "legacy_",
    substr(.clh_object_md5(list(
      attachment_key = attachment_key,
      service = service,
      model = model,
      prompt = prompt,
      text = text,
      status = status
    )), 1L, 24L)
  )
  if (!is.null(record$descriptions[[description_id]])) {
    return(list(record = record, configuration = configuration, migrated = FALSE))
  }

  cached_time <- if (!is.na(flat_path) && file.exists(flat_path)) {
    file.info(flat_path)$mtime
  } else {
    Sys.time()
  }
  metadata <- .clh_image_attempt_metadata(
    record = record,
    configuration = configuration,
    image_root = image_root,
    attachment_key = attachment_key,
    attachment = attachment,
    zip_id = zip_id,
    status = status,
    time = cached_time,
    description_id = description_id
  )
  metadata$recovered_from <- if (!is.null(entry)) "legacy_manifest" else "legacy_flat_file"
  metadata$legacy_file <- if (!is.na(flat_path)) flat_path else NA_character_
  metadata$legacy_md5 <- if (!is.na(flat_path) && file.exists(flat_path)) {
    unname(tools::md5sum(flat_path))
  } else {
    NA_character_
  }
  if (!is.null(entry$processed_at)) metadata$processed_at <- entry$processed_at
  if (identical(status, "processed") && is.null(metadata$processed_at)) {
    metadata$processed_at <- .clh_image_now(cached_time)
  }
  if (!is.null(entry$error_message)) metadata$error_message <- entry$error_message

  record <- .clh_image_stage_attempt(
    record,
    metadata,
    image_root,
    text = if (identical(status, "processed")) text else NULL,
    activate = identical(status, "processed")
  )
  list(record = record, configuration = configuration, migrated = TRUE)
}

.clh_image_index_active <- function(index, image_id, image_root) {
  image <- index$images[[image_id]]
  if (is.null(image)) return(NULL)
  record <- .clh_image_normalize_record(image, image_id)
  .clh_image_active_from_record(record, image_root)
}

.clh_image_index_configuration <- function(index,
                                           image_id,
                                           configuration_id,
                                           image_root) {
  image <- index$images[[image_id]]
  if (is.null(image) || length(image$descriptions) == 0L) return(NULL)
  ids <- names(image$descriptions)
  if (is.null(ids)) return(NULL)

  for (id in rev(ids)) {
    description <- image$descriptions[[id]]
    if (!identical(description$status, "processed") ||
      !identical(description$configuration_id, configuration_id)) {
      next
    }
    text <- .clh_image_read_description(description, image_root)
    if (.clh_image_valid_text(text)) {
      return(list(id = id, description = description, text = text))
    }
  }
  NULL
}
