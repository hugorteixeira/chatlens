.clh_test_image_chat <- function(paths, chat_key = "parallel_images") {
  keys <- paste0("path:", paths)
  df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00", tz = "UTC") + seq_along(paths),
    sender = rep("A", length(paths)),
    text = paste(basename(paths), "(file attached)"),
    message_id = seq_along(paths),
    attachment = basename(paths),
    attachment_type = rep("image", length(paths)),
    attachment_path = paths,
    attachment_key = keys,
    stringsAsFactors = FALSE
  )
  df$attachments <- as.list(df$attachment)
  df$attachment_types <- lapply(seq_along(paths), function(...) "image")
  df$attachment_paths <- as.list(paths)
  df$attachment_keys <- as.list(keys)
  df$attachment_statuses <- lapply(seq_along(paths), function(...) "present")
  chatlens:::.clh_new_chat(df, chat_key = chat_key, zip_id = "zip1")
}

.clh_test_batch_results <- function(image_paths, reverse = FALSE) {
  ids <- names(image_paths)
  if (reverse) ids <- rev(ids)
  out <- lapply(ids, function(id) {
    list(
      response_value = paste("description", basename(image_paths[[id]])),
      status_api = "SUCCESS",
      status_msg = "OK",
      duration = 0.01,
      task_id = id
    )
  })
  names(out) <- ids
  out$combined_stats <- list(workers_used = length(ids), qty_solicited = length(ids))
  out
}

test_that("image batch preflight validates the live genflow API", {
  expect_invisible(chatlens:::.clh_assert_genflow_image_batch_compatibility())

  legacy_exports <- list(
    get_provider = function(id) list(id = id),
    set_agent = function(...) list(),
    gen_batch_agent = function(agent, qty = 8, ...) list(),
    gen_batch = function(qty = 8, instructions = NULL) list()
  )
  issues <- chatlens:::.clh_genflow_image_batch_issues(legacy_exports)
  expect_true(any(grepl("set_agent.*name", issues)))
  expect_true(any(grepl("gen_batch_agent.*workers", issues)))
  expect_true(any(grepl("gen_batch_agent.*backend", issues)))
  expect_true(any(grepl("gen_batch.*agent", issues)))

  testthat::local_mocked_bindings(
    .clh_genflow_image_batch_exports = function() legacy_exports,
    .clh_genflow_runtime_info = function() {
      list(
        loaded_version = "0.0.1",
        installed_version = "0.0.5",
        package_path = "/tmp/mock-genflow"
      )
    },
    .package = "chatlens"
  )
  expect_error(
    chatlens:::.clh_assert_genflow_image_batch_compatibility(),
    paste0(
      "incompatible with Chatlens parallel image batches.*",
      "Loaded namespace version: 0.0.1; installed package version: 0.0.5.*",
      "compatible genflow package on disk differs.*Restart"
    )
  )
})

test_that("genflow preflight recommends install when the disk version is too old", {
  modern_exports <- list(
    get_provider = function(id) list(id = id),
    set_agent = function(name, setup, content, save, assign) list(),
    gen_batch_agent = function(agent, qty, workers, backend, add_img_each,
                               checkpoint_each, persist, verbose, log,
                               always_fix_errors) list(),
    gen_batch = function(qty, agent, workers, backend, add_img_each, checkpoint_each,
                         persist, verbose, log, always_fix_errors) list()
  )
  expect_length(chatlens:::.clh_genflow_image_batch_issues(modern_exports), 0L)

  testthat::local_mocked_bindings(
    .clh_genflow_image_batch_exports = function() modern_exports,
    .clh_genflow_runtime_info = function() {
      list(
        loaded_version = "0.0.4",
        installed_version = "0.0.4",
        package_path = "/tmp/mock-old-genflow"
      )
    },
    .package = "chatlens"
  )
  expect_error(
    chatlens:::.clh_assert_genflow_image_batch_compatibility(),
    "older than 0.0.5.*Install or reinstall genflow >= 0.0.5"
  )
})

test_that("incompatible genflow stops before image cache access", {
  cache_dir <- tempfile("chatlens_untouched_cache_")
  path <- tempfile("EARLY_PREFLIGHT_", fileext = ".jpg")
  writeLines("image bytes", path)
  on.exit(unlink(c(cache_dir, path), recursive = TRUE), add = TRUE)
  chat <- .clh_test_image_chat(path, chat_key = "early_genflow_preflight")
  cache_touched <- FALSE

  testthat::local_mocked_bindings(
    .clh_assert_genflow_image_batch_compatibility = function() {
      stop("mock incompatible live genflow", call. = FALSE)
    },
    .clh_resolve_store_dir = function(...) {
      cache_touched <<- TRUE
      stop("cache should not be resolved")
    },
    .package = "chatlens"
  )

  expect_error(
    cl_chat_describe_images(chat, cache_dir = cache_dir, verbose = FALSE),
    "mock incompatible live genflow"
  )
  expect_false(cache_touched)
  expect_false(dir.exists(cache_dir))
})

test_that("image descriptions use bounded batches and map named results", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, paste0("IMAGE_", 1:10, ".jpg"))
  for (i in seq_along(paths)) writeLines(paste("image bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths)

  batch_sizes <- integer(0)
  batch_workers <- integer(0)
  agents <- list()
  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      batch_sizes <<- c(batch_sizes, length(image_paths))
      batch_workers <<- c(batch_workers, workers)
      agents[[length(agents) + 1L]] <<- agent
      .clh_test_batch_results(image_paths, reverse = TRUE)
    },
    .package = "chatlens"
  )

  out <- cl_chat_describe_images(
    chat,
    prompt = "Parallel prompt",
    service = "openrouter",
    model = "vision-model",
    cache_dir = cache_dir,
    workers = 2,
    cache_mode = "configuration",
    verbose = FALSE,
    reasoning = "high"
  )

  expect_identical(batch_sizes, c(2L, 8L))
  expect_identical(batch_workers, c(2L, 2L))
  expect_true(all(vapply(agents, `[[`, character(1), "service") == "openrouter"))
  expect_true(all(vapply(agents, `[[`, character(1), "model") == "vision-model"))
  expect_true(all(vapply(agents, `[[`, character(1), "reasoning") == "high"))
  expect_identical(out$image_description, paste("description", basename(paths)))

  store <- chatlens:::.clh_chat_store_dir("parallel_images", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  manifest <- jsonlite::read_json(file.path(image_root, "image_manifest.json"), simplifyVector = FALSE)
  expect_true(all(vapply(manifest$images, function(x) identical(x$status, "processed"), logical(1))))
  expect_length(list.files(image_root, pattern = "\\.result\\.rds$", recursive = TRUE), 0L)

  batch_sizes <- integer(0)
  cached <- cl_chat_describe_images(
    chat,
    prompt = "Parallel prompt",
    service = "openrouter",
    model = "vision-model",
    cache_dir = cache_dir,
    workers = 5,
    cache_mode = "configuration",
    verbose = FALSE,
    reasoning = "high"
  )
  expect_length(batch_sizes, 0L)
  expect_identical(cached$image_description, out$image_description)
})

test_that("unsupported image runtime arguments fail before cache staging", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  path <- file.path(cache_dir, "UNKNOWN_ARG.jpg")
  writeLines("image bytes", path)
  chat <- .clh_test_image_chat(path, chat_key = "unknown_image_arg")
  calls <- 0L
  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(...) {
      calls <<- calls + 1L
      stop("provider should not be called")
    },
    .package = "chatlens"
  )

  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      verbose = FALSE,
      provider_magic = TRUE
    ),
    "Unsupported image-description argument.*provider_magic"
  )
  expect_equal(calls, 0L)
  expect_false(dir.exists(file.path(cache_dir, "whatsapp", "chats", "unknown_image_arg")))

  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      verbose = FALSE,
      reasoning = "definitely-invalid"
    ),
    "Invalid image-description argument `reasoning`"
  )
  expect_error(
    cl_chat_describe_images(
      chat,
      service = "openai",
      cache_dir = cache_dir,
      verbose = FALSE,
      reasoning = "high"
    ),
    "OpenAI reasoning path ignores image attachments"
  )
  expect_error(
    cl_chat_describe_images(
      chat,
      service = "groq",
      cache_dir = cache_dir,
      verbose = FALSE,
      reasoning = "high"
    ),
    "service = .*groq.*does not apply it"
  )
  expect_error(
    cl_chat_describe_images(
      chat,
      service = "ollama",
      cache_dir = cache_dir,
      verbose = FALSE,
      tools = TRUE,
      my_tools = list()
    ),
    "service = .*ollama.*does not apply them"
  )
  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      verbose = FALSE,
      my_tools = list()
    ),
    "set `tools = TRUE`"
  )
  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      verbose = FALSE,
      tools = TRUE
    ),
    "provide definitions in `my_tools`"
  )
  expect_error(
    cl_chat_describe_images(chat, service = "", cache_dir = cache_dir, verbose = FALSE),
    "service must be one non-empty"
  )
  expect_error(
    cl_chat_describe_images(chat, model = "", cache_dir = cache_dir, verbose = FALSE),
    "model must be one non-empty"
  )
  expect_error(
    cl_chat_describe_images(
      chat,
      service = "definitely-not-a-provider",
      cache_dir = cache_dir,
      verbose = FALSE
    ),
    "Unsupported image-description service"
  )
  expect_equal(calls, 0L)
})

test_that("image provider aliases use genflow canonical identifiers", {
  aliases <- c(
    claude = "anthropic",
    `llama-cpp` = "llamacpp",
    `fireworks-ai` = "fireworks",
    pplx = "perplexity"
  )
  resolved <- vapply(names(aliases), function(alias) {
    chatlens:::.clh_image_provider(alias)$id
  }, character(1))
  expect_identical(unname(resolved), unname(aliases))
})

test_that("custom provider capabilities control image runtime options", {
  supported_provider <- list(
    id = "custom-vision",
    kind = "openai_compat",
    supports_vision = TRUE,
    supports_tools = TRUE,
    supports_reasoning = TRUE,
    supports_plugins = TRUE,
    reasoning_field = "reasoning",
    plugins_field = "plugins"
  )
  testthat::local_mocked_bindings(
    get_provider = function(...) supported_provider,
    .package = "genflow"
  )
  expect_no_error(chatlens:::.clh_validate_image_extra(
    list(
      reasoning = "high",
      plugins = list(list(id = "plugin")),
      tools = list(list(type = "function"))
    ),
    service = "custom-vision"
  ))

  unsupported_provider <- supported_provider
  unsupported_provider$supports_vision <- FALSE
  testthat::local_mocked_bindings(
    get_provider = function(...) unsupported_provider,
    .package = "genflow"
  )
  expect_error(
    chatlens:::.clh_validate_image_extra(list(), service = "custom-no-vision"),
    "supports_vision = FALSE"
  )

  missing_fields_provider <- supported_provider
  missing_fields_provider$supports_vision <- TRUE
  missing_fields_provider$reasoning_field <- NULL
  missing_fields_provider$plugins_field <- NULL
  testthat::local_mocked_bindings(
    get_provider = function(...) missing_fields_provider,
    .package = "genflow"
  )
  expect_error(
    chatlens:::.clh_validate_image_extra(
      list(reasoning = "high"),
      service = "custom-missing-fields"
    ),
    "does not apply it"
  )

})

test_that("supported image runtime arguments reach the ephemeral agent and fingerprint", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  path <- file.path(cache_dir, "RUNTIME_ARGS.jpg")
  writeLines("image bytes", path)
  chat <- .clh_test_image_chat(path, chat_key = "image_runtime_args")
  agents <- list()
  extra <- list(
    add = "extra context",
    temp = 0.25,
    reasoning = "low",
    tools = TRUE,
    plugins = list(list(id = "web")),
    my_tools = list(list(type = "function")),
    timeout_api = 45,
    null_repeat = FALSE
  )
  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      agents[[length(agents) + 1L]] <<- agent
      .clh_test_batch_results(image_paths)
    },
    .package = "chatlens"
  )

  out <- do.call(
    cl_chat_describe_images,
    c(
      list(
        chat = chat,
        prompt = "Runtime argument prompt",
        service = "openrouter",
        model = "vision-model",
        cache_dir = cache_dir,
        workers = 1,
        verbose = FALSE,
        cache_mode = "configuration"
      ),
      extra
    )
  )
  expect_identical(out$image_description, "description RUNTIME_ARGS.jpg")
  expect_length(agents, 1L)
  for (name in names(extra)) expect_identical(agents[[1]][[name]], extra[[name]])

  expected <- chatlens:::.clh_image_configuration(
    "Runtime argument prompt", "openrouter", "vision-model", extra = extra
  )
  changed <- extra
  changed$timeout_api <- 46
  expect_false(identical(
    expected$configuration_id,
    chatlens:::.clh_image_configuration(
      "Runtime argument prompt", "openrouter", "vision-model", extra = changed
    )$configuration_id
  ))
  store <- chatlens:::.clh_chat_store_dir("image_runtime_args", cache_dir = cache_dir)
  manifest <- jsonlite::read_json(
    file.path(store, "image_descriptions", "image_manifest.json"),
    simplifyVector = FALSE
  )
  expect_true(expected$configuration_id %in% names(manifest$configurations))
  expect_setequal(
    manifest$configurations[[expected$configuration_id]]$argument_names,
    names(extra)
  )
})

test_that("worker errors stay attached to the correct image", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, c("ERROR_A.jpg", "SUCCESS_B.jpg"))
  for (i in seq_along(paths)) writeLines(paste("image bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths, chat_key = "worker_error_mapping")

  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      ids <- names(image_paths)
      responses <- vector("list", length(ids))
      names(responses) <- rev(ids)
      detailed_errors <- vector("list", length(ids))
      for (i in seq_along(ids)) {
        image_id <- ids[i]
        failed <- identical(i, 1L)
        response <- if (failed) {
          NULL
        } else {
          list(
            response_value = "description for image B",
            status_api = "SUCCESS",
            status_msg = "OK",
            duration = 0.02,
            task_id = image_id
          )
        }
        responses[image_id] <- list(response)
        checkpoint_error <- if (failed) "provider exploded A" else NULL
        detailed_errors[i] <- list(if (failed) "Internal worker error: provider exploded A" else NULL)
        saveRDS(
          list(
            schema_version = 1L,
            task_id = image_id,
            completed_at = chatlens:::.clh_image_now(),
            response = response,
            error = checkpoint_error,
            duration_seconds = 0.02
          ),
          checkpoint_paths[[image_id]]
        )
      }
      responses$combined_stats <- list(
        workers_used = workers,
        qty_solicited = length(ids),
        detailed_errors = detailed_errors
      )
      responses
    },
    .package = "chatlens"
  )

  out <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    workers = 2,
    verbose = FALSE
  )
  expect_true(is.na(out$image_description[1]))
  expect_identical(out$image_description[2], "description for image B")

  store <- chatlens:::.clh_chat_store_dir("worker_error_mapping", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  index <- chatlens:::.clh_image_index_load(image_root)
  first_id <- index$attachment_index[[paste0("path:", paths[1])]]
  second_id <- index$attachment_index[[paste0("path:", paths[2])]]
  first_record <- chatlens:::.clh_image_record_load(image_root, first_id)
  second_record <- chatlens:::.clh_image_record_load(image_root, second_id)
  first_attempt <- first_record$descriptions[[1]]
  second_attempt <- second_record$descriptions[[1]]
  expect_equal(first_attempt$status, "error")
  expect_match(first_attempt$error_message, "provider exploded A", fixed = TRUE)
  expect_equal(second_attempt$status, "processed")
  expect_length(list.files(image_root, pattern = "\\.result\\.rds$", recursive = TRUE), 0L)
})

test_that("real genflow batch preserves an internal provider error", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  path <- file.path(cache_dir, "REAL_BATCH_ERROR.jpg")
  writeLines("image bytes", path)
  chat <- .clh_test_image_chat(path, chat_key = "real_batch_worker_error")

  # Batch validation inspects the provider function's original arguments.
  provider_error <- genflow:::gen_txt.default
  body(provider_error) <- quote(stop("provider exploded in real batch"))
  testthat::local_mocked_bindings(
    gen_txt.default = provider_error,
    .package = "genflow"
  )

  out <- cl_chat_describe_images(
    chat,
    service = "openrouter",
    model = "mock-model",
    cache_dir = cache_dir,
    workers = 1,
    verbose = FALSE
  )
  expect_true(is.na(out$image_description[1]))

  store <- chatlens:::.clh_chat_store_dir("real_batch_worker_error", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  index <- chatlens:::.clh_image_index_load(image_root)
  image_id <- index$attachment_index[[paste0("path:", path)]]
  record <- chatlens:::.clh_image_record_load(image_root, image_id)
  attempt <- record$descriptions[[1]]
  expect_equal(attempt$status, "error")
  expect_match(attempt$error_message, "provider exploded in real batch", fixed = TRUE)
})

test_that("a fully rate-limited window stops the remaining queue", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, paste0("RATE_LIMIT_", 1:10, ".jpg"))
  for (i in seq_along(paths)) writeLines(paste("image bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths, chat_key = "rate_limit_queue")
  calls <- 0L

  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      calls <<- calls + 1L
      results <- lapply(names(image_paths), function(image_id) {
        list(
          response_value = "429 Too Many Requests",
          status_api = "ERROR",
          status_msg = "429 Too Many Requests: rate limit exceeded",
          duration = 0.01,
          task_id = image_id
        )
      })
      names(results) <- names(image_paths)
      results$combined_stats <- list(
        workers_used = workers,
        qty_solicited = length(image_paths),
        detailed_errors = vector("list", length(image_paths))
      )
      results
    },
    .package = "chatlens"
  )

  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      workers = 2,
      verbose = FALSE
    ),
    "Every request.*rate limit"
  )
  expect_equal(calls, 1L)
})

test_that("a uniformly failing provider stops after the warm-up batch", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, paste0("BAD_PROVIDER_", 1:10, ".jpg"))
  for (i in seq_along(paths)) writeLines(paste("image bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths, chat_key = "bad_provider_queue")
  calls <- 0L
  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      calls <<- calls + 1L
      results <- lapply(names(image_paths), function(image_id) {
        list(
          response_value = "invalid model",
          status_api = "ERROR",
          status_msg = "invalid model selected for provider",
          duration = 0.01,
          task_id = image_id
        )
      })
      names(results) <- names(image_paths)
      results$combined_stats <- list(
        workers_used = workers,
        qty_solicited = length(image_paths),
        detailed_errors = vector("list", length(image_paths))
      )
      results
    },
    .package = "chatlens"
  )

  expect_error(
    cl_chat_describe_images(
      chat,
      service = "openrouter",
      model = "invalid-model",
      cache_dir = cache_dir,
      workers = 2,
      verbose = FALSE
    ),
    "Every request.*failed.*remaining queue was stopped"
  )
  expect_equal(calls, 1L)

  store <- chatlens:::.clh_chat_store_dir("bad_provider_queue", cache_dir = cache_dir)
  manifest <- jsonlite::read_json(
    file.path(store, "image_descriptions", "image_manifest.json"),
    simplifyVector = FALSE
  )
  attempt_count <- sum(vapply(manifest$images, function(image) {
    as.integer(image$attempt_count)
  }, integer(1)))
  expect_equal(attempt_count, 2L)
})

test_that("parallel backend failure variants share one circuit-breaker signature", {
  messages <- c(
    "Index 1: No valid result received or mapped from worker.",
    "Worker 2 returned NULL.",
    "4 parallel jobs did not deliver results",
    "16 parallel function calls did not deliver results"
  )
  signatures <- vapply(messages, function(message) {
    chatlens:::.clh_image_failure_signature(
      message,
      raw = list(status_api = "ERROR"),
      checkpoint = list(error = message)
    )
  }, character(1))
  expect_length(unique(signatures), 1L)
  expect_identical(unique(signatures), "parallel_backend_no_task_result")

  expect_identical(
    chatlens:::.clh_image_failure_signature(
      "scheduler supplied no specific error",
      raw = NULL,
      checkpoint = NULL
    ),
    "parallel_backend_no_task_result"
  )
  expect_identical(
    chatlens:::.clh_image_failure_signature(
      "400 Bad Request: image bytes are unreadable",
      raw = list(status_api = "ERROR"),
      checkpoint = list(error = "400 Bad Request: image bytes are unreadable")
    ),
    "400 Bad Request: image bytes are unreadable"
  )
})

test_that("parallel backend deaths stop after warm-up and retain per-image errors", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, paste0("DEAD_WORKER_", 1:10, ".jpg"))
  for (i in seq_along(paths)) writeLines(paste("image bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths, chat_key = "dead_worker_queue")
  calls <- 0L

  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      calls <<- calls + 1L
      ids <- names(image_paths)
      results <- stats::setNames(vector("list", length(ids)), ids)
      results$combined_stats <- list(
        workers_used = workers,
        qty_solicited = length(ids),
        detailed_errors = lapply(seq_along(ids), function(i) {
          paste0("Index ", i, ": No valid result received or mapped from worker.")
        })
      )
      results
    },
    .package = "chatlens"
  )

  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      workers = 2,
      verbose = FALSE
    ),
    "Every request.*failed.*remaining queue was stopped"
  )
  expect_equal(calls, 1L)

  store <- chatlens:::.clh_chat_store_dir("dead_worker_queue", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  index <- chatlens:::.clh_image_index_load(image_root)
  image_ids <- vapply(paths[1:2], function(path) {
    index$attachment_index[[paste0("path:", path)]]
  }, character(1))
  attempts <- lapply(image_ids, function(image_id) {
    chatlens:::.clh_image_record_load(image_root, image_id)$descriptions[[1]]
  })
  expect_true(all(vapply(attempts, function(x) identical(x$status, "error"), logical(1))))
  expect_identical(
    unname(vapply(attempts, `[[`, character(1), "error_message")),
    paste0(
      "Index ", 1:2,
      ": No valid result received or mapped from worker."
    )
  )

  manifest <- jsonlite::read_json(
    file.path(image_root, "image_manifest.json"),
    simplifyVector = FALSE
  )
  attempt_count <- sum(vapply(manifest$images, function(image) {
    as.integer(image$attempt_count)
  }, integer(1)))
  expect_equal(attempt_count, 2L)
  expect_length(list.files(image_root, pattern = "\\.result\\.rds$", recursive = TRUE), 0L)
})

test_that("distinct per-image errors do not trip the provider circuit breaker", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, paste0("MIXED_ERRORS_", 1:4, ".jpg"))
  for (i in seq_along(paths)) writeLines(paste("image bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths, chat_key = "mixed_error_queue")
  calls <- 0L

  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      calls <<- calls + 1L
      if (calls > 1L) return(.clh_test_batch_results(image_paths))
      messages <- c(
        "400 Bad Request: image bytes are unreadable",
        "400 Bad Request: content rejected for this image"
      )
      results <- lapply(seq_along(image_paths), function(i) {
        list(
          response_value = messages[i],
          status_api = "ERROR",
          status_msg = messages[i],
          duration = 0.01,
          task_id = names(image_paths)[i]
        )
      })
      names(results) <- names(image_paths)
      results$combined_stats <- list(
        workers_used = workers,
        qty_solicited = length(image_paths),
        detailed_errors = vector("list", length(image_paths))
      )
      results
    },
    .package = "chatlens"
  )

  out <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    workers = 2,
    verbose = FALSE
  )
  expect_equal(calls, 2L)
  expect_true(all(is.na(out$image_description[1:2])))
  expect_true(all(!is.na(out$image_description[3:4])))
})

test_that("completed worker checkpoints recover after an interrupted batch", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  path <- file.path(cache_dir, "RECOVER.jpg")
  writeLines("recoverable image", path)
  chat <- .clh_test_image_chat(path, chat_key = "checkpoint_recovery")

  calls <- 0L
  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      calls <<- calls + 1L
      .clh_test_batch_results(image_paths)
    },
    .package = "chatlens"
  )
  initial <- cl_chat_describe_images(
    chat,
    prompt = "Initial prompt",
    model = "model-a",
    cache_dir = cache_dir,
    cache_mode = "force",
    verbose = FALSE
  )
  expect_equal(calls, 1L)

  store <- chatlens:::.clh_chat_store_dir("checkpoint_recovery", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  index <- chatlens:::.clh_image_index_load(image_root)
  key <- paste0("path:", path)
  image_id <- index$attachment_index[[key]]
  record <- chatlens:::.clh_image_record_load(image_root, image_id)
  configuration <- chatlens:::.clh_image_configuration(
    "Recovered prompt",
    "openrouter",
    "model-b"
  )
  index <- chatlens:::.clh_image_index_register_configuration(index, configuration)
  metadata <- chatlens:::.clh_image_attempt_metadata(
    record,
    configuration,
    image_root,
    attachment_key = key,
    attachment = basename(path),
    zip_id = "zip1",
    status = "processing"
  )
  checkpoint_path <- file.path(
    chatlens:::.clh_image_description_dir(image_root, image_id),
    paste0(metadata$description_id, ".result.rds")
  )
  metadata$checkpoint_file <- chatlens:::.clh_image_relative_path(image_root, checkpoint_path)
  committed <- chatlens:::.clh_image_commit_attempt(record, metadata, index, image_root)
  saveRDS(
    list(
      schema_version = 1L,
      task_id = image_id,
      completed_at = chatlens:::.clh_image_now(),
      response = list(
        response_value = "description recovered from worker",
        status_api = "SUCCESS",
        status_msg = "OK",
        duration = 2
      ),
      error = NULL,
      duration_seconds = 2
    ),
    checkpoint_path
  )

  recovered <- cl_chat_describe_images(
    initial,
    prompt = "Recovered prompt",
    model = "model-b",
    cache_dir = cache_dir,
    cache_mode = "configuration",
    verbose = FALSE
  )
  expect_equal(calls, 1L)
  expect_identical(recovered$image_description, "description recovered from worker")
  expect_false(file.exists(checkpoint_path))
  recovered_metadata <- jsonlite::read_json(
    file.path(image_root, metadata$metadata_file),
    simplifyVector = FALSE
  )
  expect_equal(recovered_metadata$status, "processed")
  expect_equal(recovered_metadata$recovered_from, "batch_checkpoint")
})

test_that("checkpoint recovery rejects a mismatched image identifier", {
  checkpoint <- list(
    schema_version = 1L,
    task_id = "img_other",
    completed_at = chatlens:::.clh_image_now(),
    response = list(response_value = "wrong image"),
    error = NULL,
    duration_seconds = 1
  )
  expect_match(
    chatlens:::.clh_image_checkpoint_problem(checkpoint, "img_expected"),
    "task_id does not match img_expected",
    fixed = TRUE
  )
})

test_that("completed batches survive a later batch failure and resume", {
  cache_dir <- tempfile("chatlens_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  paths <- file.path(cache_dir, paste0("RESUME_", 1:10, ".jpg"))
  for (i in seq_along(paths)) writeLines(paste("resume bytes", i), paths[i])
  chat <- .clh_test_image_chat(paths, chat_key = "batch_resume")

  batch_number <- 0L
  submitted <- integer(0)
  fail_second <- TRUE
  testthat::local_mocked_bindings(
    .clh_run_image_batch = function(agent, image_paths, workers, checkpoint_paths) {
      batch_number <<- batch_number + 1L
      submitted <<- c(submitted, length(image_paths))
      if (fail_second && batch_number == 2L) stop("simulated batch failure")
      .clh_test_batch_results(image_paths)
    },
    .package = "chatlens"
  )

  expect_error(
    cl_chat_describe_images(
      chat,
      cache_dir = cache_dir,
      workers = 2,
      cache_mode = "missing",
      verbose = FALSE
    ),
    "simulated batch failure"
  )
  expect_identical(submitted, c(2L, 8L))

  store <- chatlens:::.clh_chat_store_dir("batch_resume", cache_dir = cache_dir)
  image_root <- file.path(store, "image_descriptions")
  failed_manifest <- jsonlite::read_json(
    file.path(image_root, "image_manifest.json"),
    simplifyVector = FALSE
  )
  failed_statuses <- vapply(failed_manifest$images, `[[`, character(1), "status")
  expect_identical(as.integer(table(failed_statuses)), c(8L, 2L))
  expect_identical(names(table(failed_statuses)), c("error", "processed"))
  expect_false(any(failed_statuses == "processing"))
  expect_length(list.files(image_root, pattern = "\\.result\\.rds$", recursive = TRUE), 0L)

  fail_second <- FALSE
  batch_number <- 0L
  submitted <- integer(0)
  resumed <- cl_chat_describe_images(
    chat,
    cache_dir = cache_dir,
    workers = 2,
    cache_mode = "missing",
    verbose = FALSE
  )
  expect_identical(submitted, c(2L, 6L))
  expect_true(all(!is.na(resumed$image_description)))

  manifest <- jsonlite::read_json(
    file.path(store, "image_descriptions", "image_manifest.json"),
    simplifyVector = FALSE
  )
  expect_true(all(vapply(manifest$images, function(x) identical(x$status, "processed"), logical(1))))
})
