.clh_test_gen_batch_agent <- function(agent,
                                      qty,
                                      add_img_each,
                                      workers = NULL,
                                      backend = "psock",
                                      checkpoint_each = NULL,
                                      persist = FALSE,
                                      verbose = FALSE,
                                      log = FALSE,
                                      always_fix_errors = FALSE,
                                      ...) {
  stopifnot(length(add_img_each) == qty)
  stopifnot(identical(backend, "psock"))
  results <- lapply(seq_len(qty), function(i) {
    raw <- genflow::gen_txt(
      prompt = agent$context,
      add_img = add_img_each[[i]]
    )
    result <- if (is.list(raw) && !is.null(raw$status_api)) {
      raw
    } else {
      text <- chatlens:::.clh_coerce_text(raw)
      failed <- chatlens:::.clh_is_error_response(raw, text)
      list(
        response_value = raw,
        status_api = if (failed) "ERROR" else "SUCCESS",
        status_msg = if (failed) chatlens:::.clh_error_response_message(raw, text) else "OK",
        duration = 0.01
      )
    }
    result$task_id <- names(add_img_each)[i]
    if (!is.null(checkpoint_each)) {
      saveRDS(
        list(
          schema_version = 1L,
          task_id = result$task_id,
          completed_at = chatlens:::.clh_image_now(),
          response = result,
          error = NULL,
          duration_seconds = result$duration
        ),
        checkpoint_each[[i]]
      )
    }
    result
  })
  names(results) <- names(add_img_each)
  results$combined_stats <- list(
    workers_used = min(qty, if (is.null(workers)) qty else workers),
    qty_solicited = qty,
    detailed_errors = vector("list", qty)
  )
  results
}
