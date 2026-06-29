# Manifest helpers

.clh_manifest_load <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(list(updated_at = NULL, items = list()))
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

.clh_manifest_save <- function(manifest, path) {
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(TRUE)
}

.clh_runs_dir <- function(chat_key, cache_dir = NULL) {
  base <- .clh_chat_store_dir(chat_key, cache_dir)
  if (is.null(base)) return(NULL)
  .clh_ensure_dir(file.path(base, "runs"))
}

.clh_run_log_path <- function(chat_key,
                                  zip_id,
                                  kind = c("audio", "image"),
                                  zip_name = NULL,
                                  cache_dir = NULL) {
  kind <- match.arg(kind)
  runs_dir <- .clh_runs_dir(chat_key, cache_dir)
  if (is.null(runs_dir)) return(NULL)
  if (!is.null(zip_name) && nzchar(zip_name)) {
    base <- tolower(tools::file_path_sans_ext(basename(zip_name)))
    base <- gsub("[^a-z0-9]+", "_", base)
    base <- gsub("^_+|_+$", "", base)
    fname <- sprintf("%s_%s_%s.json", kind, base, zip_id)
  } else {
    fname <- sprintf("%s_%s.json", kind, zip_id)
  }
  file.path(runs_dir, fname)
}
