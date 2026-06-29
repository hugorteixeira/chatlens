# Anonymization helpers

.clh_detect_participants <- function(chat) {
  if (nrow(chat) == 0) return(character(0))
  unique(stats::na.omit(chat$sender))
}

.clh_alias_cache_path <- function(chat_key, cache_dir = NULL) {
  base <- .clh_chat_store_dir(chat_key, cache_dir)
  if (is.null(base)) return(NULL)
  file.path(base, "aliases.json")
}

.clh_save_alias_map <- function(alias_map, chat_key, cache_dir = NULL) {
  path <- .clh_alias_cache_path(chat_key, cache_dir)
  if (is.null(path)) return(invisible(FALSE))
  jsonlite::write_json(alias_map, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(TRUE)
}

.clh_load_alias_map <- function(chat_key, cache_dir = NULL) {
  path <- .clh_alias_cache_path(chat_key, cache_dir)
  if (is.null(path) || !file.exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

.clh_alias_pool <- function() {
  c(
    "Alex", "Blake", "Casey", "Drew", "Emery", "Finley", "Gray", "Harper",
    "Jamie", "Jordan", "Kai", "Logan", "Morgan", "Parker", "Quinn", "Reese",
    "Riley", "Rowan", "Skyler", "Taylor", "Avery", "Bailey", "Cameron", "Dylan",
    "Elliot", "Hayden", "Jules", "Kendall", "Lane", "Micah", "Noel", "Oakley",
    "Payton", "Reagan", "Sage", "Shawn", "Toby", "Val", "Wren", "Zion"
  )
}

.clh_suggest_aliases <- function(participants) {
  pool <- .clh_alias_pool()
  used <- character(0)
  aliases <- character(length(participants))

  for (i in seq_along(participants)) {
    name <- participants[i]
    idx <- (sum(utf8ToInt(name)) %% length(pool)) + 1
    alias <- pool[idx]
    # Ensure uniqueness
    while (alias %in% used) {
      idx <- (idx %% length(pool)) + 1
      alias <- pool[idx]
    }
    aliases[i] <- alias
    used <- c(used, alias)
  }
  aliases
}

.clh_build_alias_map <- function(participants,
                                alias_map = NULL,
                                interactive = FALSE,
                                chat_key = NULL,
                                cache_dir = NULL) {
  if (!is.null(alias_map)) {
    return(alias_map)
  }

  if (!is.null(chat_key)) {
    cached <- .clh_load_alias_map(chat_key, cache_dir)
    if (!is.null(cached)) return(cached)
  }

  suggested <- .clh_suggest_aliases(participants)
  entries <- vector("list", length(participants))

  for (i in seq_along(participants)) {
    person <- participants[i]
    alias <- suggested[i]
    variants <- person

    if (interactive && interactive()) {
      ans <- readline(sprintf("Alias for '%s' [%s]: ", person, alias))
      if (nzchar(ans)) alias <- ans
      more <- readline("Other variants/nicknames (comma-separated, optional): ")
      if (nzchar(more)) {
        extras <- trimws(unlist(strsplit(more, ",")))
        extras <- extras[nzchar(extras)]
        variants <- unique(c(variants, extras))
      }
    }

    entries[[i]] <- list(person = person, alias = alias, variants = unique(variants))
  }

  alias_map <- list(items = entries)

  if (!is.null(chat_key)) {
    .clh_save_alias_map(alias_map, chat_key, cache_dir)
  }

  alias_map
}

.clh_alias_replacements <- function(alias_map) {
  if (is.null(alias_map) || is.null(alias_map$items)) return(character(0))
  variants <- unlist(lapply(alias_map$items, function(x) x$variants))
  aliases <- unlist(lapply(alias_map$items, function(x) rep(x$alias, length(x$variants))))
  # Keep first occurrence if duplicates exist
  dedup <- !duplicated(variants)
  stats::setNames(aliases[dedup], variants[dedup])
}

#' Anonymize a chat using an alias map
#' @param chat A `chatlens_chat` object
#' @param alias_map Alias map (optional)
#' @param interactive Prompt for aliases
#' @param save_original Save a copy of the original chat in cache as `.rds`
#'   plus a mirrored `.txt` transcript
#' @param save_chat Save the current chat state in cache as `chat.rds` plus a
#'   mirrored `chat.txt` transcript
#' @param cache_dir Optional cache directory. When `NULL`, uses the chat's
#'   stored cache location or `~/.chatlens`.
#' @export
cl_chat_anonymize <- function(chat,
                                    alias_map = NULL,
                                    interactive = FALSE,
                                    save_original = TRUE,
                                    save_chat = TRUE,
                                    cache_dir = NULL) {
  if (!inherits(chat, "chatlens_chat")) stop("chat must be a chatlens_chat object")

  chat_key <- attr(chat, "chat_key")
  participants <- .clh_detect_participants(chat)

  alias_map <- .clh_build_alias_map(
    participants = participants,
    alias_map = alias_map,
    interactive = interactive,
    chat_key = chat_key,
    cache_dir = cache_dir
  )

  if (save_original && !is.null(chat_key)) {
    .clh_save_original_chat(chat, cache_dir = cache_dir, overwrite = FALSE)
  }

  save_current_chat <- function(chat) {
    if (save_chat) .clh_save_current_chat(chat, cache_dir = cache_dir) else chat
  }

  replacements <- .clh_alias_replacements(alias_map)
  if (length(replacements) == 0) {
    attr(chat, "alias_map") <- alias_map
    attr(chat, "participants") <- unique(stats::na.omit(chat$sender))
    return(save_current_chat(chat))
  }

  # Replace sender names
  sender <- chat$sender
  for (variant in names(replacements)) {
    sender[sender == variant] <- replacements[[variant]]
  }

  # Replace inside relevant text columns
  order_idx <- order(nchar(names(replacements)), decreasing = TRUE)
  variants <- names(replacements)[order_idx]

  apply_replacements <- function(x) {
    if (is.null(x)) return(x)
    if (is.list(x)) {
      return(lapply(x, function(elem) {
        if (is.null(elem)) return(elem)
        if (length(elem) == 0) return(elem)
        out <- elem
        for (variant in variants) {
          out <- gsub(variant, replacements[[variant]], out, fixed = TRUE)
        }
        out
      }))
    }
    for (variant in variants) {
      x <- gsub(variant, replacements[[variant]], x, fixed = TRUE)
    }
    x
  }

  chat$sender <- sender
  if ("text" %in% names(chat)) chat$text <- apply_replacements(chat$text)
  if ("text_raw" %in% names(chat)) chat$text_raw <- apply_replacements(chat$text_raw)
  if ("text_enriched" %in% names(chat)) chat$text_enriched <- apply_replacements(chat$text_enriched)
  if ("audio_transcript" %in% names(chat)) chat$audio_transcript <- apply_replacements(chat$audio_transcript)
  if ("audio_transcripts" %in% names(chat)) chat$audio_transcripts <- apply_replacements(chat$audio_transcripts)
  if ("image_description" %in% names(chat)) chat$image_description <- apply_replacements(chat$image_description)
  if ("image_descriptions" %in% names(chat)) chat$image_descriptions <- apply_replacements(chat$image_descriptions)

  attr(chat, "alias_map") <- alias_map
  attr(chat, "participants") <- unique(stats::na.omit(chat$sender))

  save_current_chat(chat)
}
