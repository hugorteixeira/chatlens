# AGENTS.md

Guidance for future coding agents working in this repository.

## Project Shape

`chatlens` is an R package for importing WhatsApp Android ZIP exports into a
`chatlens_chat`, enriching messages with media context, preparing compact LLM
inputs, and running chat analysis through `genflow`.

The intended user flow is:

```r
chat <- cl_whatsapp_import(path)
cl_whatsapp_summary(chat)
chat <- cl_chat_anonymize(chat)
chat <- cl_chat_transcribe_audio(chat)
chat <- cl_chat_describe_images(chat)
chat <- cl_chat_process_media(chat)
prepared <- cl_prepare_analysis(chat)
analysis <- cl_analyze_chat(prepared, prompt = prompt)
```

The package is not published yet. It is acceptable to break old internal or
legacy APIs when simplifying the current flow, but keep README, roxygen, man
pages, tests, and `NAMESPACE` aligned in the same change.

## Public API Contract

Keep the public surface intentionally small. The current exported API is:

- `cl_whatsapp_import()`
- `cl_whatsapp_summary()`
- `cl_chat_anonymize()`
- `cl_chat_transcribe_audio()`
- `cl_chat_describe_images()`
- `cl_chat_process_media()`
- `cl_prepare_analysis()`
- `cl_analyze_chat()`
- S3 method `print.chatlens_chat`

Do not reintroduce legacy public functions such as `cl_chat_split_periods()`,
`cl_periods_*()`, `cl_export_chat()`, `cl_analyze_text()`, `cl_cache_dir()`,
`cl_attachments()`, `cl_detect_participants()`, or `cl_build_alias_map()` unless
the user explicitly asks for them.

Internal helpers should be named `.clh_*`. The `h` means helper. Avoid exporting
helpers or documenting them as user-facing functions.

## Cache And Artifacts

All public functions that need storage should use `cache_dir = NULL` by default.
`NULL` resolves to `~/.chatlens`. Do not add a separate `analysis_dir` argument;
analysis output lives under the chat cache.

Default chat cache layout:

```text
~/.chatlens/whatsapp/
  extract/<zip_id>/
  chats/<chat_key>/
    chat_original.rds
    chat_original.txt
    chat.rds
    chat.txt
    aliases.json
    manifests/
    runs/
    analysis/
```

`chat_original.*` is the raw import snapshot. `chat.*` is the latest
`chatlens_chat` state after any processing. If a public function modifies a
`chatlens_chat`, it should default to saving the current state as both
`chat.rds` and mirrored `chat.txt` using the shared `.clh_save_*` helpers.

The `.txt` mirror must reflect the same chat object that was just saved as RDS.
This is a backup/export of the latest object state, not a separate source of
truth.

## Media Flow

`cl_chat_transcribe_audio()` writes audio transcript data into the chat object.
`cl_chat_describe_images()` writes image descriptions into the chat object.
`cl_chat_process_media()` creates or updates `text_enriched` with media
annotations. Media processing should keep helpful error messages in console
output and cached metadata; silent failures are not acceptable.

When media functions change the chat, keep `save_chat = TRUE` by default and
update `chat.rds` plus `chat.txt`.

## Analysis Flow

`cl_prepare_analysis()` is the only user-facing preparation/export step for LLM
analysis. It accepts `period = c("all", "year", "month", "week", "day")`; the
default is `"all"`. It accepts `formatting = c("simple", "raw")`; the default is
`"simple"`.

`"simple"` formatting is the preferred LLM input format: it removes repeated
date/name clutter by grouping repeated dates and consecutive messages from the
same sender. `"raw"` keeps one formatted line per message.

Prepared inputs are saved as:

```text
analysis/
  all/input.txt
  by_year/<YYYY>/input.txt
  by_month/<YYYY>/<MM>/input.txt
  by_week/<YYYY>/<WW>/input.txt
  by_day/<YYYY>/<MM>/<DD>/input.txt
```

Each prepared directory also gets `input.rds` when `save = TRUE`.

`cl_analyze_chat()` accepts a `chatlens_analysis_input` from
`cl_prepare_analysis()` or a raw `chatlens_chat` that will be prepared with
defaults. It writes timestamped artifacts in the same prepared input directory:

```text
prompt_<timestamp>_<service>_<model>.txt
result_<timestamp>_<service>_<model>.txt
result_<timestamp>_<service>_<model>.rds
meta_<timestamp>_<service>_<model>.json
run_<timestamp>_<service>_<model>.json
```

## Documentation Rules

Roxygen source lives in `R/*.R`; generated docs live in `man/*.Rd`.
`DESCRIPTION` has `Roxygen: list(markdown = TRUE)`, so use roxygen markdown
links such as `[cl_prepare_analysis()]` and `[genflow::gen_txt()]`.

After changing roxygen comments or signatures, run:

```sh
Rscript -e 'roxygen2::roxygenize(".")'
```

Keep `README.md` aligned with the actual exported flow. It should not mention
removed period/select/export APIs. If behavior changes, update README, roxygen,
tests, and man pages together.

## Testing And Checks

Use temporary cache directories in tests. Tests must not write to a user's real
`~/.chatlens`.

Useful validation commands:

```sh
Rscript -e 'for (f in list.files("R", full.names = TRUE)) parse(file = f); cat("parse ok\n")'
Rscript -e 'devtools::test(reporter = "summary")'
R CMD build . --no-build-vignettes
R CMD check chatlens_0.1.0.tar.gz --no-manual --no-build-vignettes
```

Prefer checking the built tarball. Direct `R CMD check .` can report local
workspace artifacts as hidden-file warnings.

Clean generated check artifacts before finishing:

```sh
rm -rf chatlens.Rcheck ..Rcheck
rm -f chatlens_*.tar.gz
```

## Editing Notes

Follow the existing R style: small base-R helpers, explicit data frames, and
shared cache helpers instead of ad hoc path or text serialization logic.

Use structured helpers for RDS-plus-text writes. Avoid making separate export
functions public; `cl_prepare_analysis()` is the user-facing analysis export.

Do not revert unrelated dirty work in this repo. Inspect `git status -sb`
before and after edits, and keep changes scoped to the requested behavior.
