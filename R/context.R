CODDR_ROOT <- normalizePath(".")
SESSIONS_DIR <- file.path(CODDR_ROOT, "sessions")

init_session <- function(session_id) {
  session_path <- file.path(SESSIONS_DIR, session_id)
  dir.create(session_path, recursive = TRUE, showWarnings = FALSE)
  manifest_path <- file.path(session_path, "active_files.txt")
  if (!file.exists(manifest_path)) writeLines(character(0), manifest_path)
  session_path
}

read_manifest <- function(session_id) {
  path <- file.path(SESSIONS_DIR, session_id, "active_files.txt")
  lines <- readLines(path, warn = FALSE)
  lines[nzchar(lines)]
}

write_manifest <- function(session_id, files) {
  path <- file.path(SESSIONS_DIR, session_id, "active_files.txt")
  writeLines(files, path)
}

add_to_context_internal <- function(session_id, file_path) {
  files <- read_manifest(session_id)
  if (!file_path %in% files) write_manifest(session_id, c(files, file_path))
  invisible(NULL)
}

remove_from_context_internal <- function(session_id, file_path) {
  files <- read_manifest(session_id)
  write_manifest(session_id, files[files != file_path])
  invisible(NULL)
}

list_context_internal <- function(session_id) {
  read_manifest(session_id)
}

turns_path <- function(session_id) {
  file.path(SESSIONS_DIR, session_id, "turns.json")
}

save_turns <- function(session_id, turns) {
  if (length(turns) == 0) return(invisible(NULL))
  recorded <- lapply(turns, ellmer::contents_record)
  writeLines(jsonlite::serializeJSON(recorded), turns_path(session_id))
  invisible(NULL)
}

load_turns <- function(session_id) {
  path <- turns_path(session_id)
  if (!file.exists(path)) return(list())
  tryCatch({
    recorded <- jsonlite::unserializeJSON(paste(readLines(path, warn = FALSE), collapse = "\n"))
    lapply(recorded, ellmer::contents_replay)
  }, error = function(e) {
    message("[warn] Could not load turns: ", conditionMessage(e))
    list()
  })
}

sweep_context <- function(session_id) {
  identity_path <- file.path(CODDR_ROOT, "IDENTITY.md")
  tools_path <- file.path(CODDR_ROOT, "TOOLS.md")

  identity <- if (file.exists(identity_path)) paste(readLines(identity_path, warn = FALSE), collapse = "\n") else ""
  tools_doc <- if (file.exists(tools_path)) paste(readLines(tools_path, warn = FALSE), collapse = "\n") else ""

  files <- read_manifest(session_id)
  active_content <- if (length(files) == 0) "" else {
    sections <- vapply(files, function(f) {
      if (!file.exists(f)) return(paste0("--- File: ", f, " (not found) ---"))
      lines <- readLines(f, warn = FALSE)
      numbered <- paste0(seq_along(lines), ": ", lines)
      paste0("--- File: ", f, " ---\n", paste(numbered, collapse = "\n"))
    }, character(1))
    paste0("## Active Files\n\n", paste(sections, collapse = "\n\n"))
  }

  paste(
    identity,
    tools_doc,
    active_content,
    sep = "\n\n---\n\n"
  )
}
