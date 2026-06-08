CODDR_ROOT <- normalizePath(".")
SESSIONS_DIR <- file.path(CODDR_ROOT, "sessions")

init_session <- function(session_id) {
  session_path <- file.path(SESSIONS_DIR, session_id)
  dir.create(file.path(session_path, "history"), recursive = TRUE, showWarnings = FALSE)
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

append_history <- function(session_id, role, message) {
  history_path <- file.path(SESSIONS_DIR, session_id, "history", "transcript.md")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entry <- paste0("\n[", timestamp, "] ", toupper(role), ":\n", message, "\n")
  current <- if (file.exists(history_path)) paste(readLines(history_path, warn = FALSE), collapse = "\n") else ""
  writeLines(paste0(current, entry), history_path)
}

sweep_context <- function(session_id) {
  session_path <- file.path(SESSIONS_DIR, session_id)

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

  history_path <- file.path(session_path, "history", "transcript.md")
  history_content <- if (file.exists(history_path)) {
    paste0("## Conversation History\n", paste(readLines(history_path, warn = FALSE), collapse = "\n"))
  } else ""

  paste(
    identity,
    tools_doc,
    active_content,
    history_content,
    sep = "\n\n---\n\n"
  )
}
