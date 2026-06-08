list_dir <- ellmer::tool(
  function(path = ".") {
    if (!dir.exists(path)) return(paste0("Directory not found: ", path))
    entries <- list.files(path, all.files = FALSE, full.names = FALSE, recursive = FALSE)
    dirs <- entries[file.info(file.path(path, entries))$isdir]
    files <- entries[!file.info(file.path(path, entries))$isdir]
    c(
      if (length(dirs)) paste0("[dir]  ", dirs),
      if (length(files)) paste0("[file] ", files)
    ) |> paste(collapse = "\n")
  },
  description = "List files and directories at the given path.",
  arguments = list(
    path = ellmer::type_string("Path to list. Defaults to current working directory.", required = FALSE)
  )
)

read_file <- ellmer::tool(
  function(path, start_line = 1, end_line = NULL) {
    if (!file.exists(path)) return(paste0("File not found: ", path))
    tryCatch({
      lines <- readLines(path, warn = FALSE)
      total_lines <- length(lines)
      if (is.null(end_line)) {
        end_line <- min(start_line + 49, total_lines)
      } else {
        end_line <- min(end_line, total_lines)
      }
      if (start_line > total_lines) return("Start line exceeds file length.")
      chunk <- lines[start_line:end_line]
      numbered_chunk <- paste0(start_line:end_line, ": ", chunk)
      paste0(
        "--- Reading ", path, " (lines ", start_line, " to ", end_line, " of ", total_lines, ") ---\n",
        paste(numbered_chunk, collapse = "\n")
      )
    }, error = function(e) paste0("Error reading file: ", conditionMessage(e)))
  },
  description = "Read a specific range of lines from a file. Use this to sample a file before adding it to context.",
  arguments = list(
    path = ellmer::type_string("Path to the file to read."),
    start_line = ellmer::type_integer("The line number to start reading from.", required = FALSE),
    end_line = ellmer::type_integer("The line number to end reading at. Defaults to 50 lines from start.", required = FALSE)
  )
)

write_file <- ellmer::tool(
  function(path, content, overwrite = FALSE) {
    if (file.exists(path) && !overwrite) {
      return(paste0("File already exists: ", path, ". Set overwrite = TRUE to replace it."))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      writeLines(content, path)
      paste0("Written: ", path)
    }, error = function(e) paste0("Error writing file: ", conditionMessage(e)))
  },
  description = "Write entire content to a file.",
  arguments = list(
    path = ellmer::type_string("Path to write to."),
    content = ellmer::type_string("Text content to write."),
    overwrite = ellmer::type_boolean("Whether to overwrite if file exists. Defaults to FALSE.", required = FALSE)
  )
)

replace_in_file <- ellmer::tool(
  function(path, search_block, replace_block) {
    if (!file.exists(path)) return(paste0("File not found: ", path))
    tryCatch({
      full_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
      if (!grepl(search_block, full_text, fixed = TRUE)) {
        return("Error: The search block was not found exactly in the file. Please read the file again and provide the exact snippet to replace.")
      }
      writeLines(gsub(search_block, replace_block, full_text, fixed = TRUE), path)
      paste0("Successfully replaced snippet in ", path)
    }, error = function(e) paste0("Error during replace: ", conditionMessage(e)))
  },
  description = "Replace a specific block of text in a file. Provide the exact text to replace.",
  arguments = list(
    path = ellmer::type_string("Path to the file."),
    search_block = ellmer::type_string("The exact text block to be replaced."),
    replace_block = ellmer::type_string("The new text block to insert.")
  )
)

shell_execute <- ellmer::tool(
  function(command) {
    tryCatch({
      if (.Platform$OS.type == "windows") {
        result <- system2("cmd", args = c("/c", command), stdout = TRUE, stderr = TRUE)
      } else {
        result <- system2("sh", args = c("-c", shQuote(command)), stdout = TRUE, stderr = TRUE)
      }
      paste(result, collapse = "\n")
    }, error = function(e) paste0("Error executing shell command: ", conditionMessage(e)))
  },
  description = "Execute a shell command and return the output.",
  arguments = list(
    command = ellmer::type_string("The shell command to execute.")
  )
)

add_to_context <- ellmer::tool(
  function(file_path) {
    add_to_context_internal(current_session_id, file_path)
    paste0("Added to context: ", file_path)
  },
  description = "Add a file to the active context so its contents are included in every turn.",
  arguments = list(
    file_path = ellmer::type_string("Path to the file to add to context.")
  )
)

remove_from_context <- ellmer::tool(
  function(file_path) {
    remove_from_context_internal(current_session_id, file_path)
    paste0("Removed from context: ", file_path)
  },
  description = "Remove a file from the active context.",
  arguments = list(
    file_path = ellmer::type_string("Path to the file to remove from context.")
  )
)

list_context <- ellmer::tool(
  function() {
    files <- list_context_internal(current_session_id)
    if (length(files) == 0) "No files in context." else paste(files, collapse = "\n")
  },
  description = "List the files currently in the active context.",
  arguments = list()
)
