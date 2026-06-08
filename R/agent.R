#' Start the coddr agent
#'
#' @param model OpenRouter model ID to use
#' @param session_id Unique identifier for the session
#' @param ... Additional arguments passed to ellmer::chat_openrouter()
#' @export
run_agent <- function(model = "google/gemma-4-31b-it:free", session_id = "default", ...) {
  source("R/context.R")
  source("R/tools_fs.R")

  # Expose session_id globally so tool closures can access it
  assign("current_session_id", session_id, envir = .GlobalEnv)

  session_path <- init_session(session_id)
  message("Session: ", session_path)

  chat <- ellmer::chat_openrouter(model = model, ...)

  chat$register_tool(list_dir)
  chat$register_tool(read_file)
  chat$register_tool(write_file)
  chat$register_tool(replace_in_file)
  chat$register_tool(shell_execute)
  chat$register_tool(add_to_context)
  chat$register_tool(remove_from_context)
  chat$register_tool(list_context)

  message("Type 'exit' to quit.")

  repeat {
    chat$set_system_prompt(sweep_context(session_id))

    user_input <- readline(prompt = ">>> ")
    if (tolower(trimws(user_input)) == "exit") break

    append_history(session_id, "user", user_input)
    response <- chat$chat(user_input)
    append_history(session_id, "assistant", response)

    cat("\n", response, "\n\n")
  }

  message("Session ended.")
}
