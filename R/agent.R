# Fetch context_length for a model from the public OpenRouter models endpoint.
# Returns NA on failure. Result is memoised by the caller.
openrouter_context_length <- function(model) {
  tryCatch({
    resp <- httr2::request("https://openrouter.ai/api/v1/models") |>
      httr2::req_perform()
    models <- httr2::resp_body_json(resp)$data
    match <- Filter(function(m) identical(m$id, model), models)
    if (length(match)) as.integer(match[[1]]$context_length) else NA_integer_
  }, error = function(e) NA_integer_)
}

# Fetch key info from OpenRouter.
# Returns list(usage, limit_remaining) — either may be NA if unavailable.
openrouter_key_info <- function() {
  tryCatch({
    resp <- httr2::request("https://openrouter.ai/api/v1/auth/key") |>
      httr2::req_headers(Authorization = paste("Bearer", Sys.getenv("OPENROUTER_API_KEY"))) |>
      httr2::req_perform()
    d <- httr2::resp_body_json(resp)$data
    list(
      usage           = if (is.null(d$usage))           NA_real_ else as.numeric(d$usage) / 100,
      limit_remaining = if (is.null(d$limit_remaining)) NA_real_ else as.numeric(d$limit_remaining)
    )
  }, error = function(e) list(usage = NA_real_, limit_remaining = NA_real_))
}

# Total tokens used so far in this chat (input + output across all turns).
chat_tokens_used <- function(chat) {
  t <- tryCatch(chat$get_tokens(), error = function(e) NULL)
  if (is.null(t) || nrow(t) == 0) return(0L)
  as.integer(sum(t$input, t$output, na.rm = TRUE))
}

fmt_k <- function(n) {
  if (n >= 1000) paste0(round(n / 1000), "k") else as.character(n)
}

build_prompt <- function(tokens_used, context_length, key_info) {
  tok <- if (!is.na(context_length)) {
    pct <- round(100 * tokens_used / context_length)
    paste0(fmt_k(tokens_used), "/", fmt_k(context_length), " [", pct, "%]")
  } else {
    fmt_k(tokens_used)
  }

  cr <- if (!is.na(key_info$limit_remaining)) {
    paste0(" | $", round(key_info$limit_remaining, 2), " remaining")
  } else if (!is.na(key_info$usage)) {
    paste0(" | $", round(key_info$usage, 4), " used")
  } else ""

  paste0("[", tok, cr, "]\n>>> ")
}

#' Start the coddr agent
#'
#' @param model OpenRouter model ID to use
#' @param session_id Unique identifier for the session
#' @param ... Additional arguments passed to ellmer::chat_openrouter()
#' @export
run_agent <- function(model = "google/gemma-4-31b-it:free", session_id = "default", ...) {
  source("R/context.R")
  source("R/tools_fs.R")
  source("R/tools_db.R")

  # Expose session_id globally so tool closures can access it
  assign("current_session_id", session_id, envir = .GlobalEnv)

  session_path <- init_session(session_id)
  message("Session: ", session_path)

  chat <- ellmer::chat_openrouter(model = model, ...)

  register_fs_tools(chat, session_id)
  register_db_tools(chat, session_id)

  context_length <- openrouter_context_length(model)

  message("Type 'exit' to quit.")

  repeat {
    chat$set_turns(load_turns(session_id))
    chat$set_system_prompt(sweep_context(session_id))

    tokens_used <- chat_tokens_used(chat)
    key_info    <- openrouter_key_info()
    user_input  <- readline(prompt = build_prompt(tokens_used, context_length, key_info))

    if (tolower(trimws(user_input)) == "exit") break

    response <- tryCatch(
      chat$chat(user_input, echo = "none"),
      error = function(e) {
        message("\n[API error] ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(response)) {
      save_turns(session_id, chat$get_turns())
      cat("\n", response, "\n\n")
    }
  }

  message("Session ended.")
}
