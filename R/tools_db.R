# R/tools_db.R
#
# Database query tool with Windows-auth TCP worker lifecycle management.
# Entry point: register_db_tools(chat, session_id)

CONNECTIONS_PATH <- "connections.csv"
WORKER_SCRIPT    <- file.path(normalizePath("."), "R", "tcp_worker.R")
SHARED_LIB       <- "C:/R/shared-library/4.6"
RSCRIPT          <- "C:/Program Files/R/R-4.6.0/bin/Rscript.exe"
PSEXEC           <- "psexec"
WORKER_TOKEN_ENV <- "R_TCP_WORKER_TOKEN"

PING_TIMEOUT_S   <- 5L
PING_RETRIES     <- 10L
PING_INTERVAL_S  <- 1L
QUERY_TIMEOUT_S  <- 120L
PREVIEW_ROWS     <- 10L
PREVIEW_COLS     <- 10L

# -----------------------------
# Connection config
# -----------------------------

load_connections <- function() {
  if (!file.exists(CONNECTIONS_PATH)) {
    stop("connections.csv not found at: ", CONNECTIONS_PATH)
  }
  df <- read.csv(CONNECTIONS_PATH, stringsAsFactors = FALSE, strip.white = TRUE)
  required <- c("name", "server", "database", "auth_type", "username", "password_env", "port")
  missing  <- setdiff(required, names(df))
  if (length(missing)) stop("connections.csv missing columns: ", paste(missing, collapse = ", "))
  df
}

get_connection <- function(name) {
  cons <- load_connections()
  row  <- cons[cons$name == name, ]
  if (nrow(row) == 0) stop("Unknown connection: ", name)
  as.list(row[1, ])
}

# -----------------------------
# Worker state (session-scoped)
# -----------------------------

worker_dir <- function(session_id, conn_name) {
  file.path(normalizePath("."), "sessions", session_id, "workers", conn_name)
}

pid_file_path <- function(session_id, conn_name) {
  file.path(worker_dir(session_id, conn_name), "worker.pid")
}

read_pid <- function(session_id, conn_name) {
  path <- pid_file_path(session_id, conn_name)
  if (!file.exists(path)) return(NULL)
  val <- suppressWarnings(as.integer(readLines(path, warn = FALSE)[1]))
  if (is.na(val)) NULL else val
}

process_alive <- function(pid) {
  if (is.null(pid)) return(FALSE)
  if (.Platform$OS.type == "windows") {
    # pskill with signal 0 checks existence without killing
    tryCatch(tools::pskill(pid, 0L) == 0L, error = function(e) FALSE)
  } else {
    tryCatch(
      { system2("kill", c("-0", as.character(pid)), stdout = FALSE, stderr = FALSE) == 0L },
      error = function(e) FALSE
    )
  }
}

kill_stale <- function(session_id, conn_name) {
  pid <- read_pid(session_id, conn_name)
  if (!is.null(pid) && process_alive(pid)) {
    message("Killing stale worker PID ", pid, " for connection: ", conn_name)
    try(tools::pskill(pid), silent = TRUE)
  }
  unlink(pid_file_path(session_id, conn_name))
}

# -----------------------------
# TCP communication
# -----------------------------

send_request <- function(port, req, timeout = QUERY_TIMEOUT_S) {
  con <- socketConnection(
    host     = "127.0.0.1",
    port     = port,
    open     = "a+b",
    blocking = TRUE,
    timeout  = timeout
  )
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  serialize(req, con)
  flush(con)
  unserialize(con)
}

ping_worker <- function(port, token) {
  tryCatch(
    {
      resp <- send_request(port, list(op = "ping", token = token), timeout = PING_TIMEOUT_S)
      isTRUE(resp$ok)
    },
    error = function(e) FALSE
  )
}

stop_worker <- function(port, token) {
  tryCatch(
    send_request(port, list(op = "stop", token = token), timeout = PING_TIMEOUT_S),
    error = function(e) invisible(NULL)
  )
}

# -----------------------------
# Worker launch (Windows only)
# -----------------------------

launch_worker <- function(conn, session_id, token) {
  if (.Platform$OS.type != "windows") {
    stop(
      "Cannot launch TCP worker on non-Windows platform. ",
      "Start the worker manually for connection: ", conn$name
    )
  }

  pwd <- Sys.getenv(conn$password_env)
  if (!nzchar(pwd)) stop("Password env var not set: ", conn$password_env)
  if (!nzchar(token)) stop("Worker token env var not set: ", WORKER_TOKEN_ENV)

  wdir <- worker_dir(session_id, conn$name)
  dir.create(wdir, recursive = TRUE, showWarnings = FALSE)

  message("Launching worker for connection '", conn$name, "' on port ", conn$port, "...")

  system2(
    PSEXEC,
    args = c(
      "-accepteula", "-nobanner",
      "-u", conn$username,
      "-p", pwd,
      paste0('"', RSCRIPT, '"'),
      "--vanilla",
      paste0('"', WORKER_SCRIPT, '"'),
      paste0('"', wdir, '"'),
      paste0('"', SHARED_LIB, '"'),
      as.character(conn$port),
      token,
      conn$server,
      conn$database
    ),
    wait = FALSE
  )
}

# -----------------------------
# Ensure worker is running
# -----------------------------

ensure_worker <- function(conn, session_id, token) {
  port <- as.integer(conn$port)

  if (ping_worker(port, token)) return(invisible(NULL))

  # Ping failed — clean up any stale process before restarting
  kill_stale(session_id, conn$name)

  launch_worker(conn, session_id, token)

  # Wait for worker to become ready
  for (i in seq_len(PING_RETRIES)) {
    Sys.sleep(PING_INTERVAL_S)
    if (ping_worker(port, token)) {
      message("Worker ready for connection: ", conn$name)
      return(invisible(NULL))
    }
  }

  stop(
    "Worker for '", conn$name, "' did not become ready after ",
    PING_RETRIES, " attempts. Check worker logs in: ",
    worker_dir(session_id, conn$name)
  )
}

# -----------------------------
# Result formatting
# -----------------------------

format_result <- function(response, conn_name, session_id) {
  df   <- response$data
  rows <- nrow(df)
  cols <- ncol(df)

  # Write full result to disk
  out_dir  <- worker_dir(session_id, conn_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(
    out_dir,
    paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  write.csv(df, out_file, row.names = FALSE)

  # Build preview
  preview_df   <- df[seq_len(min(rows, PREVIEW_ROWS)), seq_len(min(cols, PREVIEW_COLS)), drop = FALSE]
  col_names    <- names(preview_df)
  col_widths   <- pmax(nchar(col_names), vapply(preview_df, function(x) max(nchar(as.character(x)), 0L), integer(1)))
  pad          <- function(s, w) formatC(as.character(s), width = w, flag = "-")
  header       <- paste(mapply(pad, col_names,    col_widths), collapse = " | ")
  sep_line     <- paste(vapply(col_widths, function(w) strrep("-", w), character(1)), collapse = "-+-")
  data_rows    <- apply(preview_df, 1, function(r) paste(mapply(pad, r, col_widths), collapse = " | "))
  table_lines  <- c(header, sep_line, data_rows)
  table_md     <- paste(table_lines, collapse = "\n")

  truncation_note <- if (rows > PREVIEW_ROWS || cols > PREVIEW_COLS) {
    paste0(
      "\n_(preview truncated to ", min(rows, PREVIEW_ROWS), " of ", rows,
      " rows and ", min(cols, PREVIEW_COLS), " of ", cols, " columns)_"
    )
  } else ""

  paste0(
    "**Connection:** ", conn_name, "  \n",
    "**Query returned:** ", rows, " rows x ", cols, " columns  \n",
    "**Elapsed:** ", round(response$elapsed_seconds, 2), "s  \n",
    "**Full result saved to:** `", out_file, "`\n\n",
    "```\n", table_md, "\n```",
    truncation_note
  )
}

# -----------------------------
# Tool definition
# -----------------------------

make_query_database_tool <- function(session_id, token) {
  ellmer::tool(
    function(connection_name, sql) {
      conn <- tryCatch(
        get_connection(connection_name),
        error = function(e) return(paste0("Error: ", conditionMessage(e)))
      )
      if (is.character(conn)) return(conn)

      err <- tryCatch({ ensure_worker(conn, session_id, token); NULL }, error = function(e) conditionMessage(e))
      if (!is.null(err)) return(paste0("Error starting worker: ", err))

      response <- tryCatch(
        send_request(as.integer(conn$port), list(op = "query", sql = sql, token = token), timeout = QUERY_TIMEOUT_S),
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )

      if (!isTRUE(response$ok)) return(paste0("Query error: ", response$error))

      format_result(response, connection_name, session_id)
    },
    description = paste0(
      "Run a read-only SQL SELECT query against a named database connection. ",
      "Returns a markdown summary with a preview table and the path to the full CSV result on disk. ",
      "Available connections are defined in connections.csv."
    ),
    arguments = list(
      connection_name = ellmer::type_string("The name of the database connection to query (from connections.csv)."),
      sql             = ellmer::type_string("A SQL SELECT or WITH query to execute.")
    )
  )
}

# -----------------------------
# Registration
# -----------------------------

register_db_tools <- function(chat, session_id) {
  token <- Sys.getenv(WORKER_TOKEN_ENV)

  # Collect connection names for cleanup
  conn_names <- tryCatch(load_connections()$name, error = function(e) character(0))

  # Register on.exit in the caller (run_agent) frame so cleanup fires when the agent loop exits.
  # Bake conn_names into the expression so the handler has no external dependencies.
  cleanup_expr <- bquote(
    for (.conn_name in .(conn_names)) {
      .port  <- tryCatch(as.integer(get_connection(.conn_name)$port), error = function(e) NULL)
      .token <- Sys.getenv(.(WORKER_TOKEN_ENV))
      if (!is.null(.port)) stop_worker(.port, .token)
    }
  )
  eval(call("on.exit", cleanup_expr, add = TRUE, after = TRUE), envir = parent.frame())

  # Make conn_names visible in the caller's frame for the on.exit closure
  assign(".db_conn_names", conn_names, envir = parent.frame())

  chat$register_tool(make_query_database_tool(session_id, token))

  invisible(NULL)
}
