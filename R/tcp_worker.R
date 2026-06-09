# R/tcp_worker.R
#
# Long-running TCP query worker. Runs as a Windows service account via PsExec.
#
# Args:
#   1. worker_dir   - directory for pid file and logs
#   2. shared_lib   - path to shared R library
#   3. port         - TCP port to listen on
#   4. token        - shared secret for request auth
#   5. server       - SQL Server hostname
#   6. database     - database name

args <- commandArgs(trailingOnly = TRUE)

worker_dir  <- if (length(args) >= 1) args[[1]] else stop("worker_dir required")
shared_lib  <- if (length(args) >= 2) args[[2]] else stop("shared_lib required")
port        <- if (length(args) >= 3) as.integer(args[[3]]) else stop("port required")
token       <- if (length(args) >= 4) args[[4]] else stop("token required")
server      <- if (length(args) >= 5) args[[5]] else stop("server required")
database    <- if (length(args) >= 6) args[[6]] else stop("database required")

if (!nzchar(token)) stop("Worker token is empty. Refusing to start.")

dir.create(worker_dir, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(
  worker_dir,
  paste0("worker_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

log_con <- file(log_file, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

on.exit({
  while (sink.number(type = "message") > 0) sink(type = "message")
  while (sink.number(type = "output") > 0) sink(type = "output")
  close(log_con)
}, add = TRUE)

options(warn = 1)

log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")
  flush.console()
}

log_msg("Worker starting")
log_msg("Server:", server, "Database:", database)
log_msg("Port:", port)
log_msg("Worker dir:", worker_dir)
log_msg("USERNAME:", Sys.getenv("USERNAME"))
log_msg("USERDOMAIN:", Sys.getenv("USERDOMAIN"))
log_msg("R version:", R.version.string)

# Write pid file so the launcher can detect stale processes
pid_file <- file.path(worker_dir, "worker.pid")
writeLines(as.character(Sys.getpid()), pid_file)
on.exit(unlink(pid_file), add = TRUE)

log_msg("PID:", Sys.getpid(), "written to", pid_file)

# -----------------------------
# Library setup
# -----------------------------

if (!dir.exists(shared_lib)) stop("Shared library does not exist: ", shared_lib)

Sys.setenv(R_LIBS_USER = shared_lib)
.libPaths(unique(c(shared_lib, .libPaths())))

for (pkg in c("DBI", "odbc")) {
  ok <- tryCatch(
    { requireNamespace(pkg, quietly = FALSE); TRUE },
    error = function(e) { log_msg("Package load failed:", pkg, conditionMessage(e)); FALSE }
  )
  if (!ok) stop("Required package could not be loaded: ", pkg)
}

library(DBI)
library(odbc)

# -----------------------------
# DB connection
# -----------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

connect_db <- function() {
  DBI::dbConnect(
    odbc::odbc(),
    Driver             = "ODBC Driver 18 for SQL Server",
    Server             = server,
    Database           = database,
    Trusted_Connection = "Yes",
    Encrypt            = "Yes",
    TrustServerCertificate = "Yes"
  )
}

con <- connect_db()
on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

log_msg("Connected to", server, "/", database)

identity <- DBI::dbGetQuery(con, "
  select
    SYSTEM_USER       as system_user,
    ORIGINAL_LOGIN()  as original_login,
    SUSER_SNAME()     as suser_sname
")
log_msg("SQL identity:")
print(identity)

# -----------------------------
# Query handling
# -----------------------------

is_select_like <- function(sql) {
  grepl("^(select|with)\\b", trimws(sql), ignore.case = TRUE)
}

handle_request <- function(req) {
  if (!is.list(req)) stop("Request must be a list.")
  if (!identical(req$token, token)) stop("Invalid worker token.")

  op <- req$op %||% "query"

  if (identical(op, "ping")) {
    return(list(
      ok   = TRUE,
      op   = "ping",
      time = as.character(Sys.time()),
      user = paste(Sys.getenv("USERDOMAIN"), Sys.getenv("USERNAME"), sep = "\\")
    ))
  }

  if (identical(op, "stop")) {
    return(list(ok = TRUE, op = "stop", message = "Worker will stop."))
  }

  if (!identical(op, "query")) stop("Unknown operation: ", op)

  sql <- req$sql
  if (!is.character(sql) || length(sql) != 1 || !nzchar(trimws(sql))) {
    stop("Request must contain a single non-empty SQL string.")
  }
  if (!is_select_like(sql)) stop("Refusing to run non-SELECT/WITH query.")

  if (!DBI::dbIsValid(con)) {
    log_msg("Connection invalid, reconnecting.")
    con <<- connect_db()
  }

  started  <- Sys.time()
  df       <- DBI::dbGetQuery(con, sql)
  finished <- Sys.time()

  list(
    ok              = TRUE,
    op              = "query",
    rows            = nrow(df),
    cols            = ncol(df),
    started         = as.character(started),
    finished        = as.character(finished),
    elapsed_seconds = as.numeric(difftime(finished, started, units = "secs")),
    data            = df
  )
}

# -----------------------------
# TCP server
# -----------------------------

log_msg("Starting TCP server on port:", port)
server_socket <- serverSocket(port)
on.exit(try(close(server_socket), silent = TRUE), add = TRUE)
log_msg("TCP server ready")

should_stop <- FALSE

repeat {
  client <- NULL
  tryCatch(
    {
      client <- socketAccept(server_socket, blocking = TRUE, open = "a+b")
      req    <- unserialize(client)
      log_msg("Received op:", req$op %||% "query")

      response <- tryCatch(
        {
          result <- handle_request(req)
          if (identical(result$op, "stop")) should_stop <<- TRUE
          result
        },
        error = function(e) {
          log_msg("Request error:", conditionMessage(e))
          list(ok = FALSE, error = conditionMessage(e),
               call = paste(capture.output(print(conditionCall(e))), collapse = "\n"))
        }
      )

      serialize(response, client)
      flush(client)
    },
    error = function(e) log_msg("Socket error:", conditionMessage(e)),
    finally = { if (!is.null(client)) try(close(client), silent = TRUE) }
  )

  if (should_stop) break
}

log_msg("Worker stopping")
