# Internal registry for the server process started by this package.
.ditto_server <- new.env(parent = emptyenv())

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Manage a local llama.cpp embedding server
#'
#' [bertscore()] and [token_embeddings()] need a running `llama.cpp` server
#' started with `--embedding --pooling none`. Setting that up by hand is
#' fiddly, so these helpers launch the server as a background process, wait
#' until it is ready to answer requests, report its status, and shut it down
#' again.
#'
#' `start_llama_server()` resolves the server binary and model from its
#' arguments, then from the `ditto.llama_server` / `ditto.llama_model` options,
#' then from the `DITTO_LLAMA_SERVER` / `DITTO_LLAMA_MODEL` environment
#' variables. Setting the options in your `.Rprofile` lets you call
#' `start_llama_server()` with no arguments:
#'
#' ```r
#' options(
#'   ditto.llama_server = "C:/Users/me/tools/llama.cpp/llama-server.exe",
#'   ditto.llama_model  = "C:/Users/me/tools/llama.cpp/models/all-MiniLM-L6-v2-f16.gguf"
#' )
#' ```
#'
#' @param model Path to a `.gguf` embedding model. Prefer a model without a
#'   required task prefix (see [token_embeddings()]).
#' @param exe Path to the `llama-server` executable. Defaults to `"llama-server"`,
#'   which must be on the `PATH`.
#' @param host Base URL the server should listen on, also used as the default
#'   `host` by [bertscore()]. The port is taken from this URL.
#' @param pooling Pooling mode passed to `--pooling`. Must be `"none"` for
#'   token-level embeddings.
#' @param extra_args Character vector of additional `llama-server` arguments.
#' @param wait Whether to block until the server answers its health check
#'   (`TRUE`) or return immediately (`FALSE`).
#' @param timeout Seconds to wait for the server to become ready when
#'   `wait = TRUE`.
#' `find_llama_server()` looks for the executable without starting it, checking
#' the `ditto.llama_server` option and `DITTO_LLAMA_SERVER` environment variable
#' first, then the `PATH`, then a few common install directories. It only reads
#' the filesystem and never launches anything.
#'
#' @return `start_llama_server()` invisibly returns the [processx::process]
#'   handle; `stop_llama_server()` returns `NULL` invisibly;
#'   `llama_server_running()` returns a single logical; `find_llama_server()`
#'   returns the path to the executable as a string, or `NULL` if none is found.
#' @seealso [bertscore()] and [token_embeddings()], which query the server.
#' @name llama_server
#' @examples
#' \dontrun{
#' start_llama_server(
#'   model = "models/all-MiniLM-L6-v2-f16.gguf",
#'   exe = "tools/llama.cpp/llama-server.exe"
#' )
#' llama_server_running()
#' bertscore("how much do you agree", "to what extent do you agree")
#' stop_llama_server()
#' }
NULL

#' @rdname llama_server
#' @export
start_llama_server <- function(model = NULL,
                               exe = NULL,
                               host = "http://localhost:8080",
                               pooling = "none",
                               extra_args = character(),
                               wait = TRUE,
                               timeout = 60) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required to manage a llama.cpp server. ",
         "Install it with install.packages(\"processx\").", call. = FALSE)
  }

  exe <- exe %||% getOption("ditto.llama_server") %||%
    nzchar_or_null(Sys.getenv("DITTO_LLAMA_SERVER")) %||% "llama-server"
  model <- model %||% getOption("ditto.llama_model") %||%
    nzchar_or_null(Sys.getenv("DITTO_LLAMA_MODEL"))

  if (is.null(model)) {
    stop("No model supplied. Pass `model =`, set options(ditto.llama_model = ), ",
         "or the DITTO_LLAMA_MODEL environment variable.", call. = FALSE)
  }
  if (!file.exists(model)) {
    stop("Model file not found: ", model, call. = FALSE)
  }

  if (llama_server_running(host)) {
    message("A llama.cpp server is already responding at ", host, ".")
    return(invisible(.ditto_server$process))
  }

  port <- httr2::url_parse(host)$port %||% "8080"
  logfile <- tempfile("ditto-llama-server-", fileext = ".log")
  args <- c("-m", model, "--embedding", "--pooling", pooling,
            "--port", as.character(port), extra_args)

  proc <- processx::process$new(
    exe, args,
    stdout = logfile, stderr = "2>&1"
  )
  .ditto_server$process <- proc
  .ditto_server$logfile <- logfile

  if (!wait) {
    message("Starting llama.cpp server (logs: ", logfile, ").")
    return(invisible(proc))
  }

  deadline <- Sys.time() + timeout
  repeat {
    if (!proc$is_alive()) {
      stop("llama-server exited before becoming ready. Logs:\n",
           paste(readLines(logfile, warn = FALSE), collapse = "\n"),
           call. = FALSE)
    }
    if (llama_server_running(host)) break
    if (Sys.time() > deadline) {
      proc$kill()
      stop("llama-server did not become ready within ", timeout, " seconds. ",
           "Logs:\n", paste(readLines(logfile, warn = FALSE), collapse = "\n"),
           call. = FALSE)
    }
    Sys.sleep(0.25)
  }

  message("llama.cpp server ready at ", host, ".")
  invisible(proc)
}

#' @rdname llama_server
#' @export
stop_llama_server <- function() {
  proc <- .ditto_server$process
  if (is.null(proc) || !proc$is_alive()) {
    message("No llama.cpp server started by ditto is running.")
    return(invisible(NULL))
  }
  proc$kill()
  message("Stopped llama.cpp server.")
  invisible(NULL)
}

#' @rdname llama_server
#' @export
llama_server_running <- function(host = "http://localhost:8080") {
  resp <- tryCatch(
    httr2::request(host) |>
      httr2::req_url_path("/health") |>
      httr2::req_timeout(2) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  !is.null(resp) && httr2::resp_status(resp) == 200
}

#' @rdname llama_server
#' @export
find_llama_server <- function() {
  # 1. Explicitly configured location takes priority.
  configured <- getOption("ditto.llama_server") %||%
    nzchar_or_null(Sys.getenv("DITTO_LLAMA_SERVER"))
  if (!is.null(configured) && file.exists(configured)) {
    return(configured)
  }

  exe_name <- if (.Platform$OS.type == "windows") {
    "llama-server.exe"
  } else {
    "llama-server"
  }

  # 2. On the PATH. Sys.which handles the executable extension on Windows.
  on_path <- Sys.which("llama-server")
  if (nzchar(on_path)) {
    return(unname(on_path))
  }

  # 3. A few common install locations.
  candidates <- file.path(llama_server_dirs(), exe_name)
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) {
    return(hit[[1]])
  }

  NULL
}

# Directories commonly holding a llama-server binary, by platform.
llama_server_dirs <- function() {
  home <- path.expand("~")
  dirs <- c(
    file.path(home, "tools", "llama.cpp"),
    file.path(home, "llama.cpp"),
    "/opt/homebrew/bin",
    "/usr/local/bin",
    "/usr/bin"
  )
  if (.Platform$OS.type == "windows") {
    dirs <- c(
      dirs,
      file.path(Sys.getenv("LOCALAPPDATA"), "llama.cpp"),
      "C:/Program Files/llama.cpp"
    )
  }
  dirs[nzchar(dirs)]
}

# Treat an empty environment variable as unset.
nzchar_or_null <- function(x) if (nzchar(x)) x else NULL
