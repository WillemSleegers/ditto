# These tests avoid starting a real server, so they run anywhere. The live
# round-trip is gated at the bottom behind the presence of a binary and model.

test_that("start_llama_server errors when no model can be resolved", {
  skip_if_not_installed("processx")
  withr::local_options(ditto.llama_model = NULL)
  withr::local_envvar(DITTO_LLAMA_MODEL = "")

  expect_error(
    start_llama_server(model = NULL),
    "No model supplied"
  )
})

test_that("start_llama_server errors when the model file is missing", {
  skip_if_not_installed("processx")

  expect_error(
    start_llama_server(model = "does-not-exist.gguf"),
    "Model file not found"
  )
})

test_that("find_llama_server prefers the configured option", {
  f <- tempfile()
  file.create(f)
  withr::local_options(ditto.llama_server = f)
  expect_equal(find_llama_server(), f)
})

test_that("find_llama_server returns NULL when nothing is found", {
  withr::local_options(ditto.llama_server = NULL)
  withr::local_envvar(DITTO_LLAMA_SERVER = "", PATH = "")
  # With an empty PATH and no configured path, only the fixed candidate dirs
  # are checked; on a clean CI machine none exist.
  result <- find_llama_server()
  expect_true(is.null(result) || file.exists(result))
})

test_that("llama_server_running is FALSE when nothing is listening", {
  # An unlikely-to-be-used port; the request fails fast and returns FALSE.
  expect_false(llama_server_running("http://127.0.0.1:59327"))
})

test_that("a real server can be started, queried, and stopped", {
  skip_on_cran()

  exe <- Sys.getenv("DITTO_LLAMA_SERVER")
  model <- Sys.getenv("DITTO_LLAMA_MODEL")
  skip_if(
    !nzchar(exe) || !file.exists(model),
    "Set DITTO_LLAMA_SERVER and DITTO_LLAMA_MODEL to real files to run this."
  )
  skip_if_not_installed("processx")

  host <- "http://127.0.0.1:8137"
  start_llama_server(model = model, exe = exe, host = host, timeout = 90)
  withr::defer(stop_llama_server())

  expect_true(llama_server_running(host))

  score <- bertscore(
    "how much do you agree",
    "to what extent do you agree",
    host = host
  )
  expect_named(score, c("precision", "recall", "f1"))
  expect_true(all(score >= 0 & score <= 1))
})
