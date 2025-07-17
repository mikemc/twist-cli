#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Usage: Rscript print_thread.R <thread_id> [timezone]\n")
  cat("  thread_id    : ID of the thread to print (required)\n")
  cat("  timezone     : Timezone for timestamps (default: UTC)\n")
  cat("\nExamples:\n")
  cat("  Rscript print_thread.R 1234567\n")
  cat("  Rscript print_thread.R 1234567 'America/New_York'\n")
  cat("  Rscript print_thread.R 1234567 'Europe/London'\n")
  quit(status = 1)
}

library(twistr)

thread_id <- args[1]
timezone <- if (length(args) > 1) args[2] else "UTC"

if (is.na(as.numeric(thread_id))) {
  cat("Error: thread_id must be a number\n")
  quit(status = 1)
}

tryCatch({
  result <- thread_to_string(
    thread = as.numeric(thread_id),
    timezone = timezone
  )

  cat(result)
}, error = function(e) {
  cat("Error:", e$message, "\n")
  quit(status = 1)
})
