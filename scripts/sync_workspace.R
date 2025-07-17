#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0 && args[1] %in% c("-h", "--help", "help")) {
  cat("Usage: Rscript sync_workspace.R [force] [thread_limit] [timezone] [newer_than_ts]\n")
  cat("  force         : TRUE/FALSE - force sync even if no changes detected (default: FALSE)\n")
  cat("  thread_limit  : Maximum threads per channel to process (default: 20)\n")
  cat("  timezone      : Timezone for timestamps (default: UTC)\n")
  cat("  newer_than_ts : Only process threads newer than this Unix timestamp (optional)\n")
  cat("\nExamples:\n")
  cat("  Rscript sync_workspace.R\n")
  cat("  Rscript sync_workspace.R TRUE\n")
  cat("  Rscript sync_workspace.R FALSE 100 'America/New_York'\n")
  cat("  Rscript sync_workspace.R FALSE 20 UTC 1640995200\n")
  cat("\nNote: Set TWIST_TOKEN, TWIST_WORKSPACE_ID, and TWIST_WORKSPACE_DIR\n")
  cat("      environment variables or configure them in R before running.\n")
  quit(status = 0)
}

library(twistr)

force <- if (length(args) > 0) as.logical(args[1]) else FALSE
thread_limit <- if (length(args) > 1) as.numeric(args[2]) else 20
timezone <- if (length(args) > 2) args[3] else "UTC"
newer_than_ts <- if (length(args) > 3) as.numeric(args[4]) else NULL

if (!is.na(force) && !is.logical(force)) {
  cat("Error: force must be TRUE or FALSE\n")
  quit(status = 1)
}

if (!is.na(thread_limit) && (!is.numeric(thread_limit) || thread_limit <= 0)) {
  cat("Error: thread_limit must be a positive number\n")
  quit(status = 1)
}

if (!is.null(newer_than_ts) && (is.na(newer_than_ts) || newer_than_ts <= 0)) {
  cat("Error: newer_than_ts must be a positive Unix timestamp\n")
  quit(status = 1)
}

# Build options list, excluding default/empty values
options <- list()
if (!is.na(force)) options$force <- force
if (!is.na(thread_limit)) options$thread_limit <- thread_limit
if (!is.na(timezone)) options$timezone <- timezone
if (!is.null(newer_than_ts) && !is.na(newer_than_ts)) options$newer_than_ts <- newer_than_ts

tryCatch({
  cat("Starting workspace sync...\n")

  if (force) {
    cat("Force sync enabled - will sync all files regardless of timestamps\n")
  }

  if (!is.null(newer_than_ts)) {
    cat("Only processing threads newer than:", as.POSIXct(newer_than_ts, origin = "1970-01-01"), "\n")
  }

  result <- sync_workspace(options = options)

  cat("Workspace sync completed successfully\n")

}, error = function(e) {
  cat("Error:", e$message, "\n")

  # Match specific error patterns to provide targeted setup guidance
  if (grepl("No Twist API token found", e$message)) {
    cat("\nHint: Set your API token with:\n")
    cat("  export TWIST_TOKEN='your-token-here'\n")
    cat("Or configure it in R with twist_token('your-token-here')\n")
  }

  if (grepl("No Twist workspace ID found", e$message)) {
    cat("\nHint: Set your workspace ID with:\n")
    cat("  export TWIST_WORKSPACE_ID='your-workspace-id'\n")
    cat("Or configure it in R with twist_workspace_id('your-workspace-id')\n")
  }

  if (grepl("No Twist workspace.*directory", e$message)) {
    cat("\nHint: Set your workspace directory with:\n")
    cat("  export TWIST_WORKSPACE_DIR='./my-workspace'\n")
    cat("Or configure it in R with twist_workspace_dir('./my-workspace')\n")
  }

  quit(status = 1)
})
