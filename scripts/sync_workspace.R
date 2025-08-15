#!/usr/bin/env Rscript

library(twistr)

# Helper to safely get the value following a command-line flag
get_next_arg <- function(args, i, flag_name) {
  if (i + 1 > length(args)) {
    cat("Error:", flag_name, "requires a value\n")
    quit(status = 1)
  }
  args[i + 1]
}

parse_args <- function(args) {
  options <- list(
    force = FALSE,
    thread_limit = 20,
    timezone = "UTC",
    newer_than_ts = NULL,
    older_than_ts = NULL,
    channels = NULL,
    verbose = FALSE
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[i]

    if (arg %in% c("-h", "--help")) {
      show_help()
      quit(status = 0)
    } else if (arg %in% c("-f", "--force")) {
      options$force <- TRUE
    } else if (arg %in% c("-l", "--limit")) {
      options$thread_limit <- as.numeric(get_next_arg(args, i, "--limit"))
      i <- i + 1
    } else if (arg %in% c("-t", "--timezone")) {
      options$timezone <- get_next_arg(args, i, "--timezone")
      i <- i + 1
    } else if (arg %in% c("-n", "--newer-than")) {
      options$newer_than_ts <- as.numeric(get_next_arg(args, i, "--newer-than"))
      i <- i + 1
    } else if (arg %in% c("-o", "--older-than")) {
      options$older_than_ts <- as.numeric(get_next_arg(args, i, "--older-than"))
      i <- i + 1
    } else if (arg %in% c("-c", "--channels")) {
      channel_ids <- strsplit(get_next_arg(args, i, "--channels"), ",")[[1]]
      options$channels <- as.numeric(trimws(channel_ids))
      i <- i + 1
    } else if (arg %in% c("-v", "--verbose")) {
      options$verbose <- TRUE
    } else {
      cat("Error: Unknown argument:", arg, "\n")
      cat("Run with --help for usage information.\n")
      quit(status = 1)
    }

    i <- i + 1
  }

  validate_options(options)
  options
}


show_help <- function() {
  cat("Sync Twist workspace threads to local markdown files\n\n")
  cat("Usage: Rscript sync_workspace.R [options]\n\n")
  cat("Options:\n")
  cat("  -h, --help              Show this help message\n")
  cat("  -f, --force             Force sync even if no changes detected\n")
  cat("  -l, --limit NUM         Maximum threads per channel (default: 20, max: 500)\n")
  cat("  -t, --timezone TZ       Timezone for timestamps (default: UTC)\n")
  cat("  -n, --newer-than TS     Only process threads newer than Unix timestamp\n")
  cat("  -o, --older-than TS     Only process threads older than Unix timestamp\n")
  cat("  -c, --channels IDS      Comma-separated channel IDs to sync (default: all)\n")
  cat("  -v, --verbose           Enable verbose output\n\n")
  cat("Examples:\n")
  cat("  Rscript sync_workspace.R\n")
  cat("  Rscript sync_workspace.R --force --limit 100\n")
  cat("  Rscript sync_workspace.R --channels 12345,67890\n")
  cat("  Rscript sync_workspace.R --newer-than 1640995200 --timezone 'America/New_York'\n")
  cat("  Rscript sync_workspace.R --channels 12345 --force --verbose\n\n")
  cat("Environment Variables:\n")
  cat("  TWIST_TOKEN        - Your Twist API token (required)\n")
  cat("  TWIST_WORKSPACE_ID - Target workspace ID (required)\n")
  cat("  TWIST_WORKSPACE_DIR- Local directory for workspace files (required)\n")
}

validate_options <- function(options) {
  if (!is.numeric(options$thread_limit) || options$thread_limit <= 0 || options$thread_limit > 500) {
    cat("Error: thread_limit must be a positive number (max 500)\n")
    quit(status = 1)
  }

  for (ts_field in c("newer_than_ts", "older_than_ts")) {
    ts_value <- options[[ts_field]]
    if (!is.null(ts_value) && (is.na(ts_value) || ts_value <= 0)) {
      cat("Error:", ts_field, "must be a positive Unix timestamp\n")
      quit(status = 1)
    }
  }

  if (!is.null(options$channels) && (any(is.na(options$channels)) || any(options$channels <= 0))) {
    cat("Error: channel IDs must be positive numbers\n")
    quit(status = 1)
  }
}

args <- commandArgs(trailingOnly = TRUE)
options <- parse_args(args)

# Main sync logic - handles both full workspace sync and selective channel sync
sync_channels <- function(channels, sync_options) {
  if (is.null(channels)) {
    # Sync entire workspace when no specific channels requested
    result <- sync_workspace(options = sync_options)
  } else {
    # Manual channel iteration - allows continuing sync even if individual channels fail
    workspace_id <- twist_workspace_id()
    token <- twist_token()
    workspace_dir <- twist_workspace_dir()

    if (!fs::dir_exists(workspace_dir)) {
      fs::dir_create(workspace_dir)
    }

    updated_threads <- list()
    for (channel_id in channels) {
      tryCatch({
        if (options$verbose) {
          cat("Fetching channel", channel_id, "...\n")
        }
        channel <- get_channel(channel_id, token)
        result <- sync_channel(
          channel = channel,
          token = token,
          workspace_dir = workspace_dir,
          options = sync_options
        )
        updated_threads <- c(updated_threads, result)
      }, error = function(e) {
        cat("Error syncing channel", channel_id, ":", e$message, "\n")
      })
    }

    message(glue::glue("Channel sync complete. Updated {length(updated_threads)} thread files."))
    invisible(updated_threads)
  }
}

# Convert CLI options to twistr::sync_workspace() and sync_channel()
# parameters. Only includes non-default values to avoid overriding package
# defaults
build_sync_options <- function(options) {
  sync_opts <- list()
  if (options$force) sync_opts$force <- TRUE
  sync_opts$thread_limit <- options$thread_limit
  if (options$timezone != "UTC") sync_opts$timezone <- options$timezone
  if (!is.null(options$newer_than_ts)) sync_opts$newer_than_ts <- options$newer_than_ts
  if (!is.null(options$older_than_ts)) sync_opts$older_than_ts <- options$older_than_ts
  sync_opts
}

tryCatch({
  if (options$verbose) {
    cat("Starting sync with options:\n")
    cat("  Force:", options$force, "\n")
    cat("  Thread limit:", options$thread_limit, "\n")
    cat("  Timezone:", options$timezone, "\n")
    if (!is.null(options$newer_than_ts)) {
      cat("  Newer than:", as.POSIXct(options$newer_than_ts, origin = "1970-01-01"), "\n")
    }
    if (!is.null(options$older_than_ts)) {
      cat("  Older than:", as.POSIXct(options$older_than_ts, origin = "1970-01-01"), "\n")
    }
    if (!is.null(options$channels)) {
      cat("  Channels:", paste(options$channels, collapse = ", "), "\n")
    }
    cat("\n")
  } else {
    if (is.null(options$channels)) {
      cat("Starting workspace sync...\n")
    } else {
      cat("Starting channel sync...\n")
    }
  }

  sync_opts <- build_sync_options(options)
  result <- sync_channels(options$channels, sync_opts)

  cat("Sync completed successfully\n")

}, error = function(e) {
  cat("Error:", e$message, "\n")

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
