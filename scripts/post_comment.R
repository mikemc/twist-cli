#!/usr/bin/env Rscript

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Usage: post_comment.R <file_path> [dry_run] [update]\n")
  cat("  file_path    : Path to the thread markdown file (required)\n")
  cat("  dry_run      : TRUE/FALSE - preview without posting (default: FALSE)\n")
  cat("  update       : TRUE/FALSE - update file after posting (default: TRUE)\n")
  cat("\nExamples:\n")
  cat("  post_comment.R thread.md\n")
  cat("  post_comment.R thread.md TRUE\n")
  cat("  post_comment.R thread.md FALSE TRUE\n")
  quit(status = 1)
}

# Load the package
library(twistr)

# Parse arguments with defaults
file_path <- args[1]
dry_run <- if (length(args) > 1) as.logical(args[2]) else FALSE
update <- if (length(args) > 2) as.logical(args[3]) else TRUE

# Validate file exists
if (!file.exists(file_path)) {
  cat("Error: File not found:", file_path, "\n")
  quit(status = 1)
}

# Call the function with error handling
tryCatch({
  result <- post_comment_from_file(
    file_path = file_path,
    update = update,
    dry_run = dry_run
  )

  if (dry_run) {
    cat("Dry run completed successfully\n")
  } else {
    cat("Comment posted successfully\n")
  }
}, error = function(e) {
  cat("Error:", e$message, "\n")
  quit(status = 1)
})
