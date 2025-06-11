#!/usr/bin/env Rscript

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Usage: Rscript post_draft.R <file_path> [dry_run] [draft_number] [update]\n")
  cat("  file_path    : Path to the thread markdown file (required)\n")
  cat("  dry_run      : TRUE/FALSE - preview without posting (default: FALSE)\n")
  cat("  draft_number : Which draft to post if multiple exist (default: 1)\n")
  cat("  update       : TRUE/FALSE - update file after posting (default: TRUE)\n")
  cat("\nExamples:\n")
  cat("  Rscript post_draft.R thread.md\n")
  cat("  Rscript post_draft.R thread.md TRUE\n")
  cat("  Rscript post_draft.R thread.md FALSE 2 TRUE\n")
  quit(status = 1)
}

# Load the package
library(twistr)

# Parse arguments with defaults
file_path <- args[1]
dry_run <- if (length(args) > 1) as.logical(args[2]) else FALSE
draft_number <- if (length(args) > 2) as.numeric(args[3]) else 1
update <- if (length(args) > 3) as.logical(args[4]) else TRUE

# Validate file exists
if (!file.exists(file_path)) {
  cat("Error: File not found:", file_path, "\n")
  quit(status = 1)
}

# Call the function with error handling
tryCatch({
  result <- post_comment_from_file(
    file_path = file_path,
    draft_number = draft_number,
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
