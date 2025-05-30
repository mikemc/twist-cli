# Workspace-level sync functions

#' Update all channels and threads in a workspace
#'
#' @param workspace_id ID of the workspace (default: from twist_workspace_id())
#' @param token Authentication token (default: from twist_token())
#' @param workspace_dir Directory to store workspace data
#' @param options List of update options:
#'   - thread_limit: Maximum number of threads to process per channel
#'   - newer_than_ts: Only process threads newer than this timestamp
#'   - older_than_ts: Only process threads older than this timestamp
#'   - force: Force update even if timestamps indicate no changes
#'   - timezone: Timezone for timestamps
#'
#' @return List of updated thread files
#' @export
update_workspace <- function(
  workspace_id = twist_workspace_id(),
  token = twist_token(),
  workspace_dir = twist_workspace_dir(),
  options = list()
) {
  # Ensure workspace directory exists
  if (!fs::dir_exists(workspace_dir)) {
    fs::dir_create(workspace_dir)
  }

  # Get all channels in workspace
  channels <- get_workspace_channels(workspace_id, token)

  # Update each channel and collect results
  updated_threads <- purrr::map(channels, function(channel) {
    update_channel(
      channel = channel,
      token = token,
      workspace_dir = workspace_dir,
      options = options
    )
  }) |> purrr::flatten()

  message(glue::glue("Workspace update complete. Updated {length(updated_threads)} thread files."))
  invisible(updated_threads)
}

# Channel-level sync functions

#' Update all threads in a channel
#'
#' @param channel Channel object or ID
#' @param token Authentication token
#' @param workspace_dir Directory containing workspace data
#' @param options List of options (see update_workspace)
#'
#' @return List of updated thread files
#' @export
update_channel <- function(
  channel,
  token = twist_token(),
  workspace_dir = twist_workspace_dir(),
  options = list()
) {
  # If channel is an ID, get the channel object
  if (is.numeric(channel) || is.character(channel)) {
    channel <- get_channel(channel, token)
  }

  # Create or update channel directory
  channel_dir_path <- handle_channel_directory(channel, workspace_dir)

  # Configure API request parameters
  thread_limit <- options$thread_limit %||% 500  # API max is 500
  params <- list(
    channel_id = channel$id,
    token = token,
    limit = min(100, thread_limit),  # Fetch in batches of 100 or less
    order_by = "desc"  # Most recent threads first
  )

  # Add optional filters if provided
  if (!is.null(options$newer_than_ts))
    params$newer_than_ts <- options$newer_than_ts
  if (!is.null(options$older_than_ts))
    params$older_than_ts <- options$older_than_ts

  message(glue::glue("Processing channel: {channel$name} (ID: {channel$id})"))

  # Get and update threads
  updated_threads <- batch_update_threads(
    params = params,
    channel_dir = channel_dir_path,
    token = token,
    force = options$force %||% FALSE,
    timezone = options$timezone %||% "UTC",
    max_threads = thread_limit
  )

  invisible(updated_threads)
}

# Helper functions (not exported)

#' Handle channel directory creation or renaming
#'
#' @param channel Channel object
#' @param workspace_dir Base workspace directory
#'
#' @return Path to the channel directory
handle_channel_directory <- function(channel, workspace_dir) {
  expected_dir_name <- channel_dir_name(channel)
  expected_path <- fs::path(workspace_dir, expected_dir_name)

  # Check if directory exists with correct name
  if (fs::dir_exists(expected_path)) {
    return(expected_path)
  }

  # Look for directories with matching channel ID
  channel_id_prefix <- paste0(channel$id, "_")
  existing_dirs <- fs::dir_ls(
    workspace_dir,
    regexp = paste0("^", channel_id_prefix),
    type = "directory",
    fail = FALSE
  )

  if (length(existing_dirs) > 0) {
    # Directory exists but with different name (channel renamed)
    old_dir <- existing_dirs[1]
    fs::file_move(old_dir, expected_path)
    message(glue::glue("Channel directory renamed: {fs::path_file(old_dir)} → {expected_dir_name}"))
  } else {
    # Directory doesn't exist, create it
    fs::dir_create(expected_path)
    message(glue::glue("Created channel directory: {expected_dir_name}"))
  }

  expected_path
}

#' Update threads in batches to minimize API requests
#'
#' @param params List of parameters for get_channel_threads
#' @param channel_dir Directory for channel files
#' @param token Authentication token
#' @param force Force update even if timestamps indicate no changes
#' @param timezone Timezone for timestamps
#' @param max_threads Maximum number of threads to process
#'
#' @return List of updated thread file paths
batch_update_threads <- function(
  params,
  channel_dir,
  token,
  force = FALSE,
  timezone = "UTC",
  max_threads = 500
) {
  updated_files <- list()
  threads_processed <- 0
  has_more <- TRUE

  # Get existing thread files and cache metadata
  thread_files <- get_existing_thread_files(channel_dir)

  # Loop to handle pagination
  while (has_more && threads_processed < max_threads) {
    # Adjust limit for this batch
    params$limit <- min(params$limit, max_threads - threads_processed)

    # Get threads for this batch
    threads <- do.call(get_channel_threads, params)

    if (length(threads) == 0) {
      break
    }

    # Process each thread
    batch_updates <- purrr::map(threads, function(thread) {
      process_thread(thread, thread_files, channel_dir, token, force, timezone)
    }) |> purrr::compact()

    updated_files <- c(updated_files, batch_updates)

    # Setup pagination for next batch
    threads_processed <- threads_processed + length(threads)
    if (length(threads) < params$limit) {
      has_more <- FALSE
    } else {
      # Use the oldest thread ID as the before_id for pagination
      oldest_id <- min(purrr::map_dbl(threads, "id"))
      params$before_id <- oldest_id
    }

    # Progress update
    message(glue::glue("  Processed {threads_processed} threads, updated {length(updated_files)} files"))
  }

  invisible(updated_files)
}

#' Process a single thread, creating or updating its file
#'
#' @param thread Thread object from API
#' @param thread_files List of existing thread files with metadata
#' @param channel_dir Directory for channel files
#' @param token Authentication token
#' @param force Force update even if timestamps indicate no changes
#' @param timezone Timezone for timestamps
#'
#' @return Path to updated file or NULL if no update needed
process_thread <- function(
  thread,
  thread_files,
  channel_dir,
  token,
  force = FALSE,
  timezone = "UTC"
) {
  thread_id <- as.character(thread$id)

  # Check if we have this thread file already
  if (thread_id %in% names(thread_files)) {
    existing_file <- thread_files[[thread_id]]

    # Check if file needs update based on timestamp
    if (force || needs_update(thread, existing_file)) {
      return(update_thread_file(existing_file$path, token, force, timezone))
    } else {
      return(NULL)  # No update needed
    }
  } else {
    # New thread, create file
    new_path <- write_thread(thread, token, channel_dir, timezone)
    return(new_path)
  }
}

#' Get existing thread files with metadata
#'
#' @param channel_dir Directory for channel files
#'
#' @return Named list of thread file info
get_existing_thread_files <- function(channel_dir) {
  # Get all markdown files in the directory
  md_files <- fs::dir_ls(channel_dir, regexp = "\\d+_.+\\.md$", type = "file")

  # Process each file to extract thread ID and last_updated_ts
  thread_files <- list()

  for (file_path in md_files) {
    tryCatch({
      # Extract thread ID from filename
      file_name <- fs::path_file(file_path)
      thread_id <- stringr::str_extract(file_name, "^\\d+")

      # Read YAML header to get last_updated_ts
      yaml_data <- read_yaml_header(file_path)

      thread_files[[thread_id]] <- list(
        path = file_path,
        last_updated_ts = yaml_data$last_updated_ts,
        thread_id = thread_id
      )
    }, error = function(e) {
      warning(glue::glue("Error processing thread file {file_path}: {e$message}"))
    })
  }

  thread_files
}

#' Determine if a thread file needs to be updated
#'
#' @param thread Thread object from API
#' @param thread_file Thread file info (from get_existing_thread_files)
#'
#' @return TRUE if update needed, FALSE otherwise
needs_update <- function(thread, thread_file) {
  api_timestamp <- thread$last_updated_ts
  file_timestamp <- thread_file$last_updated_ts

  # Update needed if API timestamp is newer
  api_timestamp > file_timestamp
}
