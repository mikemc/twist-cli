# Thread-level functions

#' Get a single thread by ID
#'
#' @param thread_id ID of the thread
#' @param token Authentication token
#'
#' @return Thread as a list
#' @export
get_thread <- function(thread_id, token = twist_token()) {
  response <- twist_request(
    "threads/getone",
    params = list(id = thread_id),
    token = token
  )
  httr2::resp_body_json(response)
}

#' Get comments for a thread
#'
#' @param thread_id ID of the thread
#' @param token Authentication token
#'
#' @return List of comments
#' @export
get_thread_comments <- function(thread_id, token = twist_token()) {
  response <- twist_request(
    "comments/get",
    params = list(thread_id = thread_id),
    token = token
  )
  httr2::resp_body_json(response)
}

#' Write a thread to a Markdown file
#'
#' @param thread Thread object or ID
#' @param token Authentication token
#' @param dir Directory to write the file to (default: current directory)
#' @param timezone Timezone for timestamps (default: "UTC")
#'
#' @return Path to the created file
#' @export
write_thread <- function(
  thread,
  token = twist_token(),
  dir = ".",
  timezone = "UTC"
) {
  if (is.numeric(thread) || is.character(thread)) {
    thread <- get_thread(thread, token)
  }
  comments <- get_thread_comments(thread$id, token)
  thread_path <- fs::path(dir, thread_file_name(thread))
  posted_time <- as.POSIXct(
    thread$posted_ts,
    origin = "1970-01-01",
    tz = timezone
  ) |>
    as.character()

  thread_metadata <- list(
    title = thread$title,
    author = glue::glue("{thread$creator_name} ({thread$creator})"),
    created = posted_time,
    timezone = timezone,
    thread_id = thread$id,
    channel_id = thread$channel_id,
    last_updated_ts = thread$last_updated_ts,
    url = glue::glue("https://twist.com/a/{thread$workspace_id}/ch/{thread$channel_id}/t/{thread$id}/")
  )
  # The output of as.yaml already includes a trailing newline character, so we
  # need to take care to avoid adding an extra newline.
  thread_header <- stringr::str_c(
    "---\n",
    yaml::as.yaml(thread_metadata),
    "---"
  )
  thread_content_shifted <- shift_markdown_headers(thread$content)

  readr::write_lines(thread_header, thread_path)
  readr::write_lines(c("", thread_content_shifted), thread_path, append = TRUE)
  for (comment in comments) {
    readr::write_lines(
      c("", comment_to_string(comment, timezone)),
      thread_path,
      append = TRUE
    )
  }

  thread_path
}

#' Create a file name for a thread
#'
#' @param thread Thread returned by `get_thread()`
#'
#' @return String to use as file name
thread_file_name <- function(thread) {
  title_clean <- thread$title |>
    janitor::make_clean_names() |>
    stringr::str_replace_all("_", "-")
  stringr::str_glue("{thread$id}_{title_clean}.md")
}

#' Shift markdown header levels
#'
#' Parses markdown content and shifts all header levels by a specified amount.
#' Headers are capped at H6 (cannot go beyond level 6).
#'
#' @param content Character string containing markdown content
#' @param shift Integer specifying the shift amount (positive shifts down,
#'   negative shifts up)
#'
#' @return Character string with shifted header levels
shift_markdown_headers <- function(content, shift = 1) {
  lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
  in_code_block <- FALSE

  shifted_lines <- purrr::map_chr(lines, function(line) {
    # Toggle code block state
    if (grepl("^```", line)) {
      in_code_block <- !in_code_block
    }

    # Only process headers outside code blocks
    if (!in_code_block && grepl("^#{1,6}\\s", line)) {
      # Extract current header level
      current_level <- nchar(gsub("^(#{1,6})\\s.*", "\\1", line))
      new_level <- pmax(1, pmin(current_level + shift, 6))

      # Replace header markers
      header_text <- gsub("^#{1,6}\\s", "", line)
      line <- paste0(strrep("#", new_level), " ", header_text)
    }

    line
  })

  paste(shifted_lines, collapse = "\n")
}

#' Convert comment to string
#'
#' Get the text representation of a comment for writing to a thread file.
#'
#' @param comment Comment from list returned by `get_comments()`
#' @param timezone Timezone for timestamps (default: "UTC")
comment_to_string <- function(comment, timezone = "UTC") {
  comment_time <- as.POSIXct(
    comment$posted_ts,
    origin = "1970-01-01",
    tz = timezone
  )
  comment_content_shifted <- shift_markdown_headers(comment$content)

  header <- stringr::str_glue(
    "# Comment by {comment$creator_name} ({comment$creator}) at {comment_time} (Comment {comment$id})"
  )

  c(header, "", comment_content_shifted)
}

#' Read YAML header from a Markdown file
#'
#' @param file Path to the Markdown file
#' @param n_max Maximum number of lines from the file to check for YAML content
#'
#' @return List containing the YAML metadata
#' @export
read_yaml_header <- function(file, n_max = 20) {
  lines <- readr::read_lines(file, n_max = n_max)
  if (lines[1] == "---") {
    end_yaml <- which(lines == "---")[2]
    if (is.na(end_yaml)) {
      stop("Invalid markdown file format: no closing '---' for YAML header found.")
    }
    yaml_content <- lines[2:(end_yaml - 1)]
    yaml_parsed <- yaml::yaml.load(paste(yaml_content, collapse = "\n"))
  } else {
    stop("Invalid markdown file format: no YAML header found.")
  }
  yaml_parsed
}

#' Update a thread file if it has been updated in Twist
#'
#' If the thread title has been updated, then the thread file will be renamed
#' to match. If the file doesn't exist, it will be created.
#'
#' @param path Path to the thread file
#' @param token Authentication token
#' @param force Logical. Whether to force rewriting even if timestamps indicate
#'   no update (default: FALSE)
#' @param timezone Timezone for timestamps; if NULL, use timezone from existing
#'   thread file
#'
#' @return Path to updated file, or NULL if file not updated
#' @export
update_thread_file <- function(
    path,
    token = twist_token(),
    force = FALSE,
    timezone = NULL) {
  # If the file doesn't already exist, write the thread to file and exit
  if (!fs::file_exists(path)) {
    dir <- fs::path_dir(path)
    file_name <- fs::path_file(path)
    thread_id <- as.numeric(stringr::str_extract(file_name, "^\\d+"))
    thread <- get_thread(thread_id, token)
    path_new <- write_thread(thread, token, dir, timezone = timezone %||% "UTC")
    message("Thread file created: ", path_new)
    return(path_new)
  }
  # Otherwise, load the thread metadata from the file, determine if we need to
  # update, and update if necessary
  yaml_data <- read_yaml_header(path)
  thread_id <- yaml_data$thread_id
  local_last_updated_ts <- yaml_data$last_updated_ts
  # Get the current thread data from Twist
  thread <- get_thread(thread_id, token)
  twist_last_updated_ts <- thread$last_updated_ts
  # Compare timestamps to see if an update is needed
  needs_update <- force || (twist_last_updated_ts > local_last_updated_ts)

  if (needs_update) {
    if (is.null(timezone)) {
      timezone <- yaml_data$timezone
    }
    dir <- fs::path_dir(path)
    path_new <- write_thread(thread, token, dir, timezone)
    # A title changes will lead to a new file name, so we need to delete the previous file
    file_new <- fs::path_file(path_new)
    file_original <- fs::path_file(path)
    if (file_original == file_new) {
      message("Thread file updated: ", path_new)
    } else {
      fs::file_delete(path)
      message("Thread file updated and renamed: ", path_new)
    }
    return(path_new)
  } else {
    message("Thread file is already up to date: ", path)
    return(NULL)
  }
}
