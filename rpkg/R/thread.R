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
  )
  thread_header <- c(
    "---",
    "title: '{thread$title}'",
    "author: {thread$creator_name} ({thread$creator})",
    "created: {posted_time}",
    "timezone: {timezone}",
    "thread_id: {thread$id}",
    "channel_id: {thread$channel_id}",
    "last_updated_ts: {thread$last_updated_ts}",
    "url: 'https://twist.com/a/{thread$workspace_id}/ch/{thread$channel_id}/t/{thread$id}/'",
    "---"
  ) |> purrr::map_chr(\(x) stringr::str_glue(x, .envir = environment()))

  readr::write_lines(thread_header, thread_path)
  readr::write_lines(c("", thread$content), thread_path, append = TRUE)
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
  comment_text <- c(
    "---",
    stringr::str_glue("*Comment by {comment$creator_name} ({comment$creator}) on {comment_time} (Comment {comment$id})*"),
    "",
    comment$content
  )
  comment_text
}

#' Read YAML header from a Markdown file
#'
#' @param file Path to the Markdown file
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
#' If the thread title has been updated, then the thread file will be renamed to match.
#'
#' @param path Path to the thread file
#' @param token Authentication token
#' @param force Logical. Whether to force rewriting even if timestamps indicate no update (default: FALSE)
#' @param timezone Timezone for timestamps; if NULL, use timezone from existing thread file
#'
#' @return Path to updated file, or NULL if file not updated
#' @export
update_thread_file <- function(
    path,
    token = twist_token(),
    force = FALSE,
    timezone = NULL) {
  # Extract required metadata from the file
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
