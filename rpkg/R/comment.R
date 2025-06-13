#' Post a comment to a thread
#'
#' @param thread_id ID of the thread to comment on
#' @param content Content of the comment (supports Markdown)
#' @param token Authentication token
#' @param recipients Users to notify. Can be a list of user IDs, "EVERYONE",
#'   or "EVERYONE_IN_THREAD" (default: "EVERYONE_IN_THREAD")
#' @param direct_mentions List of user IDs that are directly mentioned
#' @param direct_group_mentions List of group IDs that are directly mentioned
#' @param groups List of group IDs to notify
#' @param mark_thread_position Whether to mark the thread position (default: TRUE)
#' @param thread_action Optional action: "close" or "reopen"
#'
#' @return Comment object from the API
#' @export
post_comment <- function(
  thread_id,
  content,
  token = twist_token(),
  recipients = "EVERYONE_IN_THREAD",
  direct_mentions = NULL,
  direct_group_mentions = NULL,
  groups = NULL,
  mark_thread_position = TRUE,
  thread_action = NULL
) {
  params <- as.list(environment()) |>
    purrr::discard_at("token") |>
    purrr::compact()

  response <- twist_request(
    "comments/add",
    params = params,
    token = token,
    method = "POST"
  )

  invisible(httr2::resp_body_json(response))
}

#' Post a draft comment from a thread file
#'
#' Reads a draft comment from the end of a thread file and posts it to the
#' corresponding Twist thread. A draft comment is indicated by an H1 heading
#' with the text "DRAFT COMMENT" (case insensitive) followed by optional YAML
#' parameters. Assumes draft comment continues to the end of the file.
#'
#' @param file_path Path to the thread markdown file
#' @param update Whether to update the thread file after posting to show the new comment (default: TRUE)
#' @param dry_run If TRUE, show what would be posted without actually posting (default: FALSE)
#' @param token Authentication token
#'
#' @return The posted comment object (invisible), or preview text if dry_run=TRUE
#' @export

post_comment_from_file <- function(
  file_path,
  update = TRUE,
  dry_run = FALSE,
  token = twist_token()
) {
  if (!fs::file_exists(file_path)) {
    stop("Thread file not found: ", file_path)
  }

  lines <- readr::read_lines(file_path)

  yaml_header <- read_yaml_header(file_path)
  thread_id <- yaml_header$thread_id

  if (is.null(thread_id)) {
    stop("No thread_id found in file header")
  }

  draft_section <- find_comment_section(lines, pattern = "^# DRAFT COMMENT")

  if (is.null(draft_section)) {
    stop("No draft comment found in file")
  }

  draft <- parse_comment_section(
    lines,
    draft_section,
    valid_params = c(
      "recipients", "direct_mentions", "direct_group_mentions",
      "groups", "mark_thread_position", "thread_action"
    )
  )

  if (dry_run) {
    cat("Would post comment to thread", thread_id, ":\n")
    cat("Parameters:", yaml::as.yaml(draft$params))
    cat("Content:\n", draft$content, "\n")
    return(invisible(draft))
  }

  # Use do.call to dynamically pass YAML parameters alongside required ones
  comment_result <- tryCatch({
    do.call(post_comment, c(
      list(thread_id = thread_id, content = draft$content, token = token),
      draft$params
    ))
  }, error = function(e) {
    stop("Failed to post comment: ", e$message)
  })

  if (is.null(comment_result) || is.null(comment_result$id)) {
    stop("Comment posting failed - no comment ID returned")
  }

  if (update) {
    update_thread_file(file_path, token = token, force = TRUE)
    message("Draft comment posted and thread file updated")
  } else {
    message("Draft comment posted (thread file not updated)")
  }

  invisible(comment_result)
}

#' Update an existing comment in Twist
#'
#' @param id ID of the comment to update
#' @param content New content for the comment (supports Markdown)
#' @param token Authentication token
#' @param direct_mentions List of user IDs that are directly mentioned
#' @param direct_group_mentions List of group IDs that are directly mentioned
#'
#' @return Updated comment object from the API
#' @export
update_comment <- function(
  id,
  content,
  token = twist_token(),
  direct_mentions = NULL,
  direct_group_mentions = NULL
) {
  params <- as.list(environment()) |>
    purrr::discard_at("token") |>
    purrr::compact()

  response <- twist_request(
    "comments/update",
    params = params,
    token = token,
    method = "POST"
  )

  invisible(httr2::resp_body_json(response))
}

#' Update a comment from a thread file
#'
#' Reads an edited comment from the end of a thread file and updates the
#' corresponding comment in Twist. An edited comment is indicated by an H1
#' heading with "EDIT COMMENT" (case insensitive) followed by optional YAML
#' parameters including the comment id. Assumes edited comment continues to
#' the end of the file.
#'
#' @param file_path Path to the thread markdown file
#' @param update Whether to update the thread file after editing (default: TRUE)
#' @param dry_run If TRUE, show what would be updated without actually updating (default: FALSE)
#' @param token Authentication token
#'
#' @return The updated comment object (invisible), or preview text if dry_run=TRUE
#' @export
update_comment_from_file <- function(
  file_path,
  update = TRUE,
  dry_run = FALSE,
  token = twist_token()
) {
  if (!fs::file_exists(file_path)) {
    stop("Thread file not found: ", file_path)
  }

  lines <- readr::read_lines(file_path)

  yaml_header <- read_yaml_header(file_path)
  thread_id <- yaml_header$thread_id

  if (is.null(thread_id)) {
    stop("No thread_id found in file header")
  }

  edit_section <- find_comment_section(lines, pattern = "^# EDIT COMMENT")

  if (is.null(edit_section)) {
    stop("No edit comment found in file")
  }

  edit <- parse_comment_section(
    lines,
    edit_section,
    valid_params = c("id", "direct_mentions", "direct_group_mentions"),
    required_params = "id"
  )

  if (dry_run) {
    cat("Would update comment", edit$params$id, ":\n")
    cat("Parameters:", yaml::as.yaml(edit$params))
    cat("Content:\n", edit$content, "\n")
    return(invisible(edit))
  }

  # Update the comment
  comment_result <- tryCatch({
    do.call(update_comment, c(
      list(content = edit$content, token = token),
      edit$params
    ))
  }, error = function(e) {
    stop("Failed to update comment: ", e$message)
  })

  if (is.null(comment_result) || is.null(comment_result$id)) {
    stop("Comment update failed - no comment ID returned")
  }

  if (update) {
    update_thread_file(file_path, token = token, force = TRUE)
    message("Comment updated and thread file refreshed")
  } else {
    message("Comment updated (thread file not refreshed)")
  }

  invisible(comment_result)
}

#' Find a comment section in file
#'
#' Generic function to find a single comment section (draft or edit) based on pattern. Assumes comment continues to end of file.
#'
#' @param lines Character vector of file lines
#' @param pattern Regular expression pattern to match section header
#'
#' @return List with start_line and end_line, or NULL if not found
find_comment_section <- function(lines, pattern) {
  section_starts <- which(grepl(pattern, lines, ignore.case = TRUE))

  if (length(section_starts) == 0) {
    return(NULL)
  }

  if (length(section_starts) > 1) {
    stop("Multiple comment sections found. Only one is supported per file.")
  }

  start_line <- section_starts[1]
  end_line <- length(lines)

  list(start_line = start_line, end_line = end_line)
}

#' Parse a comment section (draft or edit)
#'
#' Generic function to parse YAML parameters and content from comment sections
#'
#' @param lines Character vector of file lines
#' @param section Section info from find_comment_section
#' @param valid_params Character vector of valid parameter names for validation
#' @param required_params Character vector of required parameter names
#'
#' @return List with 'params' and 'content' elements
parse_comment_section <- function(lines, section, valid_params = NULL, required_params = NULL) {
  section_lines <- lines[section$start_line:section$end_line]

  params <- list()
  content_start <- 2

  yaml_start_line <- NULL
  for (i in 2:length(section_lines)) {
    line <- section_lines[i]
    if (trimws(line) == "") {
      next
    } else if (grepl("^```yaml\\s*$", line)) {
      yaml_start_line <- i
      break
    } else {
      content_start <- i
      break
    }
  }

  if (!is.null(yaml_start_line)) {
    yaml_end_line <- NULL
    for (i in (yaml_start_line + 1):length(section_lines)) {
      if (grepl("^```\\s*$", section_lines[i])) {
        yaml_end_line <- i
        break
      }
    }

    if (!is.null(yaml_end_line)) {
      yaml_lines <- section_lines[(yaml_start_line + 1):(yaml_end_line - 1)]

      if (length(yaml_lines) > 0) {
        yaml_content <- paste(yaml_lines, collapse = "\n")
        params <- yaml::yaml.load(yaml_content)
        params <- purrr::compact(params)
      }

      content_start <- yaml_end_line + 1
    } else {
      # Graceful degradation: continue parsing even with malformed YAML block
      warning("YAML block found but no closing ``` detected")
      content_start <- yaml_start_line + 1
    }
  }

  # Parameter validation must happen after YAML parsing to avoid premature failures
  if (!is.null(valid_params) && length(params) > 0) {
    invalid_params <- setdiff(names(params), valid_params)
    if (length(invalid_params) > 0) {
      warning("Unknown parameters ignored: ", paste(invalid_params, collapse = ", "))
      params <- params[names(params) %in% valid_params]
    }
  }

  if (!is.null(required_params)) {
    missing_params <- setdiff(required_params, names(params))
    if (length(missing_params) > 0) {
      stop("Required parameters missing: ", paste(missing_params, collapse = ", "))
    }
  }

  if (content_start <= length(section_lines)) {
    content_lines <- section_lines[content_start:length(section_lines)]
    # Preserve meaningful whitespace while removing leading/trailing noise
    content_lines <- content_lines[cumsum(content_lines != "") > 0]
    if (length(content_lines) > 0) {
      while (length(content_lines) > 0 && content_lines[length(content_lines)] == "") {
        content_lines <- content_lines[-length(content_lines)]
      }
    }
    content <- paste(content_lines, collapse = "\n")
  } else {
    content <- ""
  }

  if (nchar(trimws(content)) == 0) {
    stop("Comment section has no content")
  }

  list(params = params, content = content)
}
