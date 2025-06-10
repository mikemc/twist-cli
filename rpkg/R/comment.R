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
  params <- purrr::compact(as.list(environment())[-3])

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
#' corresponding Twist thread. Draft comments are indicated by an H1 heading
#' with the text "DRAFT COMMENT" followed by optional YAML parameters.
#' Assumes draft comments are located at the end of the file.
#'
#' @param file_path Path to the thread markdown file
#' @param draft_number Which draft to post if multiple exist at end (default: 1, meaning first found)
#' @param update Whether to update the thread file after posting to show the new comment (default: TRUE)
#' @param dry_run If TRUE, show what would be posted without actually posting (default: FALSE)
#' @param token Authentication token
#'
#' @return The posted comment object (invisible), or preview text if dry_run=TRUE
#' @export
post_comment_from_file <- function(
  file_path,
  draft_number = 1,
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

  draft_sections <- find_draft_comments(lines)

  if (length(draft_sections) == 0) {
    stop("No draft comments found in file")
  }

  if (draft_number > length(draft_sections)) {
    stop("Draft comment ", draft_number, " not found. Only ", length(draft_sections), " draft(s) exist.")
  }

  draft <- parse_draft_comment(lines, draft_sections[[draft_number]])

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

#' Find draft comment sections at the end of file
#'
#' @param lines Character vector of file lines
#'
#' @return List of draft section info (start_line, end_line)
find_draft_comments <- function(lines) {
  # Only look for draft comments, assuming they're all at the end
  draft_starts <- which(grepl("^# DRAFT COMMENT", lines))

  if (length(draft_starts) == 0) {
    return(list())
  }

  sections <- list()

  for (i in seq_along(draft_starts)) {
    start_line <- draft_starts[i]

    # Each section ends where the next one begins, or at EOF for the last one
    if (i < length(draft_starts)) {
      end_line <- draft_starts[i + 1] - 1
    } else {
      end_line <- length(lines)
    }

    sections[[i]] <- list(
      start_line = start_line,
      end_line = end_line
    )
  }

  sections
}

#' Parse a single draft comment section
#'
#' @param lines Character vector of file lines
#' @param section Section info from find_draft_comments
#'
#' @return List with 'params' and 'content' elements
parse_draft_comment <- function(lines, section) {
  section_lines <- lines[section$start_line:section$end_line]

  params <- list()
  content_start <- 2  # Start after the "# DRAFT COMMENT" line

  # YAML parameters are optional - look for ```yaml code block after header
  if (length(section_lines) >= 2 && grepl("^```yaml\\s*$", section_lines[2])) {
    yaml_end <- which(grepl("^```\\s*$", section_lines[3:length(section_lines)]))[1]

    if (!is.na(yaml_end)) {
      yaml_end <- yaml_end + 2  # Adjust for offset
      yaml_lines <- section_lines[3:yaml_end-1]

      if (length(yaml_lines) > 0) {
        yaml_content <- paste(yaml_lines, collapse = "\n")
        params <- yaml::yaml.load(yaml_content)

        params <- purrr::compact(params)

        # Validate against post_comment() parameters to catch typos early
        valid_params <- c("recipients", "direct_mentions", "direct_group_mentions",
                         "groups", "mark_thread_position", "thread_action")
        invalid_params <- setdiff(names(params), valid_params)
        if (length(invalid_params) > 0) {
          warning("Unknown parameters ignored: ", paste(invalid_params, collapse = ", "))
          params <- params[names(params) %in% valid_params]
        }
      }

      content_start <- yaml_end + 1
    }
  }

  if (content_start <= length(section_lines)) {
    content_lines <- section_lines[content_start:length(section_lines)]
    # Strip leading empty lines but preserve internal structure
    content_lines <- content_lines[cumsum(content_lines != "") > 0]
    if (length(content_lines) > 0) {
      # Clean up trailing whitespace which could cause posting issues
      while (length(content_lines) > 0 && content_lines[length(content_lines)] == "") {
        content_lines <- content_lines[-length(content_lines)]
      }
    }
    content <- paste(content_lines, collapse = "\n")
  } else {
    content <- ""
  }

  if (nchar(trimws(content)) == 0) {
    stop("Draft comment has no content")
  }

  list(params = params, content = content)
}
