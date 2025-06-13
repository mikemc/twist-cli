# tests/testthat/test-comments.R

# Helper functions (defined first)

skip_if_no_api_access <- function() {
  token <- try(twist_token(), silent = TRUE)
  workspace_id <- try(twist_workspace_id(), silent = TRUE)
  test_thread_id <- Sys.getenv("TWIST_TEST_THREAD_ID")

  if (inherits(token, "try-error") || inherits(workspace_id, "try-error") || test_thread_id == "") {
    skip("API credentials or test thread ID not available")
  }
}

#' Create a thread file for testing
#'
#' Either creates a proper thread file corresponding to a real thread, or creates a malformed thread file (missing a thread id) that doesn't correspond to a real thread.
#'
#' @param real_thread Boolean; whether to use the real test thread
create_test_thread_file <- function(real_thread = TRUE) {
  if (real_thread) {
    test_thread_id <- get_test_thread_id()
    test_file <- write_thread(test_thread_id)
  } else {
    test_file <- fs::file_temp(ext = ".md")
    header_content <- list(
      title = "Test Thread",
      created = "2024-01-01 10:00:00",
      timezone = "UTC",
      last_updated_ts = 1704110400
    )

    file_content <- c(
      "---",
      yaml::as.yaml(header_content),
      "---",
      "",
      "This is test thread content without thread_id.",
      ""
    )

    readr::write_lines(file_content, test_file)
  }

  return(test_file)
}

get_test_thread_id <- function() {
  test_thread_id <- Sys.getenv("TWIST_TEST_THREAD_ID")

  if (test_thread_id == "") {
    stop("TWIST_TEST_THREAD_ID environment variable not set. Please set it to a valid thread ID in your test workspace.")
  }

  as.numeric(test_thread_id)
}

add_draft_comment <- function(file_path, content, params = NULL) {
  draft_lines <- c("", "# DRAFT COMMENT")

  if (!is.null(params)) {
    draft_lines <- c(
      draft_lines,
      "",
      "```yaml",
      yaml::as.yaml(params),
      "```"
    )
  }

  draft_lines <- c(draft_lines, "", content)
  readr::write_lines(draft_lines, file_path, append = TRUE)
}

add_edit_comment <- function(file_path, id, content, params = NULL) {
  edit_params <- list(id = id)
  if (!is.null(params)) {
    edit_params <- c(edit_params, params)
  }

  edit_lines <- c(
    "",
    "# EDIT COMMENT",
    "",
    "```yaml",
    yaml::as.yaml(edit_params),
    "```",
    "",
    content
  )

  readr::write_lines(edit_lines, file_path, append = TRUE)
}

# Unit tests (no API calls needed)
test_that("find_comment_section works correctly", {
  lines <- c(
    "---",
    "title: Test Thread",
    "thread_id: 12345",
    "---",
    "",
    "Thread content here.",
    "",
    "# Comment by User (123) at 2024-01-01 (Comment 100)",
    "",
    "Original comment content.",
    "",
    "# DRAFT COMMENT",
    "",
    "```yaml",
    "recipients: [456]",
    "```",
    "",
    "First draft comment content.",
    "",
    "# EDIT COMMENT",
    "",
    "```yaml",
    "id: 100",
    "```",
    "",
    "Edited comment content."
  )

  # Test finding draft comments. The draft comment is assumed to run to end of
  # file, so should include the edited comment section.
  draft_section <- find_comment_section(lines, "^# DRAFT COMMENT")
  expect_equal(draft_section$start_line, 12)
  expect_equal(draft_section$end_line, 26)

  # Test finding edit comments
  edit_section <- find_comment_section(lines, "^# EDIT COMMENT")
  expect_equal(edit_section$start_line, 20)
  expect_equal(edit_section$end_line, 26)

  # Test case insensitive matching
  lines_mixed_case <- c("# draft comment", "content")
  section <- find_comment_section(lines_mixed_case, "^# DRAFT COMMENT")
  expect_equal(section$start_line, 1)
})

test_that("parse_comment_section works correctly", {
  # Test with YAML parameters
  lines <- c(
    "# DRAFT COMMENT",
    "```yaml",
    "recipients: [123, 456]",
    "thread_action: close",
    "```",
    "",
    "Comment content here.",
    "More content."
  )

  section <- list(start_line = 1, end_line = 8)
  result <- parse_comment_section(lines, section)

  expect_equal(result$params$recipients, c(123, 456))
  expect_equal(result$params$thread_action, "close")
  expect_equal(result$content, "Comment content here.\nMore content.")

  # Test without YAML parameters
  lines_no_yaml <- c(
    "# DRAFT COMMENT",
    "",
    "Simple comment content."
  )

  section_no_yaml <- list(start_line = 1, end_line = 3)
  result_no_yaml <- parse_comment_section(lines_no_yaml, section_no_yaml)

  expect_equal(result_no_yaml$params, list())
  expect_equal(result_no_yaml$content, "Simple comment content.")

  # Test parameter validation
  expect_warning(
    parse_comment_section(lines, section, valid_params = c("recipients")),
    "Unknown parameters ignored: thread_action"
  )

  # Test required parameters
  expect_error(
    parse_comment_section(lines_no_yaml, section_no_yaml, required_params = "comment_id"),
    "Required parameters missing: comment_id"
  )
})

test_that("parse_comment_section handles edge cases", {
  # Test empty content
  lines_empty <- c("# DRAFT COMMENT", "")
  section_empty <- list(start_line = 1, end_line = 2)
  expect_error(
    parse_comment_section(lines_empty, section_empty),
    "Comment section has no content"
  )

  # Test content with leading/trailing whitespace
  lines_whitespace <- c(
    "# DRAFT COMMENT",
    "",
    "",
    "Content with spacing.",
    "",
    ""
  )
  section_whitespace <- list(start_line = 1, end_line = 6)
  result <- parse_comment_section(lines_whitespace, section_whitespace)
  expect_equal(result$content, "Content with spacing.")
})

# Integration tests (require API access)
test_that("comment posting and editing integration", {
  skip_if_no_api_access()

  # Create a test thread file
  test_file <- create_test_thread_file()

  # Test 1: Post a draft comment
  add_draft_comment(test_file, "This is a test draft comment.")

  result <- post_comment_from_file(test_file, dry_run = TRUE)
  expect_equal(result$content, "This is a test draft comment.")

  # Actually post the comment
  comment_result <- post_comment_from_file(test_file, update = TRUE)
  expect_true(!is.null(comment_result$id))
  comment_id <- comment_result$id

  # Test 2: Edit the comment
  add_edit_comment(test_file, comment_id, "This is the edited comment content.")

  edit_result <- update_comment_from_file(test_file, dry_run = TRUE)
  expect_equal(edit_result$params$id, comment_id)
  expect_equal(edit_result$content, "This is the edited comment content.")

  # Actually update the comment
  update_result <- update_comment_from_file(test_file, update = TRUE)
  expect_equal(update_result$id, comment_id)

  # Verify the thread file was updated and edit section removed
  lines <- readr::read_lines(test_file)
  edit_sections <- find_comment_section(lines, "^# EDIT COMMENT")
  expect_length(edit_sections, 0)

  # Clean up
  fs::file_delete(test_file)
})

test_that("YAML parameters are handled correctly", {
  skip_if_no_api_access()

  test_file <- create_test_thread_file()

  # Add draft with YAML parameters
  draft_content <- c(
    "# DRAFT COMMENT",
    "",
    "```yaml",
    "recipients: EVERYONE_IN_THREAD",
    "mark_thread_position: false",
    "```",
    "",
    "Comment with parameters."
  )

  readr::write_lines(draft_content, test_file, append = TRUE)

  result <- post_comment_from_file(test_file, dry_run = TRUE)
  expect_equal(result$params$recipients, "EVERYONE_IN_THREAD")
  expect_equal(result$params$mark_thread_position, FALSE)

  fs::file_delete(test_file)
})

test_that("error handling works correctly", {
  skip_if_no_api_access()

  # Test file not found
  expect_error(
    post_comment_from_file("nonexistent.md"),
    "Thread file not found"
  )

  # Test file without thread_id
  test_file <- create_test_thread_file(real_thread = FALSE)
  expect_error(
    post_comment_from_file(test_file),
    "No thread_id found in file header"
  )

  # Test file without drafts
  test_file <- create_test_thread_file()
  expect_error(
    post_comment_from_file(test_file),
    "No draft comment found in file"
  )

  # Test edit without comment id
  edit_content <- c(
    "# EDIT COMMENT",
    "",
    "Content without comment id."
  )
  readr::write_lines(edit_content, test_file, append = TRUE)

  expect_error(
    update_comment_from_file(test_file),
    "Required parameters missing: id"
  )

  # Clean up
  fs::file_delete(test_file)
})
