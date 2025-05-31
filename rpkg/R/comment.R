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
