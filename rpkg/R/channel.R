#' Get a single channel by ID
#'
#' @param channel_id ID of the channel
#' @param token Authentication token
#'
#' @return Channel object
#' @export
get_channel <- function(channel_id, token = twist_token()) {
  twist_request(
    "v3/channels/getone",
    params = list(id = channel_id),
    token = token
  ) |> httr2::resp_body_json()
}

#' Create a directory name for a channel
#'
#' @param channel Channel object
#'
#' @return String to use as file name
channel_dir_name <- function(channel) {
  name_clean <- channel$name |>
    janitor::make_clean_names() |>
    stringr::str_replace_all("_", "-")
  stringr::str_glue("{channel$id}_{name_clean}")
}

#' Get threads in a channel
#'
#' @param channel_id Integer. The ID of the channel.
#' @param token Character. Authentication token.
#' @param as_ids Logical. If TRUE, only the IDs of the threads are returned. Default is FALSE.
#' @param filter_by Character. A filter can be one of "attached_to_me" or "everyone". Default is "everyone".
#' @param limit Integer. Limits the number of threads returned. Default is 20, maximum is 500.
#' @param newer_than_ts Integer. Limits threads to those newer than the specified Unix time.
#' @param older_than_ts Integer. Limits threads to those older than the specified Unix time.
#' @param before_id Integer. Limits threads to those with a lower ID than specified.
#' @param after_id Integer. Limits threads to those with a higher ID than specified.
#' @param workspace_id Integer. The ID of the workspace.
#' @param is_pinned Logical. If TRUE, only pinned threads are returned.
#' @param is_starred Logical. If TRUE, only starred threads are returned.
#' @param order_by Character. The order of the threads returned. Either "desc" (default, descending) or "asc" (ascending).
#' @param exclude_thread_ids List of Integers. The thread IDs that should be excluded from the results.
#'
#' @return List of thread objects.
#' @export
get_channel_threads <- function(
  channel_id,
  token = twist_token(),
  as_ids = NULL,
  filter_by = NULL,
  limit = NULL,
  newer_than_ts = NULL,
  older_than_ts = NULL,
  before_id = NULL,
  after_id = NULL,
  workspace_id = NULL,
  is_pinned = NULL,
  is_starred = NULL,
  order_by = NULL,
  exclude_thread_ids = NULL
) {
  # Get a named list of parameters to pass to the 'threads/get' API call
  params <- purrr::compact(as.list(environment())[-2])

  # Make a single request to the API
  response <- twist_request("v3/threads/get", params = params, token = token)

  # Parse and return the response
  httr2::resp_body_json(response)
}

#' Update an existing channel
#'
#' @param id Integer. The ID of the channel to update.
#' @param name Character. The new name of the channel (1-80 Unicode code points).
#' @param token Character. Authentication token.
#' @param color Integer. The color of the channel (optional).
#' @param public Logical. Whether the channel should be public (optional).
#' @param description Character. The description of the channel (optional).
#' @param default_groups List of integers. Groups to notify by default (optional).
#' @param default_recipients List of integers. Users to notify by default (optional).
#' @param is_favorited Logical. Whether the channel is favorited (optional).
#' @param icon Integer. The icon of the channel (optional).
#'
#' @return Updated channel object from the API
#' @export
update_channel <- function(
  id,
  name,
  token = twist_token(),
  color = NULL,
  public = NULL,
  description = NULL,
  default_groups = NULL,
  default_recipients = NULL,
  is_favorited = NULL,
  icon = NULL
) {
  params <- as.list(environment()) |>
    purrr::discard_at("token") |>
    purrr::compact()

  response <- twist_request(
    "v3/channels/update",
    params = params,
    token = token,
    method = "POST"
  )

  httr2::resp_body_json(response)
}
