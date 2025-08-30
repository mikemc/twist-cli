# Workspace-level functions

#' Get all channels in a workspace
#'
#' @inheritParams defaults
#' @param archived Whether to include archived channels (default: FALSE)
#'
#' @return List of channels
#' @export
get_workspace_channels <- function(
  workspace_id = twist_workspace_id(),
  token = twist_token(),
  archived = FALSE
) {
  response <- twist_request(
    "v3/channels/get",
    params = list(workspace_id = workspace_id),
    token = token
  )

  channels <- response |> httr2::resp_body_json()

  if (!archived) {
    channels <- channels |>
      purrr::discard(
        purrr::map_lgl(channels, "archived")
      )
  }

  channels
}

#' Create directory structure for workspace
#'
#' Creates a directory for each channel in the workspace and returns the
#' created paths.
#'
#' @inheritParams defaults
#'
#' @return Vector of created directory paths
#' @export
create_workspace_dirs <- function(
  workspace_id = twist_workspace_id(),
  token = twist_token(),
  workspace_dir = twist_workspace_dir()
) {
  if (!fs::dir_exists(workspace_dir)) {
    fs::dir_create(workspace_dir)
  }
  channels <- get_workspace_channels(workspace_id, token)
  channels_dirs <- channels |> purrr::map_chr(channel_dir_name)
  purrr::walk(channels_dirs, fs::dir_create)
  invisible(channels_dirs)
}
