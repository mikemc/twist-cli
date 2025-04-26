#' Make API request to Twist
#'
#' @param endpoint API endpoint to call (without the base URL)
#' @param params List of parameters to include in the request
#' @param token Authentication token
#' @param method HTTP method to use (default: "GET")
#'
#' @return httr2 response object
#' @export
twist_request <- function(
  endpoint,
  params = list(),
  token = twist_token(),
  method = "GET") {
  base_url <- "https://api.twist.com/api/v3/"

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_auth_bearer_token(token)

  if (method == "GET") {
    req <- httr2::req_url_query(req, !!!params)
  } else if (method == "POST") {
    req <- httr2::req_method(req, method) |>
      httr2::req_body_form(!!!params)
  }

  httr2::req_perform(req)
}

#' Set or get the Twist API token
#'
#' @param token API token to use (optional)
#'
#' @return The current API token (invisibly if setting)
#' @export
twist_token <- function(token = NULL) {
  if (!is.null(token)) {
    options(twist_token = token)
    return(invisible(token))
  }

  # Try to get from options
  token <- getOption("twist_token")

  # If not in options, try environment variable
  if (is.null(token)) {
    token <- Sys.getenv("TWIST_TOKEN")
    if (token != "") {
      options(twist_token = token)
    } else {
      stop("No Twist API token found. Please set it with twist_token() or the TWIST_TOKEN environment variable.")
    }
  }

  token
}
