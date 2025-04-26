.onLoad <- function(libname, pkgname) {
  # Check for environment variables on package load
  token <- Sys.getenv("TWIST_TOKEN")
  if (token != "") {
    options(twist_token = token)
  }

  dir <- Sys.getenv("TWIST_WORKSPACE_DIR")
  if (dir != "") {
    options(twist_workspace_dir = dir)
  }
}
