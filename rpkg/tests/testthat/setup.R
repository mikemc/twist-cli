# tests/testthat/setup.R

Sys.setenv(TWIST_TOKEN = Sys.getenv("TWIST_TEST_TOKEN"))
Sys.setenv(TWIST_WORKSPACE_ID = Sys.getenv("TWIST_TEST_WORKSPACE_ID"))

fs::dir_create(fs::path_temp("twistr-test-workspace"))
Sys.setenv(TWIST_WORKSPACE_DIR = fs::path_temp("twistr-test-workspace"))
