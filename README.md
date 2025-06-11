# twistr

An R package for creating and syncing local versions of Twist workspaces.

## Overview

This repository contains tools for working with the [Twist](https://twist.com) team communication platform API. The main component is an R package that allows you to:

- Sync Twist workspaces to local file systems
- Create markdown files for threads and comments
- Post comments back to Twist from local files
- Maintain up-to-date local copies of your team's conversations

## Repository Structure

- **`rpkg/`** - The main R package source code
  - Contains functions for API interactions, file management, and syncing
  - See `rpkg/DESCRIPTION` for package details and dependencies

- **`scripts/`** - Command-line utilities
  - Standalone scripts that use the R package for common tasks
  - Can be run directly from the command line once the R package is installed

## Installation

Install the R package from Github,

```r
remotes::install_github("mikemc/twist-cli/rpkg")
```

Or from a local copy of this repository,

```r
devtools::install("rpkg/")
```

## Setup

Before using the package, you'll need to configure your Twist API credentials:

```r
# Set your API token (see below)
twist_token("your-api-token-here")

# Set your workspace ID
twist_workspace_id("your-workspace-id")

# Set a directory for syncing workspace files
twist_workspace_dir("path/to/your/workspace/directory")
```

Alternatively, you can set environment variables:
```bash
export TWIST_TOKEN="your-api-token-here"
export TWIST_WORKSPACE_ID="your-workspace-id"
export TWIST_WORKSPACE_DIR="path/to/your/workspace/directory"
```

### Getting a Twist API Token

1. Log in to your Twist account
2. Visit the [Twist App console](https://twist.com/app_console)
3. Create a new application for personal use
4. Copy the OAuth 2 test token. Note: This token will give the R package full scope access to the currently logged in user.

## Basic Usage

### R Package

```r
library(twistr)

# Sync entire workspace to local files
update_workspace()

# Sync specific channel
update_channel(channel_id = 12345)

# Create a markdown file for a specific thread
write_thread(thread_id = 67890, dir = "my-threads/")

# Post a draft comment from a thread file
post_comment_from_file("path/to/thread-file.md")
```

### Command Line Scripts

Sync your workspace directly from the command line:

```bash
# Basic workspace sync
./scripts/update_workspace.R

# Force update all files regardless of timestamps
./scripts/update_workspace.R TRUE

# Limit to 100 threads per channel with custom timezone
./scripts/update_workspace.R FALSE 100 'America/New_York'

# Only sync threads newer than a specific timestamp
./scripts/update_workspace.R FALSE 500 UTC 1640995200

# Get help
./scripts/update_workspace.R --help
```

Post draft comments directly from the command line:

```bash
# Basic usage - post first draft comment in file
./scripts/post_draft.R path/to/thread.md

# Preview what would be posted (dry run)
./scripts/post_draft.R path/to/thread.md TRUE

# Post specific draft number
./scripts/post_draft.R path/to/thread.md FALSE 2

# Get help
./scripts/post_draft.R
```

## Workflow

A typical workflow might look like:

1. **Initial sync**: `update_workspace()` or `./scripts/update_workspace.R` to download all conversations
2. **Regular updates**: Run the same command periodically to stay current
3. **Local reading and comment drafting**: Read thread markdown files, add draft comments
4. **Post responses**: Use `post_comment_from_file()` or `./scripts/post_draft.R`

## Draft Comments

You can add draft comments to thread files by appending sections like this:

```markdown
# DRAFT COMMENT

Your comment content here. Supports **markdown** formatting.
```

(The "DRAFT COMMENT" key phrase is case insensitive.)

With optional parameters:

````markdown
# DRAFT COMMENT

```yaml
recipients: [12345, 67890]
thread_action: close
```

Your comment content here.
````


## License

MIT License - see `rpkg/LICENSE.md` for details.



