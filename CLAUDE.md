# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

See README.md for project overview, installation, and usage instructions.

## Development Commands

### Package Development

```bash
# Install package from local directory
R -e "devtools::install('rpkg/')"

# Run tests
R -e "testthat::test_dir('rpkg/tests/testthat')"

# Build package
R -e "devtools::build('rpkg/')"

# Check package
R -e "devtools::check('rpkg/')"
```

## Architecture Overview

### Core Data Flow

1. **API Layer** (`rpkg/R/api.R`): Handles authentication and HTTP requests to Twist API
2. **Workspace/Channel Management** (`rpkg/R/workspace.R`, `rpkg/R/channel.R`): Retrieves workspace structure and thread lists
3. **Thread Operations** (`rpkg/R/thread.R`): Converts threads to markdown format with YAML frontmatter
4. **Comment Operations** (`rpkg/R/comment.R`): Parses draft comments from files and posts them via API
5. **Sync Operations** (`rpkg/R/sync.R`): Orchestrates updates using timestamp comparisons

### Key Architectural Patterns

**Thread File Format**: All thread files use YAML frontmatter with thread metadata, followed by markdown content. Headers in thread content are automatically shifted down one level to accommodate the structure.

**Draft Comment Parsing**: The package looks for `# DRAFT COMMENT` sections in thread files and can parse optional YAML configuration for recipients and thread actions.

**Timestamp-Based Updates**: Files are only updated when `last_updated_ts` in Twist is newer than the local file's metadata, enabling efficient incremental syncing.

**Configuration Management**: Settings can be provided via R options, environment variables, or function parameters, with a clear precedence order.

## Code Style

### Comments

When writing code comments, explain WHY, not WHAT. Only comment on:

- Complex business logic or non-obvious algorithms
- Important assumptions, edge cases, or constraints
- Design decisions that aren't immediately clear
- Context that helps future maintainers

Avoid restating what the code obviously does. Use clear naming instead.

## Important Files

- `README.md` - User documentation, installation, and basic usage
- `rpkg/DESCRIPTION` - Package metadata and dependencies
- `rpkg/R/api.R` - Authentication and HTTP client functions
- `rpkg/R/sync.R` - High-level sync operations (`sync_workspace`, `sync_channel`)
- `rpkg/tests/testthat/` - Test suite
- `scripts/` - Command-line utilities for end users
- `dev/` - Development workspace with test data
- `dev/twist-api-docs/` - Symlink to external Twist API documentation (for reference when working with API endpoints)