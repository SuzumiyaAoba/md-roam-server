# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Environment

This project uses Nix flakes for reproducible development environments. Always work within the Nix shell:

```bash
nix develop
```

The development environment includes:
- Emacs with org-roam, md-roam, and web-server packages
- SQLite for org-roam database operations  
- curl and jq for API testing
- Standard development tools (git, ripgrep, fd)

## Core Architecture

This is an HTTP REST API server built in Emacs Lisp that exposes org-roam and md-roam functionality. The architecture consists of:

**Main Components:**
- `md-roam-server.el` - Core server implementation with all API endpoints and request handling
- `start-server.el` - Simple startup script that loads and runs the server
- `flake.nix` - Nix development environment with custom md-roam package build

**Key Design Patterns:**
- Single-file server implementation using Emacs network processes
- Standardized JSON response format with success/error status, timestamps, and consistent field naming
- Path parameter extraction system for RESTful routes (e.g., `/nodes/:id`, `/tags/:tag/nodes`)
- Hash table-based data aggregation for counting and node ID collection
- YAML front matter generation for Markdown file creation

**org-roam Integration:**
- Uses `org-roam-db-query` for direct database access instead of file parsing
- Supports both Org-mode (.org) and Markdown (.md) files via md-roam
- Node creation generates UUID-based IDs and proper YAML front matter
- Database synchronization via `org-roam-update-org-id-locations`
- Search and citations implemented using SQL queries for better performance

**API Response Consolidation:**
- Base endpoints (`/tags`, `/aliases`, `/refs`) include node_ids by default
- No separate `/detailed` endpoints - all responses provide complete information
- Consistent response structure across all endpoints using snake_case field names

**File Processing Features:**
- Citation parsing supporting both `[@citation-key]` and `[[cite:citation-key]]` formats
- File content retrieval by node ID with metadata (`/nodes/:id/content`)
- Advanced file parsing separating metadata from body (`/nodes/:id/parse`)
- Unified format for both Markdown (YAML front matter) and Org mode (properties drawer)
- File type detection returning "md", "org", or "unknown"

## Common Commands

**Start Server:**
```bash
nix develop -c emacs --batch -l start-server.el
```

**Basic Testing:**
```bash
./simple-test.sh
```

**Individual Endpoint Testing:**
```bash
# Basic endpoints
curl http://localhost:8080/files
curl http://localhost:8080/tags
curl http://localhost:8080/citations
curl -X POST http://localhost:8080/sync

# Node operations
curl http://localhost:8080/nodes/NODE_ID
curl http://localhost:8080/nodes/NODE_ID/content
curl http://localhost:8080/nodes/NODE_ID/parse

# Search and discovery  
curl http://localhost:8080/search/query
curl http://localhost:8080/tags/tagname/nodes
```

**Create Node via API:**
```bash
curl -X POST http://localhost:8080/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Test Note", "category": "#test", "tags": ["example"], "content": "Note content"}'
```

**Kill Server Process:**
```bash
pkill -f "emacs.*start-server.el"
```

## Important Implementation Details

**Node ID Handling:**
- Uses `org-id-new` for UUID generation (not `org-roam-node-generate-id`)
- Node IDs are returned in arrays for aggregation endpoints
- Path parameter extraction handles URL-encoded values

**File Operations:**
- Markdown files use YAML front matter with specific field names: `roam_aliases`, `roam_refs`
- File content endpoint includes path traversal protection
- Raw file listing shows physical files vs database nodes
- File parsing endpoint supports both Markdown (YAML) and Org mode (properties) formats
- Database queries used for search and citations instead of file parsing

**Database Integration:**
- Direct `org-roam-db-query` usage for better performance
- Search implemented with SQL LIKE queries (case-insensitive)
- Citations table integration for reference tracking
- Hash table deduplication for search results

**Error Handling:**
- Consistent error response format across all endpoints
- Graceful handling of missing nodes, files, and database issues
- Port conflict resolution (kill existing processes on 8080)

**Testing Requirements:**
- Always test within nix environment to ensure md-roam package availability
- Server startup requires 2-3 second delay for initialization
- Background server processes need explicit cleanup