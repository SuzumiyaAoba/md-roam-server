# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Environment

This project supports both Docker and native Nix development environments:

### Docker Environment (Recommended)
```bash
# Start development server
make dev

# View logs
make logs

# Access running container
make shell
```

### Native Nix Environment
```bash
nix develop
```

The development environment includes:
- Emacs with org-roam, md-roam, and web-server packages
- SQLite for org-roam database operations  
- curl and jq for API testing
- Standard development tools (git, ripgrep, fd)
- org-roam-ui web assets for graph visualization

## Core Architecture

This is an HTTP REST API server built in Emacs Lisp that exposes org-roam and md-roam functionality. The architecture consists of:

**Main Components (Modular Architecture):**
- `elisp/md-roam-server.el` - Main server entry point and network process management
- `elisp/md-roam-server-core.el` - Core configuration, utilities, and org-roam initialization
- `elisp/md-roam-server-http.el` - HTTP request parsing and response generation
- `elisp/md-roam-server-routes.el` - URL routing and endpoint dispatch
- `elisp/md-roam-server-nodes.el` - Node operations (CRUD, relationships, content)
- `elisp/md-roam-server-search.el` - Search, statistics, and metadata aggregation
- `elisp/md-roam-server-files.el` - File operations and content management
- `elisp/md-roam-server-ui.el` - org-roam-ui integration and graph interface
- `start-server.el` - Simple startup script for daemon mode
- `flake.nix` - Nix development environment with custom org-roam-ui asset management

**Docker Components:**
- `Dockerfile` - Multi-stage production build with Nix environment
- `Dockerfile.simple` - Single-stage development build
- `docker-compose.yml` - Container orchestration with volume mounting
- `Makefile` - Development workflow automation

**Key Design Patterns:**
- Modular architecture with focused, single-responsibility modules
- Emacs network processes with 0.0.0.0 binding for Docker compatibility
- Standardized JSON response format with success/error status, timestamps, and consistent field naming
- Path parameter extraction system for RESTful routes (e.g., `/nodes/:id`, `/tags/:tag/nodes`)
- Hash table-based data aggregation for counting and node ID collection
- YAML configuration file system with automatic default generation
- md-roam integration with proper YAML front matter support

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

**Complete CRUD Operations:**
- Node creation with file format selection (`POST /nodes`) - supports both .md and .org
- Full metadata support for Markdown files (tags, aliases, refs, category)
- Basic metadata support for Org files (category, simplified tags/aliases/refs)
- Node updates with file rewriting (`PUT /nodes/:id`)
- Safe node deletion with security validation (`DELETE /nodes/:id`)
- Complete metadata and content management

**org-roam Core Features:**
- Bidirectional link discovery (`/nodes/:id/backlinks`, `/nodes/:id/links`)
- Database-driven link analysis using org-roam links table
- Link type detection and relationship mapping
- Complete graph traversal capabilities

**Critical Debugging Patterns:**
- **Port Conflicts**: Always check for existing processes on 8080/35901 before starting server
- **Database Sync**: Use `POST /sync` endpoint or `org-roam-db-sync` to refresh database after external file changes
- **ID Duplication**: Server has built-in detection and cleanup for node ID duplication issues
- **File Encoding**: All file operations use UTF-8 with proper Japanese/Unicode support
- **Race Conditions**: Server uses single-fork testing and atomic file operations to prevent concurrency issues

**Elisp Development Patterns:**
- Each module follows single-responsibility principle with clear separation of concerns
- Network processes use 0.0.0.0 binding for Docker compatibility (not localhost)
- All HTTP responses use standardized JSON format with status, message, timestamp
- Error handling uses comprehensive `condition-case` blocks for robust operation
- Database queries use direct `org-roam-db-query` for performance instead of file parsing

## Common Commands

### Docker Development (Recommended)

**Start Server:**
```bash
make dev              # Build and start development server
make build           # Build Docker image only
make start           # Start existing container
make stop            # Stop and remove container
make logs            # View container logs
make shell           # Open shell in running container
```

**Basic Testing:**
```bash
curl http://localhost:8080/stats
curl http://localhost:8080/nodes
```

### Native Nix Development

**Start Server:**
```bash
nix develop -c emacs --batch -l start-server.el
# Or using the startup script:
./start.sh
```

**Stop Server:**
```bash
./stop.sh
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
curl http://localhost:8080/nodes/NODE_ID/backlinks
curl http://localhost:8080/nodes/NODE_ID/links

# Search and discovery  
curl http://localhost:8080/search/query
curl http://localhost:8080/tags/tagname/nodes
curl http://localhost:8080/aliases/aliasname/nodes
curl http://localhost:8080/refs/https%3A%2F%2Fexample.com/nodes
curl http://localhost:8080/citations/citation-key/nodes

# Statistics and overview
curl http://localhost:8080/stats
```

**Create Node via API:**
```bash
# Create Markdown file (default, full metadata support)
curl -X POST http://localhost:8080/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Test Note", "content": "Note content", "tags": ["example"], "aliases": ["Test"], "refs": ["https://example.com"], "category": "test", "file_type": "md"}'

# Create Org file (basic metadata support)
curl -X POST http://localhost:8080/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Org Note", "content": "Org content", "tags": ["org"], "category": "test", "file_type": "org"}'

# Simple node creation (defaults to Markdown)
curl -X POST http://localhost:8080/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Simple Note", "content": "Basic content"}'
```

**Update Node via API:**
```bash
curl -X PUT http://localhost:8080/nodes/NODE_ID \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated Title", "content": "Updated content"}'
```

**Delete Node via API:**
```bash
curl -X DELETE http://localhost:8080/nodes/NODE_ID
```

**Kill Server Process:**
```bash
pkill -f "emacs.*start-server.el"
# Or kill any process using port 8080
lsof -ti:8080 | xargs kill -9 || true
```

**Development Monitoring:**
```bash
# Check server health status
make health
curl -s http://localhost:8080/stats | jq '.status' || echo "❌ API not accessible"

# Monitor logs
make logs                  # View container logs
make watch-logs           # Watch logs in real-time
docker compose logs -f --tail=100  # Direct Docker command

# Container management
make status               # Check container status
make shell               # Open shell in running container
make clean              # Clean up Docker resources
make reset              # Reset all data (destructive)
```

## Important Implementation Details

**Configuration System:**
- YAML configuration file at `~/.config/md-roam-server/config.yml` (default)
- Custom configuration file path can be specified via:
  - Environment variable: `MD_ROAM_CONFIG_FILE=/path/to/config.yml`
  - Command line: `./start.sh --config /path/to/config.yml`
  - Docker: `./docker/start.sh --config /path/to/config.yml`
  - Makefile: `make dev-config CONFIG=/path/to/config.yml`
- Automatic default configuration generation on first startup
- Hash-table based configuration parsing with `gethash` support
- Configuration validation and directory path expansion

**md-roam Integration:**
- Proper md-roam mode initialization with file extension setup
- YAML front matter parsing for Markdown file recognition
- Consistent database location setup across all endpoints
- Required front matter: `title`, `id`, optional: `tags`, `aliases`, `roam_refs`

**Network Configuration:**
- Host binding set to "0.0.0.0" for Docker container networking
- Dual server setup: REST API (8080) and org-roam-ui (35901)
- Network process management with proper cleanup

**Node ID Handling:**
- Uses safe UUID v4 generation with fallback implementation
- Custom UUID format: `XXXXXXXX-XXXX-4XXX-XXXX-XXXXXXXXXXXX` with proper version bits
- Node IDs are returned in arrays for aggregation endpoints
- Path parameter extraction handles URL-encoded values

**File Operations:**
- **Markdown files (.md)**: Full YAML front matter support with `id`, `title`, `tags`, `roam_aliases`, `roam_refs`, `category`
- **Org files (.org)**: Properties drawer (`:ID:`) + org keywords (`#+title`, `#+category`, `#+filetags`, `#+roam_alias`, `#+roam_refs`)
- File content endpoint includes path traversal protection
- Safe file creation using `write-region` instead of `with-temp-file`
- Raw file listing shows physical files vs database nodes
- File parsing endpoint supports both Markdown (YAML) and Org mode (properties) formats
- Database queries used for search and citations instead of file parsing

**Database Integration:**
- Direct `org-roam-db-query` usage for better performance
- Search implemented with SQL LIKE queries (case-insensitive)
- Citations table integration for reference tracking
- Links table integration for bidirectional relationship discovery
- Hash table deduplication for search results
- Statistical aggregation using SQL COUNT and DISTINCT functions
- Complete database schema utilization (nodes, links, tags, aliases, refs, citations)

**Error Handling:**
- Comprehensive `condition-case` error handling for all file operations
- Consistent error response format across all endpoints
- Graceful handling of missing nodes, files, and database issues
- Safe directory creation and validation before file operations
- Proper HTTP status codes: 200 (success), 201 (created), 400 (client error), 500 (server error)
- Port conflict resolution (kill existing processes on 8080)

**E2E Testing Framework:**
- Tests use Vitest framework with comprehensive TypeScript configuration
- Located in `tests/` directory with modular structure and global setup
- Global setup handles server lifecycle management (start/stop)
- Single fork execution prevents server conflicts during testing
- 90-second timeouts for server initialization and complex operations
- Defensive programming patterns for API response validation
- **Test Environment**: Uses dedicated test configuration (`tests/config/test-config.yml`)
- **Test Data Directory**: `./tmp/org-roam` (isolated from user data, gitignored)
- **Test Database**: `./tmp/org-roam/org-roam.db` (separate from user database)

**Running E2E Tests:**
```bash
# Complete test suite via Makefile (starts server automatically)
make test                    # Full E2E test run
make test-watch             # Watch mode for development
make test-coverage          # Run with coverage report
make test-clean             # Clean up test artifacts

# Direct npm commands (requires manual server start)
cd tests && npm test              # Run all tests
cd tests && npm run test:core     # Core functionality tests  
cd tests && npm run test:watch    # Watch mode
cd tests && npm run test:coverage # Coverage report
cd tests && npm run test:quick    # Fast run (bail on first failure)
```

**Test Categories:**
- **Core Tests**: `nodes.test.ts`, `search.test.ts`, `files.test.ts`, `server.test.ts`, `metadata.test.ts`
- **Bug Investigation**: `id-duplication-bug.test.ts`, `metadata-duplication-bug.test.ts`, `debug-file-detection.test.ts`
- **Syntax Tests**: `org-mode-syntax.test.ts`, `org-mode-syntax-simple.test.ts` - comprehensive org-mode constructs
- **Edge Cases**: `external-modification-bug.test.ts`, `no-content-change-bug.test.ts`
- **Extended Tests**: `japanese-unicode.test.ts`, `error-handling.test.ts`, `performance.test.ts`

**Test Utilities:**
- `ApiHelpers` class: Standardized API interaction with supertest and response validation
- `TestCleanup` class: Automatic cleanup with retry logic and batch processing  
- Global setup/teardown manages server lifecycle across test suites
- Server auto-cleanup between test suites with background process detection

**Testing Requirements:**
- Docker environment preferred for consistent testing (use `make dev`)
- Server startup requires 2-3 second delay for initialization
- Background server processes need explicit cleanup
- For native testing: always test within nix environment to ensure md-roam package availability

**Docker Specific Notes:**
- Container uses root user to avoid permission complexities
- Volume mounting for org-roam directory: `/data/org-roam`
- Volume mounting for config: `/root/.config/md-roam-server`
- Health checks monitor both API and UI endpoints
- Container startup script handles configuration file creation