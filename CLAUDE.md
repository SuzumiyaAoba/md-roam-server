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
- **Emacs Server**: org-roam, md-roam, and web-server packages with SQLite
- **Hono API Server**: Bun runtime with TypeScript, Hono framework, and Biome tooling
- **Development Tools**: curl and jq for API testing, git, ripgrep, fd
- **Code Quality**: Biome (formatting/linting), husky (git hooks), lint-staged

## Core Architecture

This is a **unified API architecture** with TypeScript/Hono as the single entry point, proxying to Emacs Lisp backend:

- **Hono API Server (Port 3001)**: Single unified API endpoint for ALL operations (GET/POST/PUT/DELETE)
- **Emacs Server (Port 8080)**: Backend service for org-roam operations (not directly accessed by clients)

### Architecture Components

**Main Components (Modular Architecture):**
- `elisp/md-roam-server.el` - Main server entry point and network process management
- `elisp/md-roam-server-core.el` - Core configuration, utilities, and org-roam initialization
- `elisp/md-roam-server-http.el` - HTTP request parsing and response generation
- `elisp/md-roam-server-routes.el` - URL routing and endpoint dispatch
- `elisp/md-roam-server-nodes.el` - Node operations (CRUD, relationships, content)
- `elisp/md-roam-server-search.el` - Search, statistics, and metadata aggregation
- `elisp/md-roam-server-files.el` - File operations and content management
- `start-server.el` - Simple startup script for daemon mode
- `flake.nix` - Nix development environment

**Docker Components:**
- `Dockerfile` - Multi-stage production build with Nix environment
- `Dockerfile.simple` - Single-stage development build
- `docker-compose.yml` - Container orchestration with volume mounting
- `Makefile` - Development workflow automation

**Hono API Components (Unified API Server):**
- `src/server.ts` - Main unified API server entry point with complete routing and error handling
- `src/schemas/index.ts` - Zod validation schemas and type inference for all API endpoints
- `src/api/nodes.ts` - Complete node CRUD operations with Zod validation
- `src/api/search.ts` - Search and query operations with parameter validation
- `src/api/files.ts` - File operations proxy
- `src/api/stats.ts` - Statistics and configuration proxy
- `src/api/tags.ts` - Tags operations proxy
- `src/utils/emacs-client.ts` - Emacs server communication client
- `src/utils/response.ts` - Standardized API response helpers
- `src/types/index.ts` - TypeScript type re-exports for backward compatibility
- `src/config/index.ts` - Environment configuration with strict typing
- `package.json` - Bun/Node.js dependencies and scripts
- `biome.json` - Code formatting and linting configuration

**Key Design Patterns:**
- **Unified API Gateway**: Single Hono server proxies all requests to appropriate backend services
- **Runtime Validation**: Zod schemas provide type-safe validation for all API requests and responses
- **Type Safety**: Complete TypeScript strict mode compliance with inferred types from Zod schemas
- **Legacy Compatibility**: Supports both `/api/` prefixed and direct endpoint access
- **Pre-commit Quality Gates**: Automatic TypeScript type checking, linting, and formatting
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

**Complete CRUD Operations (Unified Hono API Server):**
- Node creation with file format selection (`POST /api/nodes`) - supports both .md and .org
- Full metadata support for Markdown files (tags, aliases, refs, category)
- Basic metadata support for Org files (category, simplified tags/aliases/refs)
- Node updates with file rewriting (`PUT /api/nodes/:id`)
- Safe node deletion with security validation (`DELETE /api/nodes/:id`)
- Tag and category management endpoints (`POST /api/nodes/:id/tags`, `DELETE /api/nodes/:id/tags/:tag`)
- **All GET operations**: Node retrieval, search, backlinks, links, aliases, refs, parsing
- **Complete API proxy**: All Emacs server functionality accessible through single API endpoint

**org-roam Core Features:**
- Bidirectional link discovery (`/nodes/:id/backlinks`, `/nodes/:id/links`)
- Database-driven link analysis using org-roam links table
- Link type detection and relationship mapping
- Complete graph traversal capabilities

**Critical Debugging Patterns:**
- **Port Conflicts**: Always check for existing processes on 8080 before starting server
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
curl http://localhost:8080/stats  # Emacs server (backend)
curl http://localhost:3001/nodes  # Hono API server (main API)
curl http://localhost:3001/health # Hono API server health check
```

### Hono API Development

**Start Hono API Server:**
```bash
bun dev                # Start API server in development mode with hot reload
bun run dev            # Equivalent command
```

**Code Quality Commands:**
```bash
bun run build         # Build API server for production
bun run start          # Start production build
bun run lint           # Lint TypeScript code
bun run format         # Format code with Biome
bun run check          # Run full check (format + lint)
bun run typecheck      # TypeScript type checking (runs in pre-commit)
```

**Quality Gates:**
```bash
# Pre-commit hook automatically runs:
# 1. bun run typecheck  - TypeScript type checking
# 2. bun lint-staged    - Format and lint staged files
```

**Unified API Testing:**
```bash
# Health check (Hono API server - main interface)
curl http://localhost:3001/health

# All operations through unified Hono API server (port 3001 - main interface)
# Node CRUD operations
curl -X POST http://localhost:3001/api/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Test Node", "content": "Test content"}'

curl -X PUT http://localhost:3001/api/nodes/NODE_ID \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated Title"}'

curl -X DELETE http://localhost:3001/api/nodes/NODE_ID

# Node retrieval (all via unified API)
curl http://localhost:3001/api/nodes
curl http://localhost:3001/api/nodes/NODE_ID
curl http://localhost:3001/api/nodes/NODE_ID/content
curl http://localhost:3001/api/nodes/NODE_ID/backlinks
curl http://localhost:3001/api/nodes/NODE_ID/links

# Search and discovery
curl http://localhost:3001/api/search/query
curl http://localhost:3001/api/tags
curl http://localhost:3001/api/stats
curl http://localhost:3001/api/files

# Full-text search with ripgrep
curl -X POST http://localhost:3001/api/search/fulltext \
  -H "Content-Type: application/json" \
  -d '{"query": "keyword", "caseSensitive": false, "maxResults": 10}'

curl "http://localhost:3001/api/search/fulltext/keyword?case=true&limit=5"

# Legacy endpoint compatibility (Hono API server - port 3001)
curl http://localhost:3001/nodes
curl http://localhost:3001/tags  
curl http://localhost:3001/stats
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
- **Zod Validation Errors**: Structured validation error responses with field-level details
- **Global Error Handler**: Catches all errors and returns consistent JSON responses
- Comprehensive `condition-case` error handling for all file operations (Emacs side)
- Consistent error response format across all endpoints
- Graceful handling of missing nodes, files, and database issues
- Safe directory creation and validation before file operations
- Proper HTTP status codes: 200 (success), 201 (created), 400 (validation error), 404 (not found), 500 (server error)
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

**Test Categories (100% Pass Rate Achievement):**
- **Core Tests**: `nodes.test.ts`, `search.test.ts`, `files.test.ts`, `server.test.ts`, `metadata.test.ts`
- **Bug Investigation**: `id-duplication-bug.test.ts`, `metadata-duplication-bug.test.ts`, `debug-file-detection.test.ts`
- **Syntax Tests**: `org-mode-syntax.test.ts`, `org-mode-syntax-simple.test.ts` - comprehensive org-mode constructs
- **Edge Cases**: `external-modification-bug.test.ts`, `no-content-change-bug.test.ts`
- **Extended Tests**: `japanese-unicode.test.ts`, `error-handling.test.ts`, `performance.test.ts`

**Current Test Status**: ✅ **100% E2E test pass rate** across all test suites including core functionality, edge cases, error scenarios, Japanese/Unicode content, and performance tests.

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
- Health checks monitor API endpoints
- Container startup script handles configuration file creation

## Code Quality and Development Practices

**TypeScript Configuration:**
- Uses `@tsconfig/strictest` for maximum type safety
- Path aliases configured (`@/*` maps to `src/*`)
- Strict mode enabled with `noEmit` for type-only compilation

**Validation and Type Safety:**
- **Zod Integration**: Runtime validation for all API endpoints
- **Type Inference**: TypeScript types automatically derived from Zod schemas
- **Request Validation**: All POST/PUT requests validated with `zValidator` middleware
- **Parameter Validation**: Path parameters (e.g., `:id`, `:tag`) validated with Zod schemas

**Pre-commit Quality Gates:**
- **TypeScript Type Check**: `tsc --noEmit` ensures no type errors
- **Code Formatting**: Biome automatically formats staged files
- **Linting**: Biome catches potential issues and enforces style
- **Husky Integration**: Quality checks run automatically on `git commit`

**Development Workflow:**
1. Make changes to TypeScript files
2. Run `bun dev` for hot-reload development
3. Commit triggers automatic type checking and formatting
4. All validation errors must be fixed before commit succeeds

**Schema-First Development:**
- Define Zod schemas in `src/schemas/index.ts`
- API routes automatically get type safety and validation
- Response types inferred from schemas
- Consistent error handling across all endpoints