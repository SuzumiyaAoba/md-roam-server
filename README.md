# md-roam-server

HTTP REST API server for org-roam and md-roam functionality with unified TypeScript API gateway.

## Architecture

This project implements a **unified API architecture** with TypeScript/Hono as the single entry point:

- **Hono API Server (Port 3001)**: TypeScript-based unified API gateway with complete CRUD operations
- **Emacs Server (Port 8080)**: Backend service for org-roam operations (internal use only)

## Features

### Core Functionality
- ✅ Complete CRUD operations for org-roam nodes (Create, Read, Update, Delete)
- ✅ Support for both Markdown (.md) and Org (.org) files
- ✅ Bidirectional link discovery and relationship mapping
- ✅ Advanced search and metadata management (tags, aliases, refs, categories)
- ✅ File content parsing and metadata extraction
- ✅ Real-time validation with Zod schemas

### Architecture & Quality
- ✅ **Feature-Sliced Design (FSD)** architecture
- ✅ TypeScript strict mode with complete type safety
- ✅ Runtime validation using Zod schemas
- ✅ Comprehensive error handling with structured responses
- ✅ Pre-commit hooks (TypeScript type checking, Biome linting/formatting)
- ✅ **100% E2E test pass rate** with Vitest framework

### Development Experience
- ✅ Docker and native Nix development environments
- ✅ Hot reload development server (Bun + TypeScript)
- ✅ Automated testing with comprehensive coverage
- ✅ Code quality gates and Git hooks

## Quick Start

### Docker (Recommended)
```bash
# Start development environment
make dev

# View logs
make logs

# Run tests
make test

# Access shell
make shell
```

**Access Points:**
- **Unified API**: http://localhost:3001
- Health check: http://localhost:3001/health

### Native Development
```bash
# Setup Nix environment
nix develop

# Start Hono API server (development mode with hot reload)
bun dev

# Or start in production mode
bun run build && bun start
```

## API Usage

### Basic Operations

```bash
# Health check
curl http://localhost:3001/health

# Create node
curl -X POST http://localhost:3001/api/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "My Note", "content": "Note content", "tags": ["example"]}'

# Get all nodes
curl http://localhost:3001/api/nodes

# Get specific node
curl http://localhost:3001/api/nodes/NODE_ID

# Update node
curl -X PUT http://localhost:3001/api/nodes/NODE_ID \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated Title", "content": "Updated content"}'

# Delete node
curl -X DELETE http://localhost:3001/api/nodes/NODE_ID

# Search nodes
curl "http://localhost:3001/api/search/query?q=search+term"

# Get tags
curl http://localhost:3001/api/tags

# Get statistics
curl http://localhost:3001/api/stats
```

### Legacy Endpoint Support

The server supports both `/api/` prefixed endpoints and direct endpoints for backward compatibility:

```bash
# Both formats work
curl http://localhost:3001/api/nodes
curl http://localhost:3001/nodes
```

## Configuration

### Default Configuration
Config file: `~/.config/md-roam-server/config.yml`

```yaml
server:
  port: 8080  # Emacs server port
  host: "0.0.0.0"

org-roam:
  directory: ~/org-roam

api:
  port: 3001  # Hono API server port
```

### Environment Variables
```bash
# API server port (default: 8080, recommended: 3001 for development)
export API_PORT=3001

# Org-roam directory
export ORG_ROAM_DIR="/path/to/org-roam"

# Configuration file path
export MD_ROAM_CONFIG_FILE="/path/to/config.yml"
```

## Development

### Project Structure (Feature-Sliced Design)
```
src/
├── app/api/                    # Application layer
├── features/                   # Feature modules
│   ├── node-management/        # Node CRUD operations
│   ├── search/                 # Search functionality
│   ├── tag-management/         # Tag operations
│   └── file-management/        # File operations
├── shared/                     # Shared utilities
│   ├── api/                   # API clients and services
│   ├── lib/                   # Schemas and utilities
│   └── services/              # Business logic services
```

### Code Quality Commands
```bash
# Development with hot reload
bun dev

# Type checking
bun run typecheck

# Linting and formatting
bun run lint
bun run format
bun run check  # format + lint

# Build for production
bun run build
```

### Testing
```bash
# Run all E2E tests
make test

# Run specific test suites
cd tests && npm run test:core
cd tests && npm run test:watch

# Test coverage
cd tests && npm run test:coverage
```

**Current Test Status**: 100% pass rate across all test suites including:
- Core functionality tests
- Edge case handling
- Error scenarios
- Japanese/Unicode content
- Performance tests

## File Format Support

### Markdown Files (.md)
- YAML front matter with full metadata support
- Fields: `id`, `title`, `tags`, `roam_aliases`, `roam_refs`, `category`

```yaml
---
title: "Example Note"
tags:
  - "example"
  - "demo"
roam_aliases:
  - "Demo Note"
roam_refs:
  - "https://example.com"
category: "documentation"
id: "550E8400-E29B-41D4-A716-446655440000"
---

Content goes here...
```

### Org Files (.org)
- Properties drawer + org-mode keywords
- Fields: `:ID:`, `#+title`, `#+category`, `#+filetags`, `#+roam_alias`, `#+roam_refs`

```org
:PROPERTIES:
:ID: 550E8400-E29B-41D4-A716-446655440000
:END:
#+title: Example Note
#+category: documentation
#+filetags: example demo
#+roam_alias: Demo Note
#+roam_refs: https://example.com

Content goes here...
```

## API Documentation

Complete API documentation available at:
- [API Specification](docs/API_SPECIFICATION.md)
- Interactive docs: http://localhost:3001/docs (when server is running)

## Docker Support

### Development
```bash
make dev      # Build and start development server
make logs     # View container logs
make shell    # Access running container
make test     # Run E2E tests in container
```

### Production
```bash
make build    # Build production image
make start    # Start production container
```

### Health Monitoring
```bash
make health   # Check server health
make status   # Check container status
```

## Contributing

1. **Code Quality**: All commits automatically trigger TypeScript type checking and Biome linting
2. **Testing**: Maintain 100% E2E test pass rate
3. **Architecture**: Follow Feature-Sliced Design principles
4. **Documentation**: Update relevant documentation when adding features

## License

[License information]

## Support

For issues and feature requests, please check the [GitHub Issues](https://github.com/SuzumiyaAoba/md-roam-server/issues).