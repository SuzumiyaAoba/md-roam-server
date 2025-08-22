# md-roam-server

HTTP server that exposes org-roam and md-roam functionality via REST API.

This server supports both Org-mode (.org) and Markdown (.md) files using [md-roam](https://github.com/nobiot/md-roam), which extends org-roam to work with Markdown files.

## Setup

1. Enter the Nix development environment:
```bash
nix develop
```

2. Start the server:
```bash
emacs --batch -l start-server.el
```

Or interactively:
```bash
emacs -l md-roam-server.el
# Then run: M-x md-roam-server-start
```

## API Endpoints

### GET /hello
Returns a simple hello world message.

**Response:**
```json
{
  "message": "Hello, World!",
  "service": "md-roam-server", 
  "status": "running"
}
```

### GET /files
Returns a list of all org-roam files with metadata.

**Response:**
```json
{
  "files": [
    {
      "id": "node-id",
      "title": "Node Title",
      "file": "/path/to/file.org",
      "level": 0,
      "tags": ["tag1", "tag2"],
      "aliases": ["alias1"]
    }
  ],
  "count": 1
}
```

### POST /sync
Synchronizes the org-roam database by scanning for new or modified files.

**Response:**
```json
{
  "status": "success",
  "message": "Database sync completed",
  "initial-count": 2,
  "final-count": 3,
  "nodes-changed": 1,
  "directory": "/Users/user/org-roam",
  "timestamp": "2024-01-01 12:00:00"
}
```

### POST /nodes
Creates a new md-roam node (Markdown file) with the specified title, category, tags, content, aliases, and references.

**Request Body:**
```json
{
  "title": "My New Note",
  "category": "#research #important",
  "tags": ["tag1", "tag2"],
  "aliases": ["Alternative Name", "Short Name"],
  "refs": ["https://example.com", "https://github.com/user/repo"],
  "content": "Initial content for the note."
}
```

**Response:**
```json
{
  "status": "success",
  "message": "Node created successfully",
  "id": "unique-node-id",
  "title": "My New Note",
  "file": "/path/to/new/file.md",
  "category": "#research #important",
  "tags": ["tag1", "tag2"],
  "aliases": ["Alternative Name", "Short Name"],
  "refs": ["https://example.com", "https://github.com/user/repo"],
  "timestamp": "2024-01-01 12:00:00"
}
```

**Generated Markdown File Format:**
```markdown
---
id: unique-node-id
title: My New Note
category: #research #important
roam_aliases: ["Alternative Name", "Short Name"]
roam_refs: https://example.com https://github.com/user/repo
---

#tag1 #tag2

Initial content for the note.
```

## Testing

Test the endpoints with curl:
```bash
# Hello endpoint
curl http://localhost:8080/hello

# Files endpoint
curl http://localhost:8080/files

# Sync database endpoint
curl -X POST http://localhost:8080/sync

# Create new node endpoint
curl -X POST http://localhost:8080/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Test Note", "category": "#testing #example", "tags": ["test"], "aliases": ["Testing"], "refs": ["https://example.com"], "content": "This is a test note."}'

# OpenAPI specification
curl http://localhost:8080/openapi.json
```

## OpenAPI Specification

The server provides an OpenAPI 3.0 specification at `/openapi.json` endpoint. This specification documents all available endpoints and can be used to generate client libraries or import into API testing tools like Postman or Swagger UI.

**Endpoint:** `GET /openapi.json`

**Response:** OpenAPI 3.0 JSON specification including:
- API information (title, version)
- Available endpoints with descriptions
- Request/response schemas
- Parameter definitions

The specification can be used with tools like:
- **Swagger UI**: For interactive API documentation
- **Postman**: Import the OpenAPI spec to generate requests
- **Code generators**: Generate client libraries in various languages
```