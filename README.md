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

### GET /files/raw
Returns a list of physical files in the org-roam directory with file metadata.

**Response:**
```json
{
  "status": "success",
  "message": "Found 20 files in org-roam directory",
  "timestamp": "2024-01-01 12:00:00",
  "directory": "/Users/user/org-roam/",
  "files": [
    {
      "name": "20240101120000-example_note.md",
      "path": "/Users/user/org-roam/20240101120000-example_note.md",
      "extension": "md",
      "size": 256,
      "modified": "2024-01-01 12:00:00",
      "created": "2024-01-01 12:00:00"
    }
  ],
  "count": 20
}
```

### GET /files/content/:filepath
Returns the content of a specific file by its relative filepath.

**Parameters:**
- `filepath` (path parameter) - The relative path to the file from org-roam directory

**Response (Success):**
```json
{
  "status": "success",
  "message": "File content retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "path": "20240101120000-example_note.md",
  "full-path": "/Users/user/org-roam/20240101120000-example_note.md",
  "size": 256,
  "modified": "2024-01-01 12:00:00",
  "content": "---\nid: example-id\ntitle: Example Note\n---\n\nThis is the content of the file."
}
```

**Response (404 - File Not Found):**
```json
{
  "status": "error",
  "message": "Error reading file 'nonexistent.md': File not found or access denied",
  "timestamp": "2024-01-01 12:00:00",
  "filepath": "nonexistent.md",
  "directory": "/Users/user/org-roam/"
}
```

### GET /tags
Returns a list of all unique tags used across org-roam nodes with usage counts.

**Response:**
```json
{
  "status": "success",
  "message": "Tags retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "tags": [
    {
      "tag": "research",
      "count": 5
    },
    {
      "tag": "project",
      "count": 3
    },
    {
      "tag": "meeting",
      "count": 2
    }
  ],
  "total-tags": 3,
  "total-usage": 10
}
```

### GET /aliases
Returns a list of all unique aliases used across org-roam nodes with usage counts.

**Response:**
```json
{
  "status": "success",
  "message": "Aliases retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "aliases": [
    {
      "alias": "Research Paper",
      "count": 3
    },
    {
      "alias": "Project Notes",
      "count": 2
    },
    {
      "alias": "Meeting Summary",
      "count": 1
    }
  ],
  "total-aliases": 3,
  "total-usage": 6
}
```

### GET /tags/detailed
Returns a list of all unique tags used across org-roam nodes with usage counts and the node IDs that use each tag.

**Response:**
```json
{
  "status": "success",
  "message": "Tags with node IDs retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "tags": [
    {
      "tag": "research",
      "count": 3,
      "node-ids": ["node-id-1", "node-id-2", "node-id-3"]
    },
    {
      "tag": "project",
      "count": 2,
      "node-ids": ["node-id-4", "node-id-5"]
    }
  ],
  "total-tags": 2
}
```

### GET /aliases/detailed
Returns a list of all unique aliases used across org-roam nodes with usage counts and the node IDs that use each alias.

**Response:**
```json
{
  "status": "success",
  "message": "Aliases with node IDs retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "aliases": [
    {
      "alias": "Research Paper",
      "count": 2,
      "node-ids": ["node-id-1", "node-id-3"]
    },
    {
      "alias": "Project Notes",
      "count": 1,
      "node-ids": ["node-id-4"]
    }
  ],
  "total-aliases": 2
}
```

### GET /refs
Returns a list of all unique refs used across org-roam nodes with usage counts.

**Response:**
```json
{
  "status": "success",
  "message": "Refs retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "refs": [
    {
      "ref": "https://example.com",
      "count": 5
    },
    {
      "ref": "https://github.com/user/repo",
      "count": 3
    },
    {
      "ref": "roam://node-id-123",
      "count": 2
    }
  ],
  "total-refs": 3,
  "total-usage": 10
}
```

### GET /refs/detailed
Returns a list of all unique refs used across org-roam nodes with usage counts and the node IDs that use each ref.

**Response:**
```json
{
  "status": "success",
  "message": "Refs with node IDs retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "refs": [
    {
      "ref": "https://example.com",
      "count": 3,
      "node-ids": ["node-id-1", "node-id-2", "node-id-3"]
    },
    {
      "ref": "roam://node-id-123",
      "count": 2,
      "node-ids": ["node-id-4", "node-id-5"]
    }
  ],
  "total-refs": 2
}
```

### GET /tags/:tag/nodes
Returns a list of nodes that have the specified tag.

**Parameters:**
- `tag` (path parameter) - The tag name to filter by

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 2 nodes with tag 'research'",
  "timestamp": "2024-01-01 12:00:00",
  "tag": "research",
  "nodes": [
    {
      "id": "node-id-1",
      "title": "Research Paper 1",
      "file": "/path/to/file1.md",
      "level": 0,
      "tags": ["research", "academic"],
      "aliases": ["Paper 1"]
    },
    {
      "id": "node-id-2", 
      "title": "Research Notes",
      "file": "/path/to/file2.md",
      "level": 0,
      "tags": ["research", "notes"],
      "aliases": null
    }
  ],
  "count": 2
}
```

**Response (No matches):**
```json
{
  "status": "success",
  "message": "No nodes found with tag 'nonexistent'",
  "timestamp": "2024-01-01 12:00:00",
  "tag": "nonexistent",
  "nodes": [],
  "count": 0
}
```

### GET /nodes/:id
Returns a single org-roam node by its ID.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (200 - Success):**
```json
{
  "status": "success",
  "message": "Node retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Node Title",
  "file": "/path/to/file.org",
  "level": 0,
  "tags": ["tag1", "tag2"],
  "aliases": ["alias1"]
}
```

**Response (404 - Not Found):**
```json
{
  "status": "error",
  "message": "Node with ID 'invalid-id' not found",
  "timestamp": "2024-01-01 12:00:00"
}
```

### GET /nodes/:id/aliases
Returns the aliases for a single org-roam node by its ID.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 2 aliases for node 'Research Paper 1'",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Research Paper 1",
  "aliases": ["Paper 1", "Research Doc"],
  "count": 2
}
```

**Response (No aliases):**
```json
{
  "status": "success",
  "message": "No aliases found for node 'Simple Note'",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Simple Note",
  "aliases": [],
  "count": 0
}
```

**Response (404 - Not Found):**
```json
{
  "status": "error",
  "message": "Node with ID 'invalid-id' not found",
  "timestamp": "2024-01-01 12:00:00"
}
```

### GET /nodes/:id/refs
Returns the refs for a single org-roam node by its ID.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 3 refs for node 'Research Paper 1'",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Research Paper 1",
  "refs": [
    "https://example.com",
    "https://github.com/user/repo",
    "roam://another-node-id"
  ],
  "count": 3
}
```

**Response (No refs):**
```json
{
  "status": "success",
  "message": "No refs found for node 'Simple Note'",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Simple Note",
  "refs": [],
  "count": 0
}
```

**Response (404 - Not Found):**
```json
{
  "status": "error",
  "message": "Node with ID 'invalid-id' not found",
  "timestamp": "2024-01-01 12:00:00"
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

# Raw files listing endpoint
curl http://localhost:8080/files/raw

# File content endpoint
curl http://localhost:8080/files/content/20250823014345-corrected_alias_format.md

# Tags endpoint
curl http://localhost:8080/tags

# Tags detailed endpoint (with node IDs)
curl http://localhost:8080/tags/detailed

# Aliases endpoint
curl http://localhost:8080/aliases

# Aliases detailed endpoint (with node IDs)
curl http://localhost:8080/aliases/detailed

# Refs endpoint
curl http://localhost:8080/refs

# Refs detailed endpoint (with node IDs)
curl http://localhost:8080/refs/detailed

# Get nodes by tag endpoint  
curl http://localhost:8080/tags/research/nodes

# Get single node by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID

# Get node aliases by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/aliases

# Get node refs by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/refs

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