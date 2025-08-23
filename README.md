# md-roam-server

HTTP REST API server that exposes org-roam and md-roam functionality for building applications on top of your org-roam knowledge base.

## Features

- **Complete CRUD Operations**: Create, read, update, and delete nodes with full metadata support
- **Bidirectional Links**: org-roam's signature backlink and forward link discovery
- **Unified File Support**: Both Org-mode (.org) and Markdown (.md) files via [md-roam](https://github.com/nobiot/md-roam)
- **Database-Driven**: Uses org-roam SQLite database for fast queries and search
- **Advanced Search**: Search by title, alias, tags, references, citations with database optimization
- **Advanced Parsing**: Separates metadata from content for both YAML front matter and Org properties
- **Citation Support**: Extracts and tracks citations in `[@citation-key]` and `[[cite:citation-key]]` formats  
- **Statistics Dashboard**: Comprehensive database statistics and health metrics
- **Snake_case Responses**: Consistent JSON field naming across all endpoints

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

## API Overview

The server provides a comprehensive REST API with the following categories of endpoints:

### File Operations
- `GET /files` - List all org-roam nodes with metadata
- `GET /files/raw` - List physical files in org-roam directory
- `GET /files/content/:filepath` - Get file content by filepath

### Node Operations
- `GET /nodes` - List all nodes with metadata
- `GET /nodes/:id` - Get single node metadata
- `GET /nodes/:id/content` - Get complete file content by node ID
- `GET /nodes/:id/parse` - Parse file and separate metadata from body
- `POST /nodes` - Create new node
- `PUT /nodes/:id` - Update existing node
- `DELETE /nodes/:id` - Delete node and file

### Node Relationships
- `GET /nodes/:id/backlinks` - Get nodes that link to this node
- `GET /nodes/:id/links` - Get nodes this node links to
- `GET /nodes/:id/aliases` - Get node aliases
- `GET /nodes/:id/refs` - Get node references

### Node Tag Management
- `POST /nodes/:id/tags` - Add a tag to a node
- `DELETE /nodes/:id/tags/:tag` - Remove a tag from a node

### Node Category Management
- `POST /nodes/:id/categories` - Add a category to a node
- `DELETE /nodes/:id/categories/:category` - Remove a category from a node

### Search & Discovery
- `GET /search/:query` - Search nodes by title or alias
- `GET /tags/:tag/nodes` - Get nodes with specific tag
- `GET /aliases/:alias/nodes` - Get nodes with specific alias
- `GET /refs/:ref/nodes` - Get nodes with specific reference
- `GET /citations/:citation/nodes` - Get nodes with specific citation

### Metadata Collections
- `GET /tags` - List all tags with usage counts
- `GET /aliases` - List all aliases with usage counts  
- `GET /refs` - List all references with usage counts
- `GET /citations` - List all citations with usage counts
- `GET /stats` - Get comprehensive database statistics

### Database Management
- `POST /sync` - Synchronize org-roam database

## API Endpoints

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
  "full_path": "/Users/user/org-roam/20240101120000-example_note.md",
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
Returns a list of all unique tags used across org-roam nodes with usage counts and the node IDs that use each tag.

**Response:**
```json
{
  "status": "success",
  "message": "Tags retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "tags": [
    {
      "tag": "research",
      "count": 3,
      "node_ids": ["node-id-1", "node-id-2", "node-id-3"]
    },
    {
      "tag": "project",
      "count": 2,
      "node_ids": ["node-id-4", "node-id-5"]
    }
  ],
  "total_tags": 2
}
```

### GET /aliases
Returns a list of all unique aliases used across org-roam nodes with usage counts and the node IDs that use each alias.

**Response:**
```json
{
  "status": "success",
  "message": "Aliases retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "aliases": [
    {
      "alias": "Research Paper",
      "count": 2,
      "node_ids": ["node-id-1", "node-id-3"]
    },
    {
      "alias": "Project Notes",
      "count": 1,
      "node_ids": ["node-id-4"]
    }
  ],
  "total_aliases": 2
}
```

### GET /refs
Returns a list of all unique refs used across org-roam nodes with usage counts and the node IDs that use each ref.

**Response:**
```json
{
  "status": "success",
  "message": "Refs retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "refs": [
    {
      "ref": "https://example.com",
      "count": 3,
      "node_ids": ["node-id-1", "node-id-2", "node-id-3"]
    },
    {
      "ref": "roam://node-id-123",
      "count": 2,
      "node_ids": ["node-id-4", "node-id-5"]
    }
  ],
  "total_refs": 2
}
```

### GET /citations
Returns a list of all unique citations used across org-roam nodes with usage counts and the node IDs that use each citation. Supports both `[@citation-key]` and `[[cite:citation-key]]` formats.

**Response:**
```json
{
  "status": "success",
  "message": "Citations retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "citations": [
    {
      "citation": "smith2023",
      "count": 3,
      "node_ids": ["node-id-1", "node-id-2", "node-id-3"]
    },
    {
      "citation": "jones2022methodology",
      "count": 1,
      "node_ids": ["node-id-4"]
    }
  ],
  "total_citations": 2
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

### GET /search/:query
Search for nodes by title or alias matching the query string (case-insensitive partial match).

**Parameters:**
- `query` (path parameter) - The search query string (URL-encoded)

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 2 nodes matching 'research'",
  "timestamp": "2024-01-01 12:00:00",
  "query": "research",
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
      "title": "My Notes",
      "file": "/path/to/file2.md",
      "level": 0,
      "tags": ["notes"],
      "aliases": ["Research Notes"]
    }
  ],
  "count": 2
}
```

**Response (No matches):**
```json
{
  "status": "success",
  "message": "No nodes found matching 'nonexistent'",
  "timestamp": "2024-01-01 12:00:00",
  "query": "nonexistent",
  "nodes": [],
  "count": 0
}
```

### GET /nodes
Returns a list of all org-roam nodes with metadata, ordered by title.

**Response (Success):**
```json
{
  "status": "success",
  "message": "Retrieved 3 nodes successfully",
  "timestamp": "2024-01-01 12:00:00",
  "nodes": [
    {
      "id": "node-id-1",
      "title": "First Node",
      "file": "relative/path/to/file1.md",
      "level": 0,
      "tags": ["tag1", "tag2"],
      "aliases": ["alias1"]
    },
    {
      "id": "node-id-2",
      "title": "Second Node",
      "file": "relative/path/to/file2.md",
      "level": 0,
      "tags": ["tag3"],
      "aliases": []
    }
  ],
  "count": 2
}
```

**Response (No nodes):**
```json
{
  "status": "success",
  "message": "No nodes found",
  "timestamp": "2024-01-01 12:00:00",
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

### GET /nodes/:id/content
Returns the full file content of a single org-roam node by its ID, including all metadata.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (Success):**
```json
{
  "status": "success",
  "message": "File content retrieved successfully for node: Node Title",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "title": "Node Title",
  "file_path": "relative/path/to/file.md",
  "full_path": "/full/path/to/file.md",
  "level": 0,
  "size": 1024,
  "modified": "2024-01-01 10:30:00",
  "tags": ["tag1", "tag2"],
  "aliases": ["alias1"],
  "content": "# Node Title\n\nThis is the full content of the file..."
}
```

**Response (404 - Not Found):**
```json
{
  "status": "error",
  "message": "Node not found: invalid-id",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "invalid-id"
}
```

### GET /nodes/:id/parse
Parses a node file and returns separated metadata and body content. Supports both Markdown (YAML front matter) and Org mode (properties drawer and #+KEYWORD: format) files with a unified response format.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (Success):**
```json
{
  "status": "success",
  "message": "File parsed successfully for node: Node Title",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "title": "Node Title",
  "file_path": "relative/path/to/file.md",
  "full_path": "/full/path/to/file.md",
  "file_type": "md",
  "level": 0,
  "size": 1024,
  "modified": "2024-01-01 10:30:00",
  "tags": ["tag1", "tag2"],
  "aliases": ["alias1"],
  "metadata": [
    {
      "id": "FC4AE1D6-9650-472E-BFD6-F6AF96B13574"
    },
    {
      "title": "Node Title"
    },
    {
      "category": ["test", "example"]
    },
    {
      "roam_refs": "https://example.com"
    }
  ],
  "body": "# Node Title\n\nThis is the body content after metadata..."
}
```

**Supported File Formats:**

- **Markdown (.md)**: YAML front matter between `---` delimiters
- **Org mode (.org)**: Properties drawer (`:PROPERTIES:` ... `:END:`) and `#+KEYWORD:` format

**Response (404 - Not Found):**
```json
{
  "status": "error",
  "message": "Node not found: invalid-id",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "invalid-id"
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
  "initial_count": 2,
  "final_count": 3,
  "nodes_changed": 1,
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

### PUT /nodes/:id
Updates an existing org-roam node with new content and metadata.

**Parameters:**
- `id` (path parameter) - The unique ID of the node to update

**Request Body:**
```json
{
  "title": "Updated Node Title",
  "category": "#updated #category",
  "tags": ["new-tag1", "new-tag2"],
  "aliases": ["New Alias"],
  "refs": ["https://updated.example.com"],
  "content": "Updated content for the node."
}
```

**Response (Success):**
```json
{
  "status": "success",
  "message": "Node updated successfully: Updated Node Title",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Updated Node Title",
  "file": "relative/path/to/file.md",
  "category": "#updated #category",
  "tags": ["new-tag1", "new-tag2"],
  "aliases": ["New Alias"],
  "refs": ["https://updated.example.com"]
}
```

### DELETE /nodes/:id
Deletes an org-roam node and its associated file.

**Parameters:**
- `id` (path parameter) - The unique ID of the node to delete

**Response (Success):**
```json
{
  "status": "success",
  "message": "Node deleted successfully: Node Title",
  "timestamp": "2024-01-01 12:00:00",
  "id": "node-id",
  "title": "Node Title",
  "file": "relative/path/to/file.md"
}
```

### GET /nodes/:id/backlinks
Returns all nodes that link to the specified node (backlinks).

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 2 backlinks for node",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "target-node-id",
  "backlinks": [
    {
      "id": "source-node-1",
      "title": "Source Node 1",
      "file": "path/to/source1.md",
      "level": 0,
      "link_type": "id"
    },
    {
      "id": "source-node-2", 
      "title": "Source Node 2",
      "file": "path/to/source2.md",
      "level": 0,
      "link_type": "id"
    }
  ],
  "count": 2
}
```

### GET /nodes/:id/links
Returns all nodes that the specified node links to (forward links).

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 1 forward links from node",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "source-node-id",
  "links": [
    {
      "id": "target-node-id",
      "title": "Target Node",
      "file": "path/to/target.md",
      "level": 0,
      "link_type": "id"
    }
  ],
  "count": 1
}
```

### GET /aliases/:alias/nodes
Returns all nodes that have the specified alias.

**Parameters:**
- `alias` (path parameter) - The alias to search for

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 1 nodes with alias 'MyAlias'",
  "timestamp": "2024-01-01 12:00:00",
  "alias": "MyAlias",
  "nodes": [
    {
      "id": "node-id",
      "title": "Node Title",
      "file": "path/to/file.md",
      "level": 0,
      "tags": ["tag1"],
      "aliases": ["MyAlias", "OtherAlias"]
    }
  ],
  "count": 1
}
```

### GET /refs/:ref/nodes  
Returns all nodes that reference the specified URL.

**Parameters:**
- `ref` (path parameter, URL-encoded) - The reference URL to search for

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 2 nodes with ref 'https://example.com'",
  "timestamp": "2024-01-01 12:00:00",
  "ref": "https://example.com",
  "nodes": [
    {
      "id": "node-id-1",
      "title": "Research Paper",
      "file": "research/paper.md",
      "level": 0,
      "tags": ["research"],
      "aliases": []
    }
  ],
  "count": 1
}
```

### GET /citations/:citation/nodes
Returns all nodes that contain the specified citation.

**Parameters:**
- `citation` (path parameter) - The citation key to search for

**Response (Success):**
```json
{
  "status": "success",
  "message": "Found 1 nodes with citation 'smith2023'",
  "timestamp": "2024-01-01 12:00:00", 
  "citation": "smith2023",
  "nodes": [
    {
      "id": "node-id",
      "title": "Literature Review",
      "file": "reviews/literature.md",
      "level": 0,
      "tags": ["literature", "review"],
      "aliases": []
    }
  ],
  "count": 1
}
```

### GET /stats
Returns comprehensive statistics about the org-roam database.

**Response (Success):**
```json
{
  "status": "success",
  "message": "Statistics retrieved successfully",
  "timestamp": "2024-01-01 12:00:00",
  "total_nodes": 150,
  "total_links": 320,
  "total_tags": 45,
  "total_aliases": 28,
  "total_refs": 67,
  "total_citations": 89,
  "file_types": {
    "md": 120,
    "org": 30
  },
  "avg_links_per_node": 2.13
}
```

### POST /nodes/:id/tags
Adds a tag to a specific node by updating the file content.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Request Body:**
```json
{
  "tag": "new-tag"
}
```

**Response (Success):**
```json
{
  "status": "success",
  "message": "Tag 'new-tag' added to node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "title": "Node Title",
  "tag_added": "new-tag",
  "current_tags": ["existing-tag", "new-tag"]
}
```

**Response (Tag Already Exists):**
```json
{
  "status": "error",
  "message": "Tag 'existing-tag' already exists on node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "tag": "existing-tag",
  "existing_tags": ["existing-tag"]
}
```

### DELETE /nodes/:id/tags/:tag
Removes a specific tag from a node by updating the file content.

**Parameters:**
- `id` (path parameter) - The unique ID of the node
- `tag` (path parameter) - The tag to remove

**Response (Success):**
```json
{
  "status": "success", 
  "message": "Tag 'old-tag' removed from node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "title": "Node Title",
  "tag_removed": "old-tag",
  "current_tags": ["remaining-tag"]
}
```

**Response (Tag Not Found):**
```json
{
  "status": "error",
  "message": "Tag 'nonexistent-tag' does not exist on node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "tag": "nonexistent-tag",
  "existing_tags": ["existing-tag"]
}
```

### POST /nodes/:id/categories
Adds a category to a specific node by updating the `category:` field in the file content.

**Parameters:**
- `id` (path parameter) - The unique ID of the node

**Request Body:**
```json
{
  "category": "new-category"
}
```

**Response (Success):**
```json
{
  "status": "success",
  "message": "Category 'new-category' added to node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "title": "Node Title",
  "category_added": "new-category",
  "current_categories": ["existing-category", "new-category"]
}
```

**Response (Category Already Exists):**
```json
{
  "status": "error",
  "message": "Category 'existing-category' already exists on node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "category": "existing-category",
  "existing_categories": ["existing-category"]
}
```

### DELETE /nodes/:id/categories/:category
Removes a specific category from a node by updating the `category:` field in the file content.

**Parameters:**
- `id` (path parameter) - The unique ID of the node
- `category` (path parameter) - The category to remove

**Response (Success):**
```json
{
  "status": "success", 
  "message": "Category 'old-category' removed from node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "title": "Node Title",
  "category_removed": "old-category",
  "current_categories": ["remaining-category"]
}
```

**Response (Category Not Found):**
```json
{
  "status": "error",
  "message": "Category 'nonexistent-category' does not exist on node 'Node Title'",
  "timestamp": "2024-01-01 12:00:00",
  "node_id": "node-id",
  "category": "nonexistent-category",
  "existing_categories": ["existing-category"]
}
```

## Testing

Test the endpoints with curl:
```bash
# Files endpoint
curl http://localhost:8080/files

# Raw files listing endpoint
curl http://localhost:8080/files/raw

# File content endpoint
curl http://localhost:8080/files/content/20250823014345-corrected_alias_format.md

# Tags endpoint (includes node IDs)
curl http://localhost:8080/tags

# Aliases endpoint (includes node IDs)
curl http://localhost:8080/aliases

# Refs endpoint (includes node IDs)
curl http://localhost:8080/refs

# Citations endpoint (includes node IDs)
curl http://localhost:8080/citations

# Get nodes by tag endpoint  
curl http://localhost:8080/tags/research/nodes

# Search nodes by title or alias endpoint
curl http://localhost:8080/search/research
curl "http://localhost:8080/search/Study%20Note"

# Get all nodes endpoint
curl http://localhost:8080/nodes

# Get single node by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID

# Get node file content by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/content

# Parse node file (metadata and body) by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/parse

# Get node aliases by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/aliases

# Get node refs by ID endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/refs

# Sync database endpoint
curl -X POST http://localhost:8080/sync

# Update node endpoint
curl -X PUT http://localhost:8080/nodes/YOUR_NODE_ID \
  -H "Content-Type: application/json" \
  -d '{"title": "Updated Title", "category": "#updated", "tags": ["updated"], "content": "Updated content."}'

# Delete node endpoint
curl -X DELETE http://localhost:8080/nodes/YOUR_NODE_ID

# Get node backlinks endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/backlinks

# Get node forward links endpoint
curl http://localhost:8080/nodes/YOUR_NODE_ID/links

# Get nodes by alias endpoint
curl http://localhost:8080/aliases/MyAlias/nodes

# Get nodes by ref endpoint (URL-encoded)
curl "http://localhost:8080/refs/https%3A%2F%2Fexample.com/nodes"

# Get nodes by citation endpoint
curl http://localhost:8080/citations/smith2023/nodes

# Get database statistics endpoint
curl http://localhost:8080/stats

# Add tag to node endpoint
curl -X POST http://localhost:8080/nodes/YOUR_NODE_ID/tags \
  -H "Content-Type: application/json" \
  -d '{"tag": "new-tag"}'

# Remove tag from node endpoint
curl -X DELETE http://localhost:8080/nodes/YOUR_NODE_ID/tags/old-tag

# Add category to node endpoint
curl -X POST http://localhost:8080/nodes/YOUR_NODE_ID/categories \
  -H "Content-Type: application/json" \
  -d '{"category": "new-category"}'

# Remove category from node endpoint
curl -X DELETE http://localhost:8080/nodes/YOUR_NODE_ID/categories/old-category

# Create new node endpoint
curl -X POST http://localhost:8080/nodes \
  -H "Content-Type: application/json" \
  -d '{"title": "Test Note", "category": "#testing #example", "tags": ["test"], "aliases": ["Testing"], "refs": ["https://example.com"], "content": "This is a test note."}'

```

## OpenAPI Specification

The server provides a complete OpenAPI 3.0 specification in the `openapi.yml` file located in the project root. This specification documents all available endpoints, request/response schemas, and can be used to generate client libraries or import into API testing tools like Postman or Swagger UI.

**File:** `openapi.yml` (static file in project root)

**Contents:** OpenAPI 3.0 YAML specification including:
- Complete API information (title, version, description)
- All available endpoints with detailed descriptions
- Request/response schemas with examples
- Parameter definitions and validation rules
- Error response formats

The specification can be used with tools like:
- **Swagger UI**: For interactive API documentation (`swagger-ui-serve openapi.yml`)
- **Postman**: Import the OpenAPI spec to generate requests
- **Code generators**: Generate client libraries in various languages
- **API testing tools**: Validate API responses against the schema

To view the specification:
```bash
# View the OpenAPI specification file
cat openapi.yml

# Serve with Swagger UI (if installed)
swagger-ui-serve openapi.yml

# Import into Postman or other API tools
```