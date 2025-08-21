# md-roam-server

HTTP server that exposes org-roam functionality via REST API.

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

## Testing

Test the endpoints with curl:
```bash
# Hello endpoint
curl http://localhost:8080/hello

# Files endpoint
curl http://localhost:8080/files
```