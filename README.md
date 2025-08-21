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

## Testing

Test the endpoint with curl:
```bash
curl http://localhost:8080/hello
```