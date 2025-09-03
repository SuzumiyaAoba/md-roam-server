# md-roam-server

HTTP REST API server for org-roam and md-roam functionality.

## Features

- Complete CRUD operations for org-roam nodes
- Support for both Markdown (.md) and Org (.org) files
- Bidirectional link discovery
- Advanced search and metadata management
- Docker and native Nix support

## Quick Start

### Docker (Recommended)
```bash
make dev
```

Access:
- API: http://localhost:8080

### Native
```bash
nix develop
./start.sh
```

## Configuration

Default config: `~/.config/md-roam-server/config.yml`

```yaml
server:
  port: 8080

org-roam:
  directory: ~/org-roam
```

## API Documentation

See [API Specification](docs/API_SPECIFICATION.md) for complete documentation.

## Testing

```bash
make test
```

## OpenAPI

The server provides OpenAPI 3.0 specification in `openapi.yml`.