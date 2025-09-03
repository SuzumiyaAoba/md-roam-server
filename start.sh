#!/bin/bash
# md-roam server startup script with daemon mode

set -e

# Default configuration file path
CONFIG_FILE=""

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --config PATH    Specify custom configuration file path"
            echo "  --help, -h       Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0                                    # Use default config"
            echo "  $0 --config /path/to/config.yml      # Use custom config"
            echo "  MD_ROAM_CONFIG_FILE=/path/to/config.yml $0  # Use env var"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Set environment variable if config file is specified
if [[ -n "$CONFIG_FILE" ]]; then
    export MD_ROAM_CONFIG_FILE="$CONFIG_FILE"
    echo "Using configuration file: $CONFIG_FILE"
fi

# Check if emacs daemon is already running
if pgrep -f "emacs.*daemon" > /dev/null; then
    echo "Emacs daemon is already running. Stopping existing daemon..."
    pkill -f "emacs.*daemon" || true
    sleep 2
fi

# Check if ports are in use
if lsof -ti:8080 > /dev/null 2>&1; then
    echo "Port 8080 is already in use. Killing processes..."
    lsof -ti:8080 | xargs kill || true
    sleep 1
fi

echo "Starting md-roam server in daemon mode..."

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Enter nix environment and start daemon with modular elisp
nix develop -c bash -c "cd '$SCRIPT_DIR' && emacs --daemon --eval \"(progn (add-to-list 'load-path \\\"$SCRIPT_DIR\\\") (add-to-list 'load-path \\\"$SCRIPT_DIR/elisp\\\") (require 'md-roam-server) (md-roam-server-start) (message \\\"md-roam server started in daemon mode\\\"))\""

# Wait for daemon to start
sleep 3

# Check if server is running
if curl -s http://localhost:8080/stats > /dev/null; then
    echo "✅ md-roam server started successfully!"
    echo "📡 REST API: http://localhost:8080"
    echo ""
    echo "To stop the server:"
    echo "  ./stop.sh"
    echo ""
    echo "To check status:"
    echo "  curl http://localhost:8080/stats"
else
    echo "❌ Failed to start server"
    exit 1
fi