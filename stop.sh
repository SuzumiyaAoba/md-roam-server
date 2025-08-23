#!/bin/bash
# md-roam server stop script

set -e

echo "Stopping md-roam server..."

# Stop server gracefully if possible
if command -v emacsclient > /dev/null 2>&1; then
    echo "Attempting graceful shutdown..."
    nix develop -c emacsclient -e "(md-roam-server-stop)" 2>/dev/null || true
    sleep 2
fi

# Kill emacs daemon
if pgrep -f "emacs.*daemon" > /dev/null; then
    echo "Stopping Emacs daemon..."
    pkill -f "emacs.*daemon" || true
    sleep 1
fi

# Kill any processes on the ports
if lsof -ti:8080 > /dev/null 2>&1; then
    echo "Killing processes on port 8080..."
    lsof -ti:8080 | xargs kill || true
fi

if lsof -ti:35901 > /dev/null 2>&1; then
    echo "Killing processes on port 35901..."
    lsof -ti:35901 | xargs kill || true
fi

# Verify ports are free
sleep 1
if ! lsof -ti:8080 > /dev/null 2>&1 && ! lsof -ti:35901 > /dev/null 2>&1; then
    echo "✅ md-roam server stopped successfully!"
else
    echo "⚠️  Some processes may still be running. Check with: lsof -i:8080,35901"
fi