#!/bin/bash
# md-roam server stop script

set -e

echo "🛑 Stopping md-roam server..."

# Stop Emacs daemon
if pgrep -f "emacs.*daemon" > /dev/null; then
    echo "Stopping Emacs daemon..."
    pkill -f "emacs.*daemon" || true
    sleep 2
fi

# Kill processes on port 8080
if lsof -ti:8080 > /dev/null 2>&1; then
    echo "Killing processes on port 8080..."
    lsof -ti:8080 | xargs kill || true
    sleep 1
fi

# Check if all processes are stopped
if ! lsof -ti:8080 > /dev/null 2>&1; then
    echo "✅ All md-roam server processes stopped successfully"
else
    echo "⚠️  Some processes may still be running. Check with: lsof -i:8080"
fi