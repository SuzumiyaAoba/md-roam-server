#!/usr/bin/env bash
# Docker container startup script for md-roam-server

set -e

echo "🐳 Starting md-roam-server in Docker container..."

# Create necessary directories
mkdir -p /data/org-roam
mkdir -p ~/.config/md-roam-server

# Check if custom configuration is mounted, otherwise use default
if [ -f ~/.config/md-roam-server/config.yml ]; then
    echo "📝 Using existing configuration file..."
elif [ -d ~/.config/md-roam-server ]; then
    # Config directory is mounted but no config.yml file
    if [ ! -f ~/.config/md-roam-server/config.yml ]; then
        echo "📝 Creating default configuration in mounted directory..."
        cp /app/docker/config.yml ~/.config/md-roam-server/config.yml
    fi
else
    echo "📝 Creating default container configuration..."
    mkdir -p ~/.config/md-roam-server
    cp /app/docker/config.yml ~/.config/md-roam-server/config.yml
fi

# Set up data directory permissions
chown -R $(id -u):$(id -g) /data/org-roam ~/.config/md-roam-server

# Display configuration
echo "⚙️  Container configuration:"
echo "   - Data directory: /data/org-roam"
echo "   - Config file: ~/.config/md-roam-server/config.yml"
echo "   - API port: 8080"
echo "   - UI port: 35901"

# Function to handle shutdown gracefully
shutdown_handler() {
    echo "🛑 Received shutdown signal, stopping md-roam-server..."
    pkill -f "md-roam-server" || true
    exit 0
}

# Set up signal handlers
trap shutdown_handler SIGTERM SIGINT

# Start the server using Nix environment
echo "🚀 Launching md-roam-server..."

# Use nix-shell to ensure all dependencies are available
nix develop --command bash -c "
    cd /app
    emacs --batch \
        --eval '(progn
            (add-to-list (quote load-path) \"/app\")
            (add-to-list (quote load-path) \"/app/elisp\")
            (require (quote md-roam-server))
            (message \"Container: Starting md-roam-server...\")
            (condition-case err
                (progn
                    (md-roam-server-start)
                    (message \"✅ md-roam-server started successfully in container\")
                    (while t (sleep-for 1)))
                (error
                    (message \"❌ Failed to start server: %s\" (error-message-string err))
                    (kill-emacs 1))))'" &

# Wait for the background process
EMACS_PID=$!

# Health check loop
echo "🏥 Starting health check loop..."
while kill -0 $EMACS_PID 2>/dev/null; do
    sleep 30
    if curl -f -s http://localhost:8080/stats >/dev/null 2>&1; then
        echo "✅ Health check passed at $(date)"
    else
        echo "⚠️  Health check failed at $(date)"
    fi
done

echo "🔚 md-roam-server container stopped"