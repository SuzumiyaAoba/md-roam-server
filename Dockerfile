# md-roam-server Dockerfile - Multi-stage build for optimized container
FROM nixos/nix:latest AS builder

# Enable experimental features for Nix flakes
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

# Set up build environment
WORKDIR /build
COPY flake.nix flake.lock ./

# Pre-build Nix environment to cache dependencies
RUN nix develop --command echo "Nix environment cached"

# Copy application source
COPY elisp/ ./elisp/
COPY start-server.el openapi.yml ./
COPY docker/ ./docker/

# Build org-roam-ui web assets
RUN nix build .#org-roam-ui-web -o org-roam-ui-web

# Final runtime image
FROM nixos/nix:latest

# Install runtime dependencies
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

# Install essential packages and create user
RUN nix-env -iA nixpkgs.bash nixpkgs.curl nixpkgs.coreutils nixpkgs.shadow && \
    BASH_PATH=$(which bash) && \
    echo "appuser:x:1000:1000::/home/appuser:$BASH_PATH" >> /etc/passwd && \
    echo "appuser:x:1000:" >> /etc/group && \
    mkdir -p /home/appuser && \
    chown 1000:1000 /home/appuser

# Set up directories
RUN mkdir -p /app /data/org-roam && \
    chown -R 1000:1000 /app /data

# Copy application from builder
COPY --from=builder --chown=1000:1000 /build/ /app/

# Set up Nix permissions for appuser
RUN chown -R 1000:1000 /nix/var/ && \
    mkdir -p /nix/var/log/nix/drvs && \
    chown -R 1000:1000 /nix/var/log/nix/ && \
    chown -R 1000:1000 /nix/store/ || echo "Store permissions set"

# Switch to application user
USER 1000:1000
WORKDIR /app

# Create default configuration
RUN mkdir -p /home/appuser/.config/md-roam-server

# Copy Docker-specific configurations
COPY --chown=1000:1000 docker/config.yml /app/docker/
COPY --chown=1000:1000 docker/start-docker.sh /app/docker/

# Make scripts executable
RUN chmod +x /app/docker/start-docker.sh

# Expose API and UI ports
EXPOSE 8080 35901

# Add labels for metadata
LABEL maintainer="md-roam-server" \
      description="REST API server for org-roam with org-roam-ui integration" \
      version="2.0.0" \
      org.opencontainers.image.title="md-roam-server" \
      org.opencontainers.image.description="Emacs Lisp HTTP server for org-roam functionality" \
      org.opencontainers.image.version="2.0.0" \
      org.opencontainers.image.source="https://github.com/your-repo/md-roam-server"

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
    CMD curl -f http://localhost:8080/stats || exit 1

# Set environment variables
ENV EMACS_SERVER_TIMEOUT=60 \
    ORG_ROAM_DATA_DIR=/data/org-roam \
    CONFIG_DIR=/home/appuser/.config/md-roam-server

# Default command  
CMD ["/app/docker/start-docker.sh"]