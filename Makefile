# md-roam-server Docker Makefile

.PHONY: help build run stop logs shell test clean dev prod e2e e2e-watch e2e-coverage e2e-setup e2e-clean

# Default target
help:
	@echo "🐳 md-roam-server Docker Commands"
	@echo ""
	@echo "📦 Build Commands:"
	@echo "  make build     - Build the Docker image"
	@echo "  make rebuild   - Force rebuild without cache"
	@echo ""
	@echo "🚀 Runtime Commands:"
	@echo "  make run       - Run with docker compose (development)"
	@echo "  make dev       - Start development environment"
	@echo "  make prod      - Start production environment with Traefik"
	@echo "  make stop      - Stop all services"
	@echo "  make restart   - Restart services"
	@echo ""
	@echo "📊 Monitoring Commands:"
	@echo "  make logs      - Show container logs"
	@echo "  make status    - Show container status"
	@echo "  make health    - Check container health"
	@echo "  make shell     - Open shell in container"
	@echo ""
	@echo "🧪 Testing Commands:"
	@echo "  make test      - Test API endpoints (basic)"
	@echo "  make test-ui   - Test UI accessibility"
	@echo "  make e2e       - Run E2E test suite"
	@echo "  make e2e-watch - Run E2E tests in watch mode"
	@echo "  make e2e-coverage - Run E2E tests with coverage"
	@echo ""
	@echo "🧹 Cleanup Commands:"
	@echo "  make clean     - Clean up containers and images"
	@echo "  make reset     - Reset all data (⚠️  destructive)"

# Variables
IMAGE_NAME=md-roam-server
CONTAINER_NAME=md-roam-server
COMPOSE_FILE=docker compose.yml

# Build commands
build:
	@echo "🔨 Building md-roam-server Docker image..."
	docker compose build

rebuild:
	@echo "🔨 Force rebuilding md-roam-server Docker image..."
	docker compose build --no-cache

# Runtime commands
run: dev

dev:
	@echo "🚀 Starting md-roam-server in development mode..."
	docker compose up -d md-roam-server
	@echo "✅ Services started!"
	@echo "   📡 API: http://localhost:8080"
	@echo "   🌐 UI:  http://localhost:35901"
	@make status

prod:
	@echo "🚀 Starting md-roam-server in production mode with Traefik..."
	docker compose --profile production up -d
	@echo "✅ Production services started!"
	@echo "   📡 API: http://md-roam-api.localhost"
	@echo "   🌐 UI:  http://md-roam-ui.localhost"
	@echo "   🔧 Traefik Dashboard: http://localhost:8081"

stop:
	@echo "🛑 Stopping md-roam-server services..."
	docker compose down

restart:
	@echo "🔄 Restarting md-roam-server services..."
	@make stop
	@sleep 2
	@make run

# Monitoring commands
logs:
	@echo "📋 Showing md-roam-server logs..."
	docker compose logs -f md-roam-server

status:
	@echo "📊 Container Status:"
	@docker compose ps
	@echo ""
	@echo "🏥 Health Status:"
	@docker ps --format "table {{.Names}}\t{{.Status}}" --filter name=md-roam

health:
	@echo "🏥 Testing API health..."
	@curl -s http://localhost:8080/stats | jq '.status' || echo "❌ API not accessible"
	@echo "🌐 Testing UI accessibility..."
	@curl -s -o /dev/null -w "%{http_code}" http://localhost:35901 | grep -q "200" && echo "✅ UI accessible" || echo "❌ UI not accessible"

shell:
	@echo "🐚 Opening shell in md-roam-server container..."
	docker compose exec md-roam-server bash

# Testing commands
test:
	@echo "🧪 Testing md-roam-server API endpoints..."
	@echo "Testing root endpoint..."
	@curl -s http://localhost:8080/ | jq '.status' || echo "❌ Root endpoint failed"
	@echo "Testing stats endpoint..."
	@curl -s http://localhost:8080/stats | jq '.status' || echo "❌ Stats endpoint failed"
	@echo "Testing config endpoint..."
	@curl -s http://localhost:8080/config | jq '.status' || echo "❌ Config endpoint failed"
	@echo "✅ API tests completed"

test-ui:
	@echo "🧪 Testing org-roam-ui accessibility..."
	@curl -s -o /dev/null -w "UI Status: %{http_code}\n" http://localhost:35901

# Cleanup commands
clean:
	@echo "🧹 Cleaning up Docker resources..."
	docker compose down --rmi local --volumes --remove-orphans
	docker system prune -f
	@echo "✅ Cleanup completed"

reset:
	@echo "⚠️  This will delete ALL data including org-roam files!"
	@read -p "Are you sure? Type 'yes' to continue: " confirm; \
	if [ "$$confirm" = "yes" ]; then \
		echo "🗑️  Resetting all data..."; \
		docker compose down -v --remove-orphans; \
		docker volume rm md-roam-server_org-roam-data md-roam-server_config-data 2>/dev/null || true; \
		echo "✅ Reset completed"; \
	else \
		echo "❌ Reset cancelled"; \
	fi

# Development helpers
watch-logs:
	@echo "👀 Watching logs (Ctrl+C to stop)..."
	docker compose logs -f --tail=100

monitor:
	@echo "📊 Starting monitoring stack..."
	docker compose --profile monitoring up -d
	@echo "✅ Monitoring started!"
	@echo "   📊 Prometheus: http://localhost:9090"

# E2E Testing commands
e2e:
	@echo "🧪 Running E2E test suite..."
	@echo "🚀 Starting server for testing..."
	@make dev
	@sleep 3
	@echo "📋 Running TypeScript E2E tests..."
	@cd tests && npm test
	@echo "✅ E2E tests completed"

e2e-watch:
	@echo "🧪 Running E2E tests in watch mode..."
	@echo "🚀 Starting server for testing..."
	@make dev
	@sleep 3
	@echo "👀 Running tests in watch mode (Ctrl+C to stop)..."
	@cd tests && npm run test:watch

e2e-coverage:
	@echo "🧪 Running E2E tests with coverage report..."
	@echo "🚀 Starting server for testing..."
	@make dev
	@sleep 3
	@echo "📊 Running tests with coverage..."
	@cd tests && npm run test:coverage
	@echo "📄 Coverage report available in tests/coverage/"

e2e-setup:
	@echo "🔧 Setting up E2E test dependencies..."
	@cd tests && npm install
	@echo "✅ E2E test setup completed"

e2e-clean:
	@echo "🧹 Cleaning E2E test environment..."
	@cd tests && rm -rf node_modules coverage .vitest
	@echo "✅ E2E test cleanup completed"

# Quick development cycle
dev-cycle: stop build dev logs