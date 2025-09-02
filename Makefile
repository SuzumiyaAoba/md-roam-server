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

# Development commands
dev: ## Start development server with hot reload
	@echo "🚀 Starting md-roam-server in development mode..."
	@docker-compose up --build

dev-config: ## Start development server with custom config file
	@echo "🚀 Starting md-roam-server with custom config..."
	@if [ -z "$(CONFIG)" ]; then \
		echo "❌ Error: CONFIG variable is required. Usage: make dev-config CONFIG=/path/to/config.yml"; \
		exit 1; \
	fi
	@MD_ROAM_CONFIG_FILE=$(CONFIG) docker-compose up --build

build: ## Build Docker image
	@echo "🔨 Building md-roam-server Docker image..."
	@docker-compose build

start: ## Start container in detached mode
	@echo "▶️  Starting md-roam-server container..."
	@docker-compose up -d

start-config: ## Start container with custom config file
	@echo "▶️  Starting md-roam-server with custom config..."
	@if [ -z "$(CONFIG)" ]; then \
		echo "❌ Error: CONFIG variable is required. Usage: make start-config CONFIG=/path/to/config.yml"; \
		exit 1; \
	fi
	@MD_ROAM_CONFIG_FILE=$(CONFIG) docker-compose up -d

# Runtime commands
run: dev

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
	@curl -s http://localhost:8080/stats | jq '.status' || echo "❌ Emacs API not accessible"
	@curl -s http://localhost:3001/health | jq '.status' || echo "❌ Hono API not accessible"
	@echo "🌐 Testing UI accessibility..."
	@curl -s -o /dev/null -w "%{http_code}" http://localhost:35901 | grep -q "200" && echo "✅ UI accessible" || echo "❌ UI not accessible"

shell:
	@echo "🐚 Opening shell in md-roam-server container..."
	docker compose exec md-roam-server bash

# Testing commands
test: ## Run E2E tests with test configuration
	@echo "🧪 Running E2E tests with test configuration..."
	@echo "📁 Test files will be created in: ./tmp/org-roam"
	@cd tests && npm test

test-watch: ## Run E2E tests in watch mode
	@echo "🧪 Running E2E tests in watch mode..."
	@cd tests && npm run test:watch

test-coverage: ## Run E2E tests with coverage report
	@echo "🧪 Running E2E tests with coverage..."
	@cd tests && npm run test:coverage

test-clean: ## Clean up test artifacts
	@echo "🧹 Cleaning up test artifacts..."
	@rm -rf tmp/
	@rm -rf tests/tmp/
	@echo "✅ Test artifacts cleaned up"

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
	@echo "📋 Running TypeScript E2E tests (server managed by test suite)..."
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

# API Development Commands
api-dev:
	@echo "🚀 Starting Hono API server in development mode..."
	bun dev

api-build:
	@echo "🏗️ Building Hono API server..."
	bun build

api-start:
	@echo "🚀 Starting Hono API server in production mode..."
	bun start

api-lint:
	@echo "🔍 Linting API code..."
	bun lint

api-format:
	@echo "✨ Formatting API code..."
	bun format

api-check:
	@echo "🔧 Running full API check..."
	bun check

# Quick development cycle
dev-cycle: stop build dev logs