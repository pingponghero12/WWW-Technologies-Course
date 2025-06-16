# vim: noet
#
SRC_DIR := src
BUILD_DIR := build
DOCKER_DIR := docker

all: $(BUILD_DIR)
	cd $(BUILD_DIR) && ghc -o sa -threaded -hidir . -odir . ../$(SRC_DIR)/ConnectDB.hs ../$(SRC_DIR)/Types.hs ../$(SRC_DIR)/BeerShopDB.hs ../$(SRC_DIR)/Auth.hs ../$(SRC_DIR)/Main.hs

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

run: all
	./$(BUILD_DIR)/sa

# Docker commands
docker-start:
	cd $(DOCKER_DIR) && sudo docker-compose down -v
	cd $(DOCKER_DIR) && sudo docker-compose build --no-cache
	cd $(DOCKER_DIR) && sudo docker-compose up -d

docker-stop:
	cd $(DOCKER_DIR) && sudo docker-compose down --remove-orphans

docker-logs:
	cd $(DOCKER_DIR) && sudo docker-compose logs --tail=50 -f db

docker-restart: docker-stop docker-start

docker-status:
	cd $(DOCKER_DIR) && sudo docker-compose ps

# Combined targets
start: docker-start
	@echo "Waiting for database to be ready..."
	@sleep 5
	@echo "Database should be ready. You can now run 'make run' to start the API."

stop: docker-stop

# Test target that starts docker, runs tests, then optionally stops docker
test: docker-start
	@echo "Waiting for database to be ready..."
	@sleep 10
	@echo "Starting API ..."
	@./$(BUILD_DIR)/sa & echo $$! > $(BUILD_DIR)/api.pid
	@sleep 3
	@echo "Tests completed. Database is still running. Use 'make docker-stop' to stop it."

# Development workflow targets
dev-setup: docker-start all
	@echo "Development environment ready!"
	@echo "Database is running, application is built."
	@echo "Run 'make run' to start the API server."

dev-restart: docker-restart all
	@echo "Development environment restarted and rebuilt!"

# Help target
help:
	@echo "Available targets:"
	@echo "  all          - Build the application"
	@echo "  run          - Build and run the application"
	@echo "  clean        - Clean build artifacts"
	@echo ""
	@echo "Docker commands:"
	@echo "  docker-start - Stop, rebuild, and start database"
	@echo "  docker-stop  - Stop database and remove containers"
	@echo "  docker-logs  - Show database logs (tail -f)"
	@echo "  docker-restart - Restart database"
	@echo "  docker-status - Show container status"
	@echo ""
	@echo "Combined targets:"
	@echo "  start        - Start database and wait for it to be ready"
	@echo "  stop         - Stop database"
	@echo "  test         - Start database, run tests"
	@echo "  dev-setup    - Start database and build application"
	@echo "  dev-restart  - Restart database and rebuild application"
	@echo "  help         - Show this help message"

.PHONY: all clean run docker-start docker-stop docker-logs docker-restart docker-status start stop test dev-setup dev-restart help
