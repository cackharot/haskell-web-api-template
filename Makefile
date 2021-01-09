STACK=stack
CWD=$(shell pwd)
BIN=$(CWD)/bin
APP_NAME=ApiTemplate-exe
PORT?=3000
HTTPS_PORT?=3443
OPTS=+RTS -N2 -RTS

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the project and generate executable
	$(STACK) build --local-bin-path $(BIN) --copy-bins

test: ## Runs the test suite
	$(STACK) test

run-watch: ## Build & Runs the program watching for file changes
	$(STACK) build --fast --file-watch --exec "$(CWD)/scripts/killRun.sh"

runp: ## Runs the program binary directly
	$(BIN)/$(APP_NAME) $(OPTS) --port $(PORT)

runhttps: ## Runs server on HTTPS
	$(BIN)/$(APP_NAME) $(OPTS) --port $(HTTPS_PORT) --protocol http+tls  --tlskey certs/localhost.key --tlscert certs/localhost.crt

run: ## Runs the program via stack & complies if necessary
	$(STACK) run

clean: ## Cleans the project
	$(STACK) clean

.PHONY: clean test
