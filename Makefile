STACK=stack
CWD=$(shell pwd)
BIN=$(CWD)/bin
OPTS=+RTS -N2 -RTS

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the project and generate executable
	$(STACK) build --local-bin-path $(BIN) --copy-bins

test: ## Runs the test suite
	$(STACK) test

runp: ## Runs the program binary directly
	APP_ENVIRONMENT=PRODUCTION $(BIN)/ApiTemplate-exe $(OPTS)

run: ## Runs the program via stack & complies if necessary
	$(STACK) run

clean: ## Cleans the project
	$(STACK) clean

.PHONY: clean test
