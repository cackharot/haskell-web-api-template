STACK=stack
CWD=$(shell pwd)

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the project and generate executable
	$(STACK) build

test: ## Runs the test suite
	$(STACK) test

run: ## Runs the program
	$(STACK) run

clean: ## Cleans the project
	$(STACK) clean

.PHONY: clean test
