STACK=stack
CWD=$(shell pwd)
BIN=$(CWD)/bin
DIST=$(CWD)/dist
APP_NAME=chakra-exe
PORT?=3000
HTTPS_PORT?=3443
 OPTS=+RTS -N4 -xn -A32m -RTS # non moving gc
# OPTS=+RTS -N4 -qg -A128m -RTS # no parallel gc

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build the project and generate executable
	$(STACK) build --local-bin-path $(BIN) --copy-bins

build-prof:
	$(STACK) build --local-bin-path $(BIN) --copy-bins --profile

doc: ## Build the project haddoc documentation
	$(STACK) haddock --no-haddock-deps && open .stack-work/dist/x86_64-osx/Cabal-3.0.1.0/doc/html/chakra/index.html

package: ## Build source distribute package to upload to hackage
	$(STACK) sdist --tar-dir "$(DIST)"

test: ## Runs the test suite
	$(STACK) test

run-watch: ## Build & Runs the program watching for file changes
	$(STACK) build --fast --file-watch --exec "$(CWD)/scripts/killRun.sh"

runp: ## Runs the program binary directly
	$(BIN)/$(APP_NAME) $(OPTS) --port $(PORT)

run-prof:
	$(BIN)/$(APP_NAME) --port $(PORT) +RTS -N3 -qg -l -p -A128m -RTS

runhttps: ## Runs server on HTTPS
	$(STACK) exec $(APP_NAME) -- $(OPTS) --port $(HTTPS_PORT) --protocol http+tls  --tlskey certs/localhost.key --tlscert certs/localhost.crt

run: ## Runs the program via stack & complies if necessary
	$(STACK) run

clean: ## Cleans the project
	$(STACK) clean

.PHONY: clean test
