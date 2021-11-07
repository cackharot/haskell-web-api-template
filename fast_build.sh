#!/usr/bin/env bash -e
ghcid --command "stack ghci chakra:exe:chakra-exe --ghci-options=-fobject-code" -v --warnings
